#' Regression modelling for BeSD outcomes
#'
#' @param x A `besd_data` object.
#' @param outcome Single BeSD `item_id`.
#' @param predictors Character vector (by_country) or list(common, context) (multilevel).
#' @param scope "by_country" or "multilevel".
#' @param engine "frequentist" or "bayes".
#' @param ref Reference rule: "mode" or "first".
#' @param random_slopes If TRUE (multilevel), fit random slopes for common predictors.
#' @param ref_levels Optional named list of explicit reference labels (multilevel 
#' common predictors only).
#' @param min_n_context Rare level threshold for context predictors (default 10).
#' @param ... Passed to engine (glm/clm/brm/etc).
#' #' @note **Survey weights are not currently supported.** If `x` was created with
#'   a `weight_col`, that column is stored in the object but ignored by `besd_regress()`. 
#'   All models are fitted on unweighted data. This is a known limitation; weighted 
#'   regression support is planned for a future release.
#' @export
besd_regress <- function(x, 
                         outcome, 
                         predictors, 
                         scope = c("by_country", "multilevel"),
                         engine = c("frequentist", "bayes"), 
                         ref = c("mode", "first"),
                         random_slopes = FALSE, 
                         correlated_re = FALSE,
                         ref_levels = list(),
                         min_n_context = 10L, ...) {
  
  .assert_besd(x)
  
  # Validate outcome early to avoid confusing downstream errors
  if (!is.character(outcome) || length(outcome) != 1L || is.na(outcome) || 
      !nzchar(outcome)) {
    .stopf("`outcome` must be a single non-empty BeSD `item_id` string.")
  }
  
  scope  <- match.arg(scope)
  engine <- match.arg(engine)
  ref    <- match.arg(ref)
  
  info <- besd_info(x)
  df <- dplyr::as_tibble(x)
  dict <- info$besd_dict
  country_col <- info$country_col
  
  # Warn if the object carries a weight column - it is not used in regression.
  if (!is.null(info$weight_col) && nzchar(info$weight_col)) {
    warning(
      sprintf(
        paste0(
          "Survey weights (`%s`) are stored in this `besd_data` object but are not ",
          "currently supported by `besd_regress()`. Models fitted on unweighted data"
        ),
        info$weight_col
      ),
      call. = FALSE
    )
  }
  
  # Prepare outcome (expand multichoice if needed)
  y_type <- .item_type(dict, outcome)
  outcomes <- outcome
  if (y_type == "multichoice") {
    levs <- dict$levels[[match(outcome, dict$item_id)]]
    ex <- .expand_multichoice_outcome(df, outcome, levs, sep = .BESD_SEP)
    df <- ex$df
    outcomes <- ex$outcomes
    y_type <- "binary"
  }
  
  # Route to scope-specific handler
  if (scope == "by_country") {
    predictors <- as.character(predictors)
    .assert_has_cols(df, c(country_col, predictors), "data")
    .fit_by_country(df, outcomes, y_type, predictors, country_col, dict, engine, ref,
                    info$dem_dict, outcome, ...)
  } else {
    preds <- if (is.list(predictors)) {
      predictors
    } else {
      list(common = predictors, context = character())
    } 
    .assert_has_cols(df, c(country_col, preds$common, preds$context), "data")
    .fit_multilevel(df, outcomes, y_type,
                    preds_common  = preds$common,
                    preds_context = preds$context %||% character(),
                    country_col, dict, engine, ref,
                    random_slopes = random_slopes,
                    correlated_re = correlated_re,
                    ref_levels = ref_levels,
                    min_n_context = as.integer(min_n_context),
                    dem_dict = info$dem_dict, outcome = outcome, ...)
  }
}


# Model fitting
.fit_model <- function(dat, formula, y_type, engine, multilevel,
                       do_complete_cases = TRUE, ...) {
  
  vars <- all.vars(formula)
  missing <- setdiff(vars, names(dat))
  if (length(missing)) .stopf("Formula references missing: %s", .pastec(missing))
  
  dd <- dat[, unique(vars), drop = FALSE]
  
  if (do_complete_cases) {
    dd <- dd[stats::complete.cases(dd), , drop = FALSE]
    dd <- droplevels(dd)
    if (nrow(dd) == 0L) .stopf("No complete cases.")
  }
  
  if (engine == "frequentist") {
    if (y_type == "binary") {
      if (multilevel) {
        .require_pkg("lme4", "for multilevel logistic regression")
        return(lme4::glmer(formula, data = dd, family = stats::binomial(), ...))
      }
      return(stats::glm(formula, data = dd, family = stats::binomial(), ...))
    }
    if (y_type == "ordinal") {
      .require_pkg("ordinal", "for ordinal regression")
      if (multilevel) return(ordinal::clmm(formula, data = dd, link = "logit", ...))
      return(ordinal::clm(formula, data = dd, link = "logit", ...))
    }
    .stopf("Unsupported y_type: %s", y_type)
  }
  
  # Bayesian
  .require_pkg("brms", "for Bayesian regression")
  if (y_type == "binary") {
    pri <- c(brms::set_prior("normal(0, 1)", class = "b"),
             brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept"))
    return(brms::brm(formula, data = dd, family = brms::bernoulli(link = "logit"),
                     prior = pri, ...))
  }
  if (y_type == "ordinal") {
    pri <- c(brms::set_prior("normal(0, 1)", class = "b"),
             brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept"))
    return(brms::brm(formula, data = dd, family = brms::cumulative(link = "logit"),
                     prior = pri, ...))
  }
  .stopf("Unsupported y_type: %s", y_type)
}

.fit_by_country <- function(df, outcomes, y_type, predictors, country_col, dict, engine, 
                            ref, dem_dict, outcome, ...) {
  
  # warned refs
  warned_refs <- new.env(parent = emptyenv())
  
  # Preprocess predictors once globally (encode factors + compute within-country refs)
  prep <- .prep_predictors(df, predictors, group_col = country_col, min_n = 5L, 
                           ref_rule = ref)
  df <- prep$df
  countries <- sort(unique(as.character(df[[country_col]])))
  fits <- setNames(vector("list", length(countries)), countries)
  
  for (cc in countries) {
    dcc <- df[df[[country_col]] == cc, , drop = FALSE]
    fits_cc <- list()
    
    # Important:
    # If a predictor is entirely NA in this country, do NOT include it in complete.cases().
    # This prevents "all rows incomplete" -> country/outcome being skipped.
    preds_cc <- predictors[vapply(
      predictors,
      function(p) any(!is.na(dcc[[p]])),
      logical(1)
    )]
    
    for (yy in outcomes) {
      
      # Work only on rows where the outcome exists
      dd_y <- dcc[!is.na(dcc[[yy]]), c(yy, preds_cc), drop = FALSE]
      if (nrow(dd_y) == 0L) next
      
      # Pass 1: choose candidate predictors that vary among non-missing values
      keep1 <- preds_cc[vapply(preds_cc, function(p) {
        v <- dd_y[[p]]
        v <- v[!is.na(v)]
        if (!length(v)) return(FALSE)
        if (is.factor(v)) nlevels(droplevels(v)) >= 2L else length(unique(v)) >= 2L
      }, logical(1))]
      if (!length(keep1)) next
      
      # Build the dataset actually used for fitting (complete cases on yy + keep1 only)
      dd <- dd_y[, c(yy, keep1), drop = FALSE]
      dd <- dd[stats::complete.cases(dd), , drop = FALSE]
      dd <- droplevels(dd)
      if (nrow(dd) == 0L) next
      
      # Apply within-country refs on the final-fitting dataset
      dd <- .apply_within_refs(dd, keep1, cc, prep$ref_code_by_group, warned = warned_refs)
      
      # Outcome variance check on the same dd we will fit
      y <- dd[[yy]]
      if ((is.factor(y) && nlevels(y) < 2L) || (!is.factor(y) && length(unique(y)) < 2L)) {
        next
      }
      
      # Pass 2: after complete-cases, some predictors can collapse -> drop them
      keep2 <- keep1[vapply(keep1, function(p) {
        v <- dd[[p]]
        if (is.factor(v)) nlevels(v) >= 2L else length(unique(v)) >= 2L
      }, logical(1))]
      if (!length(keep2)) next
      
      # IMPORTANT: if keep changed, rebuild dd to regain rows (since fewer predictors)
      if (length(keep2) < length(keep1)) {
        dd <- dd_y[, c(yy, keep2), drop = FALSE]
        dd <- dd[stats::complete.cases(dd), , drop = FALSE]
        dd <- droplevels(dd)
        if (nrow(dd) == 0L) next
        dd <- .apply_within_refs(dd, keep2, cc, prep$ref_code_by_group, warned = warned_refs)
        
        y <- dd[[yy]]
        if ((is.factor(y) && nlevels(y) < 2L) || (!is.factor(y) && length(unique(y)) < 2L)) {
          next
        }
      }
      
      f <- stats::as.formula(paste0(yy, " ~ ", paste(keep2, collapse = " + ")))
      
      # Fit WITHOUT re-filtering
      fits_cc[[yy]] <- .fit_model(dd, f, y_type, engine, FALSE, 
                                  do_complete_cases = FALSE, ...)
      fits[[cc]] <- fits_cc
    }
  }
  
  structure(list(
    fits = fits,
    meta = list(
      scope = "by_country", engine = engine, outcome = outcome, outcomes = outcomes,
      y_type = y_type, predictors = predictors, country_col = country_col,
      countries = countries, ref_rule = ref,
      level_map = prep$level_map, ref_code_by_group = prep$ref_code_by_group,
      dem_dict = dem_dict
    ),
    dict = dict
  ), class = "besd_fit_by_country")
}

.fit_multilevel <- function(df, outcomes, y_type, preds_common, preds_context, 
                            country_col, dict, engine, ref, random_slopes, correlated_re, 
                            ref_levels, min_n_context, dem_dict, outcome, ...) {
  
  if (!is.finite(min_n_context) || min_n_context < 0L) {
    .stopf("`min_n_context` must be a non-negative integer.")
  }
  
  # Common predictors: global refs (when factors) by construction
  prep_common <- .prep_predictors(df, preds_common, group_col = NULL, min_n = 0L, 
                                  ref_rule = ref)
  df <- prep_common$df
  
  # Apply explicit ref overrides (common predictors only)
  for (nm in intersect(names(ref_levels), preds_common)) {
    v <- df[[nm]]
    if (!is.factor(v) || is.ordered(v)) next
    want_label <- ref_levels[[nm]]
    if (!is.character(want_label) || length(want_label) != 1L) next
    mp <- prep_common$level_map[[nm]]
    if (is.null(mp)) next
    idx <- match(want_label, mp$label)
    if (!is.na(idx)) {
      want_code <- mp$code[[idx]]
      if (want_code %in% levels(v)) df[[nm]] <- stats::relevel(v, ref = want_code)
    }
  }
  
  # Context predictors: within-country refs (and rare dropping) by construction
  if (length(preds_context)) {
    prep_ctx <- .prep_predictors(df, preds_context, group_col = country_col,
                                 min_n = min_n_context, ref_rule = ref)
    df <- prep_ctx$df
  } else {
    prep_ctx <- list(level_map = list(), ref_code_by_group = list())
  }
  
  # Encode countries
  countries <- sort(unique(as.character(df[[country_col]])))
  country_code_of <- setNames(sprintf("C%02d", seq_along(countries)), countries)
  
  # Expand context to dummies
  added_ctx <- character(0)
  ctx_term_map <- list()
  if (length(preds_context)) {
    for (var in preds_context) {
      ex <- .add_context_dummies(
        df, var, country_col, country_code_of,
        prep_ctx$ref_code_by_group[[var]],
        prep_ctx$level_map[[var]]
      )
      df <- ex$df
      added_ctx <- c(added_ctx, ex$added)
      ctx_term_map <- c(ctx_term_map, ex$term_map)
    }
  }
  
  # Build term map
  term_map <- .build_term_map(preds_common, prep_common$level_map, added_ctx, ctx_term_map)
  
  # Fit models
  predictors_fixed <- c(preds_common, added_ctx)
  fits <- list()
  
  # Decide whether to estimate random-effect correlations:
  # - Only relevant for Bayesian brms fits with random slopes.
  # - Setting correlated_re=FALSE removes the cor_* parameters and is often faster.
  use_uncor <- isTRUE(engine == "bayes" && isTRUE(random_slopes) && !isTRUE(correlated_re))
  
  for (yy in outcomes) {
    fixed_part <- paste(predictors_fixed, collapse = " + ")
    
    random_part <- if (random_slopes && length(preds_common)) {
      # `||` in brms = independent (uncorrelated) random intercept/slopes
      bar <- if (use_uncor) "||" else "|"
      paste0("(1 + ", paste(preds_common, collapse = " + "), " ", bar, " ", country_col, ")")
    } else {
      paste0("(1 | ", country_col, ")")
    }
    
    f <- stats::as.formula(paste0(yy, " ~ ", fixed_part, " + ", random_part))
    
    vars <- unique(c(yy, all.vars(f)))
    dd <- df[, vars, drop = FALSE]
    dd <- dd[stats::complete.cases(dd), , drop = FALSE]
    if (nrow(dd) == 0L) next
    
    fits[[yy]] <- .fit_model(dd, f, y_type, engine, TRUE, do_complete_cases = FALSE, ...)  
  }
  
  structure(list(
    fits = fits,
    meta = list(
      scope = "multilevel", 
      engine = engine, 
      outcome = outcome, 
      outcomes = outcomes,
      y_type = y_type, 
      predictors_common = preds_common, 
      predictors_context = preds_context,
      country_col = country_col, 
      ref_rule = ref, 
      random_slopes = random_slopes,
      correlated_re = correlated_re,
      # Baseline codes (references) used after factor encoding/releveling
      ref_code_common = prep_common$ref_code,
      ref_code_by_group_context = prep_ctx$ref_code_by_group,
      level_map = prep_common$level_map, 
      level_map_context = prep_ctx$level_map,
      term_map = term_map, 
      country_code_of = country_code_of, 
      dem_dict = dem_dict,
      # Helpful flag for downstream reporting/QA
      uncorrelated_random_effects = use_uncor
    ),
    dict = dict
  ), class = "besd_fit")
}

# Helper functions
.expand_multichoice_outcome <- function(df, item, levels_std, sep = .BESD_SEP) {
  x <- as.character(df[[item]])
  x[x == ""] <- NA_character_
  
  # safe option codes
  opt_codes <- .make_safe_names(levels_std, sep = "_")
  
  # IMPORTANT: no "__" in outcome column names
  out_names <- paste0(item, "_mc_", opt_codes)
  
  mat <- lapply(levels_std, function(lv) {
    as.integer(!is.na(x) & vapply(strsplit(x, sep, fixed = TRUE), 
                                  function(tok) lv %in% tok, logical(1)))
  })
  names(mat) <- out_names
  
  df2 <- cbind(df, as.data.frame(mat, stringsAsFactors = FALSE))
  
  list(df = df2, outcomes = out_names)
}

.make_safe_names <- function(x, sep = "_") {
  if (!is.character(x)) x <- as.character(x)
  x <- gsub("[^\\p{L}\\p{N}]+", "_", x, perl = TRUE)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- ifelse(grepl("^[0-9]", x), paste0("x", sep, x), x)
  x[x == ""] <- "x"
  make.unique(x, sep = sep)
}

.prep_predictors <- function(df, predictors, group_col = NULL, min_n = 0L, 
                             ref_rule = "mode") {
  .assert_has_cols(df, predictors, "df")
  if (!is.null(group_col)) .assert_has_cols(df, group_col, "df")
  
  grp <- if (!is.null(group_col)) as.character(df[[group_col]]) else NULL
  groups <- if (!is.null(grp)) unique(grp) else character(0)
  
  level_map <- list()
  ref_code_by_group <- list()
  ref_code <- list()  # (ungrouped) reference code actually used after relevel()
  
  for (nm in predictors) {
    v <- if (is.character(df[[nm]])) {
      factor(df[[nm]])
    } else if (is.logical(df[[nm]])) {
      as.integer(df[[nm]]) 
    } else df[[nm]]
    
    # Drop rare within groups (context-style)
    if (!is.null(grp) && is.factor(v) && min_n > 0L) {
      vv <- as.character(v)
      for (g in unique(grp)) {
        ii <- which(grp == g)
        tab <- table(vv[ii], useNA = "no")
        rare <- names(tab)[tab < min_n]
        if (length(rare)) vv[ii][vv[ii] %in% rare] <- NA_character_
      }
      v <- droplevels(factor(vv, levels = levels(v)))
    }
    
    # Encode unordered factors
    if (is.factor(v) && !is.ordered(v)) {
      levs <- levels(v)
      codes <- sprintf("__%02d", seq_along(levs))
      v <- factor(codes[match(v, levs)], levels = codes)
      level_map[[nm]] <- tibble::tibble(code = codes, label = levs)
      code_of <- setNames(codes, levs)
      label_of <- setNames(levs, codes)
      
      # Global ref (only when not grouped): used for multilevel common predictors
      if (is.null(grp)) {
        labels_now <- label_of[as.character(v)]
        ref_label <- .pick_ref(labels_now, ref_rule, levels = levs)
        ref0 <- code_of[[ref_label]]
        if (!is.null(ref0) && !is.na(ref0) && ref0 %in% levels(v)) {
          v <- stats::relevel(v, ref = ref0)
        }
        ref_code[[nm]] <- if (!is.null(ref0) && !is.na(ref0)) {
          as.character(ref0) 
        } else {
          NA_character_
        }
      }
      
      # Within-group refs (when grouped): used for by-country and multilevel context 
      # predictors
      if (!is.null(grp)) {
        lab <- label_of[as.character(v)]
        refs <- setNames(rep(NA_character_, length(groups)), groups)
        
        for (g in groups) {
          ii <- which(grp == g)
          ref_label <- .pick_ref(lab[ii], ref_rule, levels = levs)
          
          # ---- (robustness) ----
          # If a country has all NA for this predictor, .pick_ref() returns NA.
          # We keep the group's reference as NA (and downstream code will skip / handle it).
          if (is.na(ref_label)) {
            refs[g] <- NA_character_
          } else {
            ref0 <- unname(code_of[ref_label])
            refs[g] <- if (!is.na(ref0)) ref0 else NA_character_
          }
        }
        ref_code_by_group[[nm]] <- refs
      }
    }
    
    # Non-factor predictors (or grouped refs only): no global baseline code
    if (is.null(grp) && is.null(ref_code[[nm]])) ref_code[[nm]] <- NA_character_
    
    df[[nm]] <- v
  }
  
  list(df = df, level_map = level_map, ref_code_by_group = ref_code_by_group, 
       ref_code = ref_code)
}

.pick_ref <- function(x, rule = "mode", levels = NULL) {
  rule <- match.arg(rule, c("mode", "first"))
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  
  if (rule == "first") {
    # Deterministic: prefer the first declared factor level (not the first observation 
    # in the data).
    if (!is.null(levels) && length(levels)) {
      levs <- as.character(levels)
      levs <- levs[!is.na(levs)]
      hit <- levs[levs %in% x]
      if (length(hit)) return(hit[[1]])
      return(levs[[1]])
    }
    if (is.factor(x) && length(levels(x))) return(levels(x)[[1]])
    return(levels(factor(x))[[1]])
  }
  
  tab <- table(x)
  names(tab)[which.max(tab)][[1]]
}

.apply_within_refs <- function(df, predictors, group_value, ref_code_by_group, 
                               warned = NULL) {
  for (nm in predictors) {
    if (!is.factor(df[[nm]]) || is.ordered(df[[nm]])) next
    refs <- ref_code_by_group[[nm]]
    if (is.null(refs)) next
    
    r0 <- refs[[group_value]]
    if (is.null(r0) || is.na(r0)) next
    
    levs <- levels(df[[nm]])
    if (length(levs) == 0L) next
    
    if (r0 %in% levs) {
      df[[nm]] <- stats::relevel(df[[nm]], ref = r0)
    } else if (!is.null(warned)) {
      key <- paste0(group_value, "::", nm)
      if (!exists(key, envir = warned, inherits = FALSE)) {
        assign(key, TRUE, envir = warned)
        warning(
          sprintf(
            paste0(
              "Within-country reference '%s' for predictor '%s' in '%s' is not ",
              "present after filtering; leaving default reference '%s'."
            ),
            r0, nm, group_value, levs[[1]]
          ),
          call. = FALSE
        )
      }
    }
  }
  df
}

.add_context_dummies <- function(df, var, country_col, country_code_of, ref_code_by_group,
                                 level_map) {
  v <- df[[var]]
  if (!is.factor(v) || is.ordered(v)) {
    .stopf("Context predictor `%s` must be an unordered factor.", var)
  }
  
  cc <- as.character(df[[country_col]])
  v_chr <- as.character(v)  # codes like "__01" or NA
  
  added <- character(0)
  term_map <- list()
  
  label_of <- if (!is.null(level_map) && all(c("code", "label") %in% names(level_map))) {
    stats::setNames(level_map$label, level_map$code)
  } else NULL
  
  for (country in names(country_code_of)) {
    ccode <- country_code_of[[country]]
    base <- ref_code_by_group[[country]] %||% NA_character_
    ii <- which(cc == country)
    
    lev_here <- unique(v_chr[ii])
    lev_here <- lev_here[!is.na(lev_here)]
    
    # If all context values are NA in this country, add NO dummies for this country.
    # The country stays in the model (via common predictors + random intercept),
    # but contributes no information for this context predictor.
    if (!length(lev_here)) next
    
    # If baseline is NA (e.g. no valid ref), choose a baseline from observed levels
    if (is.na(base)) base <- lev_here[[1]]
    
    for (lv in setdiff(lev_here, base)) {
      col <- paste0("ctx_", var, "_", ccode, "_", sub("^__", "", lv))
      
      # ---- (critical) ----
      # Ensure dummy columns are NEVER NA:
      # previously: (cc == country & as.character(v) == lv) yields NA when v is NA.
      # now: explicit !is.na(v_chr) guard => missing context implies dummy=0.
      df[[col]] <- as.integer(cc == country & !is.na(v_chr) & v_chr == lv)
      
      added <- c(added, col)
      term_map[[col]] <- list(
        var = var, country = country, level_code = lv,
        level_label = if (!is.null(label_of)) unname(label_of[[lv]]) else NA_character_,
        baseline_code = base,
        baseline_label = if (!is.null(label_of) && !is.na(base)) {
          unname(label_of[[base]])
        } else {
          NA_character_
        }
      )
    }
  }
  
  list(df = df, added = added, term_map = term_map)
}

.build_term_map <- function(preds_common, level_map_common, added_ctx, ctx_term_map) {
  out <- list()
  for (v in preds_common) {
    mp <- level_map_common[[v]]
    if (is.null(mp)) {
      out[[v]] <- list(var = v, level_label = NA_character_, country = NA_character_)
    } else {
      for (j in seq_len(nrow(mp))) {
        term <- paste0(v, "__", sub("^__", "", mp$code[[j]]))
        out[[term]] <- list(var = v, level_label = mp$label[[j]], country = NA_character_)
      }
    }
  }
  if (length(added_ctx)) for (nm in added_ctx) out[[nm]] <- ctx_term_map[[nm]]
  out
}

