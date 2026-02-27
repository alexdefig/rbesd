# ── besd_prepare() ─────────────────────────────────────────────────────────────

#' Prepare a BeSD dataset for regression
#'
#' Encodes predictors, selects reference levels, applies `min_n_context`
#' rare-level collapsing, expands context dummies, and runs pre-flight checks
#' (invariant predictors, missingness warnings). Returns a `besd_prep` object
#' consumed by `besd_regress()` internally, or inspectable by the user before
#' fitting.
#'
#' @param x A `besd_data` object.
#' @param outcome Single BeSD `item_id`.
#' @param predictors Character vector (`by_country`) or
#'   `list(common = ..., context = ...)` (`multilevel`).
#' @param scope `"by_country"` or `"multilevel"`.
#' @param engine `"frequentist"` or `"bayes"`.
#' @param ref Reference rule: `"mode"` or `"first"`.
#' @param random_slopes If `TRUE` (multilevel), random slopes for common
#'   predictors.
#' @param correlated_re If `TRUE` (multilevel Bayes), estimate correlations
#'   between random effects.
#' @param ref_levels Optional named list of explicit reference labels
#'   (multilevel common predictors only).
#' @param min_n_context Rare-level threshold for context predictors (default
#'   `10L`). Observations in levels with fewer than this many observations
#'   within a country are recoded to `NA` before fitting.
#'
#' @return A `besd_prep` object (a named list with class `"besd_prep"`)
#'   containing the encoded data frame, predictor metadata, level maps,
#'   reference codes, term map, and all configuration fields needed for
#'   fitting and tidying.
#' @export
besd_prepare <- function(x,
                         outcome,
                         predictors,
                         scope         = c("by_country", "multilevel"),
                         engine        = c("frequentist", "bayes"),
                         ref           = c("mode", "first"),
                         random_slopes = FALSE,
                         correlated_re = FALSE,
                         ref_levels    = list(),
                         min_n_context = 10L) {
  
  .assert_besd(x)
  
  scope  <- match.arg(scope)
  engine <- match.arg(engine)
  ref    <- match.arg(ref)
  
  info        <- besd_info(x)
  df          <- dplyr::as_tibble(x)
  dict        <- info$besd_dict
  country_col <- info$country_col
  
  # ── Guard: un-recoded missing tokens ────────────────────────────────────────
  # If missing_action = "keep" was used in as_besd(), missing-response tokens
  # (e.g. "Don't know") remain as trailing factor levels rather than NA.
  # These must be recoded before fitting or they will be passed to the model
  # as real response categories. We detect this by checking whether any factor
  # column involved in the model has levels beyond those in the dictionary.
  .check_no_missing_token_levels(df, outcome, predictors, dict, info$meta)
  
  # ── Expand multichoice outcomes ─────────────────────────────────────────────
  y_type   <- .item_type(dict, outcome)
  outcomes <- outcome
  if (y_type == "multichoice") {
    levs     <- dict$levels[[match(outcome, dict$item_id)]]
    ex       <- .expand_multichoice_outcome(df, outcome, levs, sep = .BESD_SEP)
    df       <- ex$df
    outcomes <- ex$outcomes
    y_type   <- "binary"
  }
  
  # ── Normalise predictors ────────────────────────────────────────────────────
  if (scope == "by_country") {
    preds_common  <- as.character(predictors)
    preds_context <- character()
  } else {
    preds_list    <- if (is.list(predictors)) predictors
    else list(common = predictors, context = character())
    preds_common  <- preds_list$common  %||% character()
    preds_context <- preds_list$context %||% character()
  }
  all_preds <- unique(c(preds_common, preds_context))
  
  .assert_has_cols(df, c(country_col, all_preds), "data")
  .check_predictor_variance(df, all_preds)
  
  # ── Scope-specific encoding ─────────────────────────────────────────────────
  
  if (scope == "by_country") {
    
    prep_preds <- .prep_predictors(df, preds_common,
                                   group_col = country_col,
                                   min_n     = 5L,
                                   ref_rule  = ref)
    df <- prep_preds$df
    
    .warn_missingness(df,
                      vars        = c(outcomes, preds_common),
                      country_col = country_col,
                      threshold   = 0.05,
                      context     = "by_country")
    
    countries                 <- sort(unique(as.character(df[[country_col]])))
    level_map                 <- prep_preds$level_map
    level_map_context         <- list()
    ref_code                  <- list()
    ref_code_by_group         <- prep_preds$ref_code_by_group
    ref_code_by_group_context <- list()
    added_ctx                 <- character()
    term_map                  <- list()
    country_code_of           <- list()
    use_uncor                 <- FALSE
    
  } else {
    
    # Common predictors: global reference selection, no grouping
    prep_common <- .prep_predictors(df, preds_common,
                                    group_col = NULL,
                                    min_n     = 0L,
                                    ref_rule  = ref)
    df <- prep_common$df
    
    # Apply explicit reference overrides (common predictors only)
    for (nm in intersect(names(ref_levels), preds_common)) {
      v          <- df[[nm]]
      if (!is.factor(v) || is.ordered(v)) next
      want_label <- ref_levels[[nm]]
      if (!is.character(want_label) || length(want_label) != 1L) next
      mp         <- prep_common$level_map[[nm]]
      if (is.null(mp)) next
      idx        <- match(want_label, mp$label)
      if (!is.na(idx)) {
        want_code <- mp$code[[idx]]
        if (want_code %in% levels(v)) df[[nm]] <- stats::relevel(v, ref = want_code)
      }
    }
    
    # Context predictors: per-country reference selection + min_n collapsing
    if (length(preds_context)) {
      prep_ctx <- .prep_predictors(df, preds_context,
                                   group_col = country_col,
                                   min_n     = as.integer(min_n_context),
                                   ref_rule  = ref)
      df <- prep_ctx$df
    } else {
      prep_ctx <- list(level_map = list(), ref_code_by_group = list())
    }
    
    # Country encoding (C01, C02, ...)
    countries       <- sort(unique(as.character(df[[country_col]])))
    country_code_of <- stats::setNames(sprintf("C%02d", seq_along(countries)),
                                       countries)
    
    # Expand context variables to per-country dummy columns
    added_ctx    <- character()
    ctx_term_map <- list()
    if (length(preds_context)) {
      for (var in preds_context) {
        ex           <- .add_context_dummies(df, var, country_col,
                                             country_code_of,
                                             prep_ctx$ref_code_by_group[[var]],
                                             prep_ctx$level_map[[var]])
        df           <- ex$df
        added_ctx    <- c(added_ctx, ex$added)
        ctx_term_map <- c(ctx_term_map, ex$term_map)
      }
    }
    
    # Warn on joint missingness across outcome + common predictors only.
    # Context predictors are intentionally excluded: their NAs are converted to
    # 0s in the dummy columns above and do NOT cause listwise deletion. A
    # country where a context predictor is entirely missing is expected and
    # normal — those rows simply contribute without that predictor.
    .warn_missingness(df,
                      vars        = c(outcomes, preds_common),
                      country_col = country_col,
                      threshold   = 0.05,
                      context     = "multilevel")
    
    # Separately report countries where a context predictor has no observed
    # levels — informational only, not a data quality warning.
    if (length(preds_context)) {
      .inform_context_coverage(df, preds_context, country_col)
    }
    
    level_map                 <- prep_common$level_map
    level_map_context         <- prep_ctx$level_map
    ref_code                  <- prep_common$ref_code
    ref_code_by_group         <- list()
    ref_code_by_group_context <- prep_ctx$ref_code_by_group
    term_map                  <- .build_term_map(preds_common, prep_common$level_map,
                                                 added_ctx, ctx_term_map)
    use_uncor                 <- isTRUE(engine == "bayes" &&
                                          isTRUE(random_slopes) &&
                                          !isTRUE(correlated_re))
  }
  
  structure(
    list(
      # Prepared data
      df                        = df,
      outcomes                  = outcomes,
      y_type                    = y_type,
      # Predictor names
      preds_common              = preds_common,
      preds_context             = preds_context,
      added_ctx                 = added_ctx,
      # Encoding lookups
      level_map                 = level_map,
      level_map_context         = level_map_context,
      ref_code                  = ref_code,
      ref_code_by_group         = ref_code_by_group,
      ref_code_by_group_context = ref_code_by_group_context,
      # Decoding support (multilevel only; empty list for by_country)
      term_map                  = term_map,
      country_code_of           = country_code_of,
      countries                 = countries,
      # Configuration
      scope                     = scope,
      engine                    = engine,
      outcome                   = outcome,
      country_col               = country_col,
      ref_rule                  = ref,
      random_slopes             = random_slopes,
      correlated_re             = correlated_re,
      use_uncor                 = use_uncor,
      min_n_context             = as.integer(min_n_context),
      dem_dict                  = info$dem_dict,
      dict                      = dict
    ),
    class = "besd_prep"
  )
}


# Encode predictors, select reference levels, and apply min_n rare-level collapsing.
# Returns a list: `df` (modified), `level_map`, `ref_code` (ungrouped), and
# `ref_code_by_group` (per-group, when `group_col` is set).
.prep_predictors <- function(df, predictors, group_col = NULL, min_n = 0L,
                             ref_rule = "mode") {
  .assert_has_cols(df, predictors, "df")
  if (!is.null(group_col)) .assert_has_cols(df, group_col, "df")
  
  grp    <- if (!is.null(group_col)) as.character(df[[group_col]]) else NULL
  groups <- if (!is.null(grp)) unique(grp) else character(0)
  
  level_map          <- list()
  ref_code_by_group  <- list()
  ref_code           <- list()
  
  for (nm in predictors) {
    v <- if (is.character(df[[nm]])) {
      factor(df[[nm]])
    } else if (is.logical(df[[nm]])) {
      as.integer(df[[nm]])
    } else {
      df[[nm]]
    }
    
    # Recode rare levels to NA within each group (context-style)
    if (!is.null(grp) && is.factor(v) && min_n > 0L) {
      vv <- as.character(v)
      for (g in unique(grp)) {
        ii   <- which(grp == g)
        tab  <- table(vv[ii], useNA = "no")
        rare <- names(tab)[tab < min_n]
        if (length(rare)) vv[ii][vv[ii] %in% rare] <- NA_character_
      }
      v <- droplevels(factor(vv, levels = levels(v)))
    }
    
    # Encode unordered factors to opaque codes (__01, __02, ...)
    if (is.factor(v) && !is.ordered(v)) {
      levs   <- levels(v)
      codes  <- sprintf("__%02d", seq_along(levs))
      v      <- factor(codes[match(v, levs)], levels = codes)
      
      level_map[[nm]] <- tibble::tibble(code = codes, label = levs)
      code_of  <- stats::setNames(codes, levs)
      label_of <- stats::setNames(levs, codes)
      
      # Global reference (ungrouped only — multilevel common predictors)
      if (is.null(grp)) {
        labels_now <- label_of[as.character(v)]
        ref_label  <- .pick_ref(labels_now, ref_rule, levels = levs)
        ref0       <- code_of[[ref_label]]
        if (!is.null(ref0) && !is.na(ref0) && ref0 %in% levels(v)) {
          v <- stats::relevel(v, ref = ref0)
        }
        ref_code[[nm]] <- if (!is.null(ref0) && !is.na(ref0)) {
          as.character(ref0)
        } else {
          NA_character_
        }
      }
      
      # Per-group references (by_country and multilevel context predictors)
      if (!is.null(grp)) {
        lab  <- label_of[as.character(v)]
        refs <- stats::setNames(rep(NA_character_, length(groups)), groups)
        for (g in groups) {
          ii        <- which(grp == g)
          ref_label <- .pick_ref(lab[ii], ref_rule, levels = levs)
          if (is.na(ref_label)) {
            refs[g] <- NA_character_
          } else {
            ref0    <- unname(code_of[ref_label])
            refs[g] <- if (!is.na(ref0)) ref0 else NA_character_
          }
        }
        ref_code_by_group[[nm]] <- refs
      }
    }
    
    # Non-factor predictors: no global baseline code
    if (is.null(grp) && is.null(ref_code[[nm]])) ref_code[[nm]] <- NA_character_
    
    df[[nm]] <- v
  }
  
  list(df = df, level_map = level_map,
       ref_code_by_group = ref_code_by_group,
       ref_code = ref_code)
}


# Relevel factors in `df` to the per-country references stored in `ref_code_by_group`, 
# for the single country `group_value`.
.apply_within_refs <- function(df, predictors, group_value, ref_code_by_group,
                               warned = NULL) {
  for (nm in predictors) {
    if (!is.factor(df[[nm]]) || is.ordered(df[[nm]])) next
    refs <- ref_code_by_group[[nm]]
    if (is.null(refs)) next
    
    r0   <- refs[[group_value]]
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
              "Within-country reference '%s' for predictor '%s' in '%s' is ",
              "not present after filtering; leaving default reference '%s'."
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


# Expand a single context variable into per-country binary dummy columns.
# Returns list: `df` (with new columns), `added` (column names), `term_map`.
.add_context_dummies <- function(df, var, country_col, country_code_of,
                                 ref_code_by_group, level_map) {
  v <- df[[var]]
  if (!is.factor(v) || is.ordered(v)) {
    .stopf("Context predictor `%s` must be an unordered factor.", var)
  }
  
  cc    <- as.character(df[[country_col]])
  v_chr <- as.character(v)   # codes like "__01" or NA
  
  added    <- character(0)
  term_map <- list()
  
  label_of <- if (!is.null(level_map) && all(c("code", "label") %in% names(level_map))) {
    stats::setNames(level_map$label, level_map$code)
  } else NULL
  
  for (country in names(country_code_of)) {
    ccode    <- country_code_of[[country]]
    base     <- ref_code_by_group[[country]] %||% NA_character_
    ii       <- which(cc == country)
    
    lev_here <- unique(v_chr[ii])
    lev_here <- lev_here[!is.na(lev_here)]
    
    # No levels observed in this country — skip entirely
    if (!length(lev_here)) next
    
    # Fall back to first observed level if no valid baseline
    if (is.na(base)) base <- lev_here[[1]]
    
    for (lv in setdiff(lev_here, base)) {
      col <- paste0("ctx_", var, "_", ccode, "_", sub("^__", "", lv))
      
      # NA in `v_chr` must not propagate to the dummy — treat as 0
      df[[col]] <- as.integer(cc == country & !is.na(v_chr) & v_chr == lv)
      
      added <- c(added, col)
      term_map[[col]] <- list(
        var            = var,
        country        = country,
        level_code     = lv,
        level_label    = if (!is.null(label_of)) unname(label_of[[lv]])
        else NA_character_,
        baseline_code  = base,
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


# Build the term_map used by tidy_model() / .parse_terms() to decode internal
# column names back to human-readable variable + level labels.
.build_term_map <- function(preds_common, level_map_common, added_ctx,
                            ctx_term_map) {
  out <- list()
  for (v in preds_common) {
    mp <- level_map_common[[v]]
    if (is.null(mp)) {
      out[[v]] <- list(var = v, level_label = NA_character_,
                       country = NA_character_)
    } else {
      for (j in seq_len(nrow(mp))) {
        term        <- paste0(v, "__", sub("^__", "", mp$code[[j]]))
        out[[term]] <- list(var = v, level_label = mp$label[[j]],
                            country = NA_character_)
      }
    }
  }
  if (length(added_ctx)) {
    for (nm in added_ctx) out[[nm]] <- ctx_term_map[[nm]]
  }
  out
}


# ── Missing-token level guard ──────────────────────────────────────────────────

# Stop if any outcome or predictor column still carries factor levels that are
# not in the dictionary — the signature of missing_action = "keep" having been
# left in place before fitting. These extra levels are missing-response tokens
# (e.g. "Don't know") that would be passed to the model as real categories.
#
# Detection: a factor column whose levels include anything not in dict$levels
# for that item. Only checks columns that are in the dictionary; dem predictors
# not in besd_dict are looked up in dem_dict if present.
.check_no_missing_token_levels <- function(df, outcome, predictors, dict,
                                           meta = NULL) {
  all_preds <- if (is.list(predictors)) {
    unique(c(predictors$common %||% character(), predictors$context %||% character()))
  } else {
    as.character(predictors)
  }
  
  cols_to_check <- unique(c(outcome, all_preds))
  cols_to_check <- intersect(cols_to_check, names(df))
  
  # Build a combined name -> dict_levels lookup from both dicts
  dict_levels <- stats::setNames(dict$levels, dict$item_id)
  
  bad <- character()
  for (col in cols_to_check) {
    v <- df[[col]]
    if (!is.factor(v) || is.ordered(v)) next
    expected <- dict_levels[[col]]
    if (is.null(expected)) next          # not in dict — skip
    extra <- setdiff(levels(v), expected)
    if (length(extra)) bad <- c(bad, col)
  }
  
  if (!length(bad)) return(invisible(NULL))
  
  # Build a helpful hint from meta$missing_tokens if available
  tokens <- meta$missing_tokens %||% NULL
  token_hint <- if (!is.null(tokens)) {
    toks <- if (is.character(tokens)) tokens else unlist(tokens, use.names = FALSE)
    toks <- unique(toks[!is.na(toks) & nzchar(toks)])
    if (length(toks)) {
      paste0(
        " Missing tokens detected: ",
        paste(paste0('"', toks, '"'), collapse = ", "), "."
      )
    } else ""
  } else ""
  
  .stopf(
    paste0(
      "Column(s) %s still contain factor levels outside the dictionary. ",
      "This usually means `missing_action = \"keep\"` was used in `as_besd()` ",
      "and missing-response tokens have not been recoded to NA.",
      "%s",
      " Call `besd_recode_missing()` before fitting, or use ",
      "`missing_action = \"na\"` in `as_besd()`."
    ),
    .pastec(bad),
    token_hint
  )
}


# ── Predictor validation ───────────────────────────────────────────────────────

# Stop with an actionable message if any predictor is globally constant or all-NA.
.check_predictor_variance <- function(df, preds) {
  if (!length(preds)) return(invisible(NULL))
  
  invariant <- vapply(preds, function(p) {
    if (!p %in% names(df)) return(FALSE)   # missing cols caught by .assert_has_cols
    v <- df[[p]]
    v <- v[!is.na(v)]
    if (!length(v)) return(TRUE)           # all-NA is effectively constant
    n_unique <- if (is.factor(v)) nlevels(droplevels(v)) else length(unique(v))
    n_unique < 2L
  }, logical(1))
  
  bad <- preds[invariant]
  if (!length(bad)) return(invisible(NULL))
  
  .stopf(
    paste0(
      "The following predictor%s %s constant (only one unique non-NA value) ",
      "across the full dataset and cannot be used in regression. ",
      "Please remove %s from `predictors` and re-run:\n  %s"
    ),
    if (length(bad) == 1L) "" else "s",
    if (length(bad) == 1L) "is" else "are",
    if (length(bad) == 1L) "it" else "them",
    paste(bad, collapse = ", ")
  )
}


# ── Reference-level selection ──────────────────────────────────────────────────

# Regex for catch-all / residual labels that should never be the reference level.
.PICK_REF_AVOID_RE <- paste0(
  "^other",           # Other, Other/prefer not to say, Other/NB, …
  "|^prefernot",      # Prefer not to say, Prefer not to answer
  "|^declin",         # Declined, Decline to state
  "|^noneofthe",      # None of the above
  "|^notstat",        # Not stated
  "|^unspecified$",   # Unspecified
  "|^unknown$"        # Unknown
)

# Return TRUE for labels that are catch-all / residual categories.
.is_catchall <- function(x) {
  grepl(.PICK_REF_AVOID_RE, .strip_non_alpnum(x))
}

# Pick reference level
.pick_ref <- function(x, rule = "mode", levels = NULL) {
  rule <- match.arg(rule, c("mode", "first"))
  x    <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  
  if (rule == "first") {
    if (!is.null(levels) && length(levels)) {
      levs      <- as.character(levels)
      levs      <- levs[!is.na(levs)]
      hit       <- levs[levs %in% x]
      preferred <- hit[!.is_catchall(hit)]
      if (length(preferred)) return(preferred[[1]])
      if (length(hit))       return(hit[[1]])
      return(levs[[1]])
    }
    if (is.factor(x) && length(levels(x))) {
      levs      <- levels(x)
      preferred <- levs[!.is_catchall(levs) & levs %in% x]
      if (length(preferred)) return(preferred[[1]])
      return(levs[[1]])
    }
    levs      <- levels(factor(x))
    preferred <- levs[!.is_catchall(levs)]
    if (length(preferred)) return(preferred[[1]])
    return(levs[[1]])
  }
  
  # rule == "mode": most frequent non-catch-all level
  tab           <- table(x)
  tab_preferred <- tab[!.is_catchall(names(tab))]
  
  # Degenerate case: every level is a catch-all — fall back to overall mode
  if (!length(tab_preferred)) tab_preferred <- tab
  
  names(tab_preferred)[which.max(tab_preferred)][[1]]
}