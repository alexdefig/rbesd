
# ── besd_regress() ─────────────────────────────────────────────────────────────

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
#' @section Missing data and complete-case analysis:
#' `besd_regress()` uses **listwise complete-case deletion**: any row with a missing
#' value in the outcome or any predictor is dropped before fitting. This is unbiased
#' only if data are missing completely at random (MCAR). If missingness is associated
#' with the outcome or a predictor (MAR, MNAR) estimates may be biased. 
#'
#' Use [besd_missing_summary()] to inspect variable-level missingness before fitting.
#' A warning is issued automatically when any predictor exceeds 5% missing or when 
#' the listwise complete-case dataset exceeds 5% missingness. If missingness is 
#' substantial or patterned, consider imputing (e.g. with \pkg{mice}) before calling 
#' `besd_regress()`.
#'
#' For context predictors (e.g. ethnicity), `min_n_context` (default 10) silently
#' recodes observations belonging to rare levels within a country to `NA` prior to 
#' fitting, to avoid near-empty cells in the model matrix. These observations are then
#' excluded by complete-case deletion. Reduce this threshold only if you are confident 
#' small cells will not cause convergence problems.
#' @note **Survey weights are not currently supported.** If `x` was created with
#'   a `weight_col`, that column is stored in the object but ignored by `besd_regress()`. 
#'   All models are fitted on unweighted data. This is a known limitation; weighted 
#'   regression support is planned for a future release.
#'   
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
  
  # Assert class and get object fields
  .assert_besd(x)
  info <- besd_info(x)
  
  # Match input args
  scope  <- match.arg(scope)
  engine <- match.arg(engine)
  ref    <- match.arg(ref)
  
  # Validate outcome early to avoid confusing downstream errors
  if (!is.character(outcome) || length(outcome) != 1L || is.na(outcome) || 
      !nzchar(outcome) || !(outcome %in% info$besd_items)) { 
    .stopf("`outcome` must be a single non-empty BeSD `item_id` string.")
  }
  
  df <- dplyr::as_tibble(x)
  dict <- info$besd_dict
  country_col <- info$country_col
  
  # Warn if the object carries a weight column - it is not used in regression.
  if (!is.null(info$weight_col) && nzchar(info$weight_col)) {
    warning(
      sprintf(
        paste0(
          "Survey weights (`%s`) are stored in this `besd_data` object but are not ",
          "currently supported by `besd_regress()`. Models will be fitted on unweighted ", 
          "data."
        ),
        info$weight_col
      ),
      call. = FALSE
    )
  }
  
  # Prepare besd_data object for regression
  prep <- besd_prepare(
   x, 
   outcome       = outcome, 
   predictors    = predictors,
   scope         = scope,
   engine        = engine,
   ref           = ref,
   random_slopes = random_slopes,
   correlated_re = correlated_re,
   ref_levels    = ref_levels,
   min_n_context = min_n_context
  )
 
 # Dispatch to fitting call
 if (scope == "by_country") {
   .fit_by_country(prep, ...)
 } else {
   .fit_multilevel(prep, ...)
 }
}


# ── .fit_by_country(), .fit_multilevel(), .fit_model() ─────────────────────────

# Fit per-country models: encodes predictors, applies complete-case deletion
# and two-pass predictor dropping within each country, then fits one model
# per outcome. Countries or outcomes with insufficient variance are skipped.
.fit_by_country <- function(prep, ...) {
  df          <- prep$df
  outcomes    <- prep$outcomes
  y_type      <- prep$y_type
  predictors  <- prep$preds_common
  country_col <- prep$country_col
  engine      <- prep$engine
  countries   <- prep$countries
  
  warned_refs <- new.env(parent = emptyenv())
  fits <- stats::setNames(vector("list", length(countries)), countries)
  
  for (cc in countries) {
    dcc     <- df[df[[country_col]] == cc, , drop = FALSE]
    fits_cc <- list()
    
    preds_cc <- predictors[vapply(
      predictors,
      function(p) any(!is.na(dcc[[p]])),
      logical(1)
    )]
    
    for (yy in outcomes) {
      
      dd_y <- dcc[!is.na(dcc[[yy]]), c(yy, preds_cc), drop = FALSE]
      if (nrow(dd_y) == 0L) next
      
      keep1 <- preds_cc[vapply(preds_cc, function(p) {
        v <- dd_y[[p]]
        v <- v[!is.na(v)]
        if (!length(v)) return(FALSE)
        if (is.factor(v)) nlevels(droplevels(v)) >= 2L
        else              length(unique(v)) >= 2L
      }, logical(1))]
      if (!length(keep1)) next
      
      dd <- dd_y[, c(yy, keep1), drop = FALSE]
      dd <- dd[stats::complete.cases(dd), , drop = FALSE]
      dd <- droplevels(dd)
      if (nrow(dd) == 0L) next
      
      dd <- .apply_within_refs(dd, keep1, cc, prep$ref_code_by_group,
                               warned = warned_refs)
      
      y <- dd[[yy]]
      if ((is.factor(y) && nlevels(y) < 2L) ||
          (!is.factor(y) && length(unique(y)) < 2L)) next
      
      keep2 <- keep1[vapply(keep1, function(p) {
        v <- dd[[p]]
        if (is.factor(v)) nlevels(v) >= 2L else length(unique(v)) >= 2L
      }, logical(1))]
      if (!length(keep2)) next
      
      if (length(keep2) < length(keep1)) {
        dd <- dd_y[, c(yy, keep2), drop = FALSE]
        dd <- dd[stats::complete.cases(dd), , drop = FALSE]
        dd <- droplevels(dd)
        if (nrow(dd) == 0L) next
        dd <- .apply_within_refs(dd, keep2, cc, prep$ref_code_by_group,
                                 warned = warned_refs)
        y  <- dd[[yy]]
        if ((is.factor(y) && nlevels(y) < 2L) ||
            (!is.factor(y) && length(unique(y)) < 2L)) next
      }
      
      f <- stats::as.formula(paste0(yy, " ~ ", paste(keep2, collapse = " + ")))
      fits_cc[[yy]] <- .fit_model(dd, f, y_type, engine, FALSE,
                                  do_complete_cases = FALSE, ...)
      fits[[cc]] <- fits_cc
    }
  }
  
  structure(
    list(
      fits = fits,
      meta = list(
        scope             = "by_country",
        engine            = prep$engine,
        outcome           = prep$outcome,
        outcomes          = prep$outcomes,
        y_type            = prep$y_type,
        predictors        = prep$preds_common,
        country_col       = prep$country_col,
        countries         = prep$countries,
        ref_rule          = prep$ref_rule,
        level_map         = prep$level_map,
        ref_code_by_group = prep$ref_code_by_group,
        dem_dict          = prep$dem_dict
      ),
      dict = prep$dict
    ),
    class = "besd_fit_by_country"
  )
}

# Fit a single multilevel model across all countries. Encoding and dummy expansion
# are already done; this function builds the mixed-effects formula, applies 
# complete-case deletion globally, and dispatches to the engine.
.fit_multilevel <- function(prep, ...) {
  df           <- prep$df
  outcomes     <- prep$outcomes
  y_type       <- prep$y_type
  preds_common <- prep$preds_common
  added_ctx    <- prep$added_ctx
  country_col  <- prep$country_col
  engine       <- prep$engine
  
  predictors_fixed <- c(preds_common, added_ctx)
  fits <- list()
  
  for (yy in outcomes) {
    fixed_part <- paste(predictors_fixed, collapse = " + ")
    
    random_part <- if (prep$random_slopes && length(preds_common)) {
      bar <- if (prep$use_uncor) "||" else "|"
      paste0("(1 + ", paste(preds_common, collapse = " + "),
             " ", bar, " ", country_col, ")")
    } else {
      paste0("(1 | ", country_col, ")")
    }
    
    f    <- stats::as.formula(paste0(yy, " ~ ", fixed_part, " + ", random_part))
    vars <- unique(c(yy, all.vars(f)))
    dd   <- df[, vars, drop = FALSE]
    dd   <- dd[stats::complete.cases(dd), , drop = FALSE]
    if (nrow(dd) == 0L) next
    
    fits[[yy]] <- .fit_model(dd, f, y_type, engine, TRUE,
                             do_complete_cases = FALSE, ...)
  }
  
  structure(
    list(
      fits = fits,
      meta = list(
        scope                       = "multilevel",
        engine                      = prep$engine,
        outcome                     = prep$outcome,
        outcomes                    = prep$outcomes,
        y_type                      = prep$y_type,
        predictors_common           = prep$preds_common,
        predictors_context          = prep$preds_context,
        country_col                 = prep$country_col,
        ref_rule                    = prep$ref_rule,
        random_slopes               = prep$random_slopes,
        correlated_re               = prep$correlated_re,
        ref_code_common             = prep$ref_code,
        ref_code_by_group_context   = prep$ref_code_by_group_context,
        level_map                   = prep$level_map,
        level_map_context           = prep$level_map_context,
        term_map                    = prep$term_map,
        country_code_of             = prep$country_code_of,
        dem_dict                    = prep$dem_dict,
        uncorrelated_random_effects = prep$use_uncor
      ),
      dict = prep$dict
    ),
    class = "besd_fit"
  )
}

# Dispatch to the appropriate model fitter based on engine and y_type. Expects a 
# pre-filtered data frame; do_complete_cases should be FALSE in normal use since 
# callers handle deletion before this point.
.fit_model <- function(dat, formula, y_type, engine, multilevel) {
  
  vars    <- all.vars(formula)
  missing <- setdiff(vars, names(dat))
  if (length(missing)) .stopf("Formula references missing: %s", .pastec(missing))
  
  dd <- dat[, unique(vars), drop = FALSE]
  
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
  
  .require_pkg("brms", "for Bayesian regression")
  if (y_type == "binary") {
    pri <- c(brms::set_prior("normal(0, 1)", class = "b"),
             brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept"))
    return(brms::brm(formula, data = dd,
                     family = brms::bernoulli(link = "logit"),
                     prior = pri, ...))
  }
  if (y_type == "ordinal") {
    pri <- c(brms::set_prior("normal(0, 1)", class = "b"),
             brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept"))
    return(brms::brm(formula, data = dd,
                     family = brms::cumulative(link = "logit"),
                     prior = pri, ...))
  }
  .stopf("Unsupported y_type: %s", y_type)
}

# ── expand multichoice BeSD helper ─────────────────────────────────────────────
.expand_multichoice_outcome <- function(df, item, levels_std, sep = .BESD_SEP) {
  x     <- as.character(df[[item]])
  x[x == ""] <- NA_character_
  
  opt_codes <- .make_safe_names(levels_std, sep = "_")
  out_names <- paste0(item, "_mc_", opt_codes)
  
  mat <- lapply(levels_std, function(lv) {
    as.integer(!is.na(x) & vapply(
      strsplit(x, sep, fixed = TRUE),
      function(tok) lv %in% tok,
      logical(1)
    ))
  })
  names(mat) <- out_names
  
  df2 <- cbind(df, as.data.frame(mat, stringsAsFactors = FALSE))
  list(df = df2, outcomes = out_names)
}
