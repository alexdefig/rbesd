# ── besd_regress() ─────────────────────────────────────────────────────────────

#' Regression modelling for BeSD outcomes
#'
#' @param x A `besd_data` object.
#' @param outcome One or more BeSD `item_id` strings.
#' @param predictors Character vector (by_country) or list(common, context) (multilevel).
#'   Predictors may be demographic variables **or** BeSD `item_id`s, so item-on-item
#'   models such as `tf_benefits ~ tf_safety` are supported. `multichoice` items
#'   cannot be used as predictors (they pack several responses into one column);
#'   derive a binary indicator column first.
#' @param scope "by_country" or "multilevel".
#' @param engine "frequentist" or "bayes".
#' @param ref Reference rule: "mode" or "first".
#' @param random_slopes If TRUE (multilevel), fit random slopes for common predictors.
#' @param correlated_re If TRUE (multilevel Bayes), estimate correlations between
#'   random effects.
#' @param ref_levels Optional named list of explicit reference labels (multilevel
#'   common predictors only). Ordinal BeSD items used as predictors are dummy-coded
#'   against a reference level rather than fitted with polynomial contrasts, and
#'   `ref = "mode"` selects the modal level as that reference. To compare against the
#'   bottom of the scale instead, name it explicitly, e.g.
#'   `ref_levels = list(tf_safety = "Not at all safe")`.
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
#' substantial consider imputing (e.g. with \pkg{mice}) before calling `besd_regress()`.
#'
#' For context predictors (e.g. ethnicity), `min_n_context` (default 10) silently
#' recodes observations belonging to rare levels within a country to `NA` prior to
#' fitting, to avoid near-empty cells in the model matrix. These observations are then
#' excluded by complete-case deletion. Reduce this threshold only if you are confident 
#' small cells will not cause convergence problems.
#' 
#' After examining missing data with [besd_missing_summary()] consider manually 
#' recoding rare levels before calling [as_besd()].
#' 
#' @section Poststratification:
#' [besd_poststratify()] requires every predictor to exist in the poststratification
#' frame. A model whose predictors include BeSD items therefore cannot be
#' poststratified; the model fit and [tidy_model()] output are unaffected.
#'
#' @note **Survey weights are not currently supported.** If `x` was created with
#'   a `weight_col`, that column is stored in the object but ignored by `besd_regress()`.
#'   All models are fitted on unweighted data. This is a known limitation; weighted
#'   regression support is planned for a future release.
#'
#' @export
besd_regress <- function(x,
                         outcome,
                         predictors,
                         scope         = c("by_country", "multilevel"),
                         engine        = c("frequentist", "bayes"),
                         ref           = c("mode", "first"),
                         random_slopes = FALSE,
                         correlated_re = FALSE,
                         ref_levels    = list(),
                         min_n_context = 10L, ...) {
  
  .assert_besd(x)
  info <- besd_info(x)
  
  scope  <- match.arg(scope)
  engine <- match.arg(engine)
  ref    <- match.arg(ref)
  
  # Validate outcome before passing to besd_prepare() to give a clear error.
  if (!is.character(outcome) || length(outcome) == 0L ||
      any(is.na(outcome)) || any(!nzchar(outcome)) ||
      any(!outcome %in% info$besd_items)) {
    .stopf("`outcome` must be a non-empty character vector of BeSD `item_id` strings.")
  }
  
  # Validate predictors. Predictors may be demographics or BeSD `item_id`s, but
  # multichoice items are packed multi-response strings and cannot be used, and
  # an outcome must not also appear on the right-hand side.
  preds_flat <- if (is.list(predictors)) {
    unique(c(predictors$common %||% character(), predictors$context %||% character()))
  } else {
    as.character(predictors)
  }
  preds_flat <- preds_flat[!is.na(preds_flat) & nzchar(preds_flat)]

  clash <- intersect(outcome, preds_flat)
  if (length(clash)) {
    .stopf(
      "Item(s) %s appear as both `outcome` and `predictors`. A variable cannot predict itself.",
      .pastec(clash)
    )
  }

  mc_preds <- intersect(preds_flat, info$besd_items)
  mc_preds <- mc_preds[vapply(mc_preds,
                              function(p) .item_type(info$besd_dict, p) == "multichoice",
                              logical(1))]
  if (length(mc_preds)) {
    .stopf(
      paste0(
        "Multichoice item(s) %s cannot be used as predictors: they store several ",
        "responses packed into one column. Derive a binary indicator column for the ",
        "response option(s) of interest and use that instead."
      ),
      .pastec(mc_preds)
    )
  }

  # Missing-token guard for predictors. Must run on the raw data: after
  # besd_prepare() the predictor levels are opaque `__01` codes.
  .check_no_missing_token_levels(dplyr::as_tibble(x), character(0), preds_flat,
                                 info$besd_dict, info$meta,
                                 include_ordered = TRUE)

  # Warn if the object carries a weight column — it is not used in regression.
  if (!is.null(info$weight_col) && nzchar(info$weight_col %||% "")) {
    warning(
      sprintf(
        paste0(
          "Survey weights (`%s`) are stored in this `besd_data` object but are not ",
          "currently supported by `besd_regress()`. Models will be fitted on ",
          "unweighted data."
        ),
        info$weight_col
      ),
      call. = FALSE
    )
  }
  
  prep <- besd_prepare(
    x,
    predictors    = predictors,
    scope         = scope,
    engine        = engine,
    ref           = ref,
    random_slopes = random_slopes,
    correlated_re = correlated_re,
    ref_levels    = ref_levels,
    min_n_context = min_n_context,
    warn_missingness = FALSE
  )
  
  dict        <- info$besd_dict
  log_rows    <- list()
  fits_all    <- list()
  y_types_all <- list()
  label_map_all  <- list()  # sub-outcome -> human-readable level (multichoice only)
  parent_map_all <- list()  # sub-outcome -> parent item_id  (multichoice only)
  n           <- length(outcome)
  
  # Fit model for each outcome
  for (i in seq_along(outcome)) {
    yy <- outcome[[i]]
    if (n > 1L) message(sprintf("Fitting outcome %d/%d: %s", i, n, yy))
    t0 <- proc.time()[["elapsed"]]
    
    result <- tryCatch({
      
      # Missing token guard for this outcome
      .check_no_missing_token_levels(prep$df, yy, NULL, dict, info$meta)
      
      # Resolve y_type and expand multichoice on a local copy of df
      y_type   <- .item_type(dict, yy)
      df_y     <- prep$df
      outcomes <- yy
      outcome_label_map  <- list()
      outcome_parent_map <- list()
      if (y_type == "multichoice") {
        levs     <- dict$levels[[match(yy, dict$item_id)]]
        ex       <- .expand_multichoice_outcome(df_y, yy, levs, sep = .BESD_SEP)
        df_y     <- ex$df
        outcomes <- ex$outcomes
        y_type   <- "binary"
        outcome_label_map  <- ex$outcome_label_map
        outcome_parent_map <- ex$outcome_parent_map
      }
      
      # Warn on joint missingness for this outcome + common predictors
      .warn_missingness(df_y,
                        vars        = c(outcomes, prep$preds_common),
                        country_col = prep$country_col,
                        threshold   = 0.05,
                        context     = prep$scope)
      
      # Augment prep with outcome-specific fields for the fitting functions
      prep_y          <- prep
      prep_y$df       <- df_y
      prep_y$outcome  <- yy
      prep_y$outcomes <- outcomes
      prep_y$y_type   <- y_type
      prep_y$outcome_label_map <- outcome_label_map
      
      fit <- if (scope == "by_country") {
        .fit_by_country(prep_y, ...)
      } else {
        .fit_multilevel(prep_y, ...)
      }
      
      list(fit = fit, error = NULL)
      
    }, error = function(e) {
      list(fit = NULL, error = conditionMessage(e))
    })
    
    elapsed <- round(proc.time()[["elapsed"]] - t0, 1)
    log_rows[[yy]] <- tibble::tibble(
      outcome   = yy,
      status    = if (is.null(result$error)) "success" else "failed",
      runtime_s = elapsed,
      error     = result$error %||% NA_character_
    )
    
    if (!is.null(result$fit)) {
      # Multichoice outcomes expand into several binary sub-outcomes; keep the
      # sub-outcome -> level label map so poststratified rows can be labelled.
      label_map_all  <- utils::modifyList(label_map_all,  as.list(outcome_label_map))
      parent_map_all <- utils::modifyList(parent_map_all, as.list(outcome_parent_map))

      # Flatten the per-outcome besd_fit wrapper: extract raw models and y_type.
      # For by_country the inner structure is fits[[country]][[sub_outcome]];
      # for multilevel it is fits[[sub_outcome]]. Both are merged into fits_all
      # so the returned object has no double nesting regardless of n outcomes.
      if (scope == "by_country") {
        for (cc in names(result$fit$fits)) {
          if (is.null(fits_all[[cc]])) fits_all[[cc]] <- list()
          for (nm in names(result$fit$fits[[cc]])) {
            fits_all[[cc]][[nm]] <- result$fit$fits[[cc]][[nm]]
            y_types_all[[nm]]    <- result$fit$meta$y_type
          }
        }
      } else {
        for (nm in names(result$fit$fits)) {
          fits_all[[nm]]    <- result$fit$fits[[nm]]
          y_types_all[[nm]] <- result$fit$meta$y_type
        }
      }
    }
  }

  log    <- dplyr::bind_rows(log_rows)
  n_fail <- sum(log$status == "failed")
  if (n_fail > 0L)
    message(sprintf("%d outcome(s) failed. Inspect $log for details.", n_fail))

  structure(
    list(
      fits = fits_all,
      prep = prep,
      meta = list(
        scope    = scope,
        engine   = engine,
        outcomes = names(y_types_all),
        y_types  = y_types_all,
        outcome_label_map  = label_map_all,
        outcome_parent_map = parent_map_all
      ),
      log  = log
    ),
    class = if (scope == "by_country") "besd_fit_by_country" else "besd_fit"
  )
}


# ── Orchestrator ───────────────────────────────────────────────────────────────

# Dispatch to the appropriate model fitter based on engine and y_type. Expects
# a pre-filtered data frame; callers handle complete-case deletion before this.
.fit_model <- function(dat, formula, y_type, engine, multilevel, ...) {
  
  vars    <- all.vars(formula)
  missing <- setdiff(vars, names(dat))
  if (length(missing)) {
    .stopf("Formula references missing column(s): %s", .pastec(missing))
  } 
  
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


# ── Dispatchers ────────────────────────────────────────────────────────────────

# Fit per-country models: applies complete-case deletion and two-pass predictor
# dropping within each country, then fits one model per outcome. Countries or
# outcomes with insufficient variance are skipped.
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
          (!is.factor(y) && length(unique(y)) < 2L)) {
        outcome_label <- prep$outcome_label_map[[yy]] %||% yy
        warning(sprintf(
          paste0(
            "[%s / %s] Skipped: outcome has no variance (all values identical)", 
            "after complete-case deletion.",  
          ),
          cc, outcome_label
        ), call. = FALSE)
        next
      }
      
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
            (!is.factor(y) && length(unique(y)) < 2L)) {
          outcome_label <- prep$outcome_label_map[[yy]] %||% yy
          warning(sprintf(
            "[%s / %s] Skipped: outcome has no variance after predictor trimming.",
            cc, outcome_label
          ), call. = FALSE)
          next
        }
      }
      
      f <- stats::as.formula(paste0(yy, " ~ ", paste(keep2, collapse = " + ")))
      m <- withCallingHandlers(
        .fit_model(dd, f, y_type, engine, FALSE, ...),
        warning = function(w) {
          outcome_label <- (prep$outcome_label_map %||% list())[[yy]] %||% yy
          warning(sprintf("[%s / %s] %s", cc, outcome_label, conditionMessage(w)), 
                  call. = FALSE)
          invokeRestart("muffleWarning")
        }
      )
      
      # Record the predictor column names actually used so tidy_model() can
      # verify every coefficient is decodable by prep$term_map / prep$level_map.
      attr(m, "besd_fitted_terms") <- keep2
      fits_cc[[yy]] <- m
      fits[[cc]]    <- fits_cc
    }
  }
  
  structure(
    list(
      fits = fits,
      prep = prep,
      meta = list(
        scope    = "by_country",
        engine   = prep$engine,
        outcome  = prep$outcome,
        outcomes = prep$outcomes,
        y_type   = prep$y_type
      ),
      dict = prep$dict
    ),
    class = "besd_fit_by_country"
  )
}

# Fit a single multilevel model across all countries. Encoding and dummy are done in 
# besd_prepare(); this function builds the  mixed-effects formuls and applies 
# complete-case deletion globally.
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
    
    m <- tryCatch(
      withCallingHandlers(
        .fit_model(dd, f, y_type, engine, TRUE, ...),
        warning = function(w) {
          outcome_label <- (prep$outcome_label_map %||% list())[[yy]] %||% yy
          warning(sprintf("[multilevel / %s] %s", outcome_label, conditionMessage(w)),
                  call. = FALSE)
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        outcome_label <- (prep$outcome_label_map %||% list())[[yy]] %||% yy
        warning(sprintf("[multilevel / %s] Skipped: %s", outcome_label, conditionMessage(e)),
                call. = FALSE)
        NULL
      }
    )
    if (is.null(m)) next
    attr(m, "besd_fitted_terms") <- predictors_fixed
    fits[[yy]] <- m
  }
  
  structure(
    list(
      fits = fits,
      prep = prep,
      meta = list(
        scope    = "multilevel",
        engine   = prep$engine,
        outcome  = prep$outcome,
        outcomes = prep$outcomes,
        y_type   = prep$y_type
      ),
      dict = prep$dict
    ),
    class = "besd_fit"
  )
}


# ── Expand multichoice outcomes ─────────────────────────────────────────────────

# Expand a packed multichoice column into one binary indicator column per level.
# Returns a list with the augmented data frame (`df`) and the new column names
# (`outcomes`) to be used as model outcomes.
.expand_multichoice_outcome <- function(df, item, levels_std, sep = .BESD_SEP) {
  x         <- as.character(df[[item]])
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
  # after
  list(df = df2, outcomes = out_names,
       outcome_label_map  = stats::setNames(levels_std, out_names),
       # sub-outcome -> parent item_id, so downstream summaries can report the
       # parent item with one row per level (as summary.besd_data() does).
       outcome_parent_map = stats::setNames(rep(item, length(out_names)), out_names))
}


# ── Print methods ────────────────────────────────────────────────────────────────

#' @export
print.besd_fit <- function(x, ...) {
  n_out  <- length(x$meta$outcomes %||% character())
  n_ok   <- if (!is.null(x$log)) sum(x$log$status == "success") else n_out
  n_fail <- if (!is.null(x$log)) sum(x$log$status == "failed")  else 0L
  cat(sprintf(
    "<besd_fit>  scope: %s | engine: %s | %d fitted, %d failed\n",
    x$meta$scope %||% "?", x$meta$engine %||% "?", n_ok, n_fail
  ))
  if (!is.null(x$log)) print(x$log)
  invisible(x)
}


# ── besd_fitted_probs() ──────────────────────────────────────────────────────────

#' Extract fitted probabilities from a BeSD regression model
#'
#' Returns fitted (predicted) probabilities from a [besd_regress()] output.
#' For Bayesian models, `n_sample` draws from the posterior distribution of
#' predicted probabilities are returned, enabling full uncertainty propagation
#' through downstream steps such as [besd_poststratify()]. For frequentist
#' models, a single point-estimate prediction is returned.
#'
#' @param fit A `besd_fit` or `besd_fit_by_country` object from
#'   [besd_regress()].
#' @param newdata `NULL` to predict on the training data, or a
#'   `besd_poststrat_frame` object from [besd_poststrat_frame()] to predict
#'   on new data. Any other input will error with a clear message.
#' @param n_sample Integer. Number of posterior draws for Bayesian models.
#'   For large datasets, keep this small (e.g. 50) to limit memory use.
#'   Ignored for frequentist models.
#'
#' @return A `besd_fitted` object containing:
#' \describe{
#'   \item{`draws`}{Named list, one element per outcome. Binary: matrix
#'     `[n_sample x n_obs]` (Bayes) or `[1 x n_obs]` (frequentist). Ordinal:
#'     3-D array `[n_sample x n_obs x n_categories]`.}
#'   \item{`meta`}{Model metadata: `engine`, `scope`, `outcomes`, `y_type`,
#'     `n_sample`, and `categories` (ordinal labels or `NULL` for binary).}
#'   \item{`row_ids`}{Tibble with the country column and `.row_id` aligned to
#'     the rows of `newdata` (or the training data if `newdata = NULL`).}
#' }
#'
#' @seealso [besd_poststrat_frame()], [besd_poststratify()]
#' @export
besd_fitted_probs <- function(fit, newdata = NULL, n_sample = 50L) {

  .assert_besd_fit(fit)

  if (!is.null(newdata)) .assert_besd_poststrat_frame(newdata)

  prep     <- fit$prep
  engine   <- prep$engine
  y_types  <- fit$meta$y_types  %||% list()
  outcomes <- fit$meta$outcomes %||% character()
  n_sample <- as.integer(n_sample)
  encoded  <- newdata %||% prep$df

  draws_list <- list()
  cats_list  <- list()

  if (inherits(fit, "besd_fit")) {
    for (yy in outcomes) {
      m <- fit$fits[[yy]]
      if (is.null(m)) next
      ytype            <- y_types[[yy]] %||% "binary"
      result           <- .fitted_probs_one(m, encoded, engine, ytype, n_sample)
      draws_list[[yy]] <- result$draws
      cats_list[[yy]]  <- result$categories
    }
  } else {
    result     <- .fitted_probs_by_country(fit, encoded, prep,
                                            engine, y_types, outcomes, n_sample)
    draws_list <- result$draws
    cats_list  <- result$cats
  }

  country_col <- prep$country_col
  row_ids     <- tibble::tibble(
    .country = as.character(encoded[[country_col]]),
    .row_id  = seq_len(nrow(encoded))
  )
  names(row_ids)[[1L]] <- country_col

  structure(
    list(
      draws  = draws_list,
      meta   = list(
        engine     = engine,
        scope      = prep$scope,
        outcomes   = outcomes,
        # first outcome; kept for besd_poststratify compat
        y_type     = (y_types[[outcomes[[1L]]]] %||% "binary"),
        y_types    = y_types,
        n_sample   = n_sample,
        categories = cats_list,
        # Dictionaries carried through so besd_poststratify() can emit
        # summary()-compatible `item_type` / `response` columns and attach
        # dictionary attributes without needing the original besd_data object.
        besd_dict  = prep$dict,
        dem_dict   = prep$dem_dict,
        outcome_label_map  = fit$meta$outcome_label_map  %||% list(),
        outcome_parent_map = fit$meta$outcome_parent_map %||% list()
      ),
      row_ids = row_ids
    ),
    class = "besd_fitted"
  )
}


# Route predictions for besd_fit_by_country: each row in encoded is sent to the
# model for its country. Results fill a matrix/array in newdata row order.
# Rows with no matching country model receive NA.
.fitted_probs_by_country <- function(fit, encoded, prep,
                                     engine, y_types, outcomes, n_sample) {
  country_col <- prep$country_col
  countries   <- as.character(encoded[[country_col]])
  n_obs       <- nrow(encoded)
  n_draws     <- if (engine == "frequentist") 1L else n_sample

  draws_out <- list()
  cats_out  <- list()

  for (yy in outcomes) {
    y_type <- y_types[[yy]] %||% "binary"
    mat    <- NULL  # initialised on first successful prediction
    cats   <- NULL

    for (cc in prep$countries) {
      m   <- (fit$fits[[cc]] %||% list())[[yy]]
      idx <- which(countries == cc)
      if (is.null(m) || !length(idx)) next

      result <- .fitted_probs_one(
        m, encoded[idx, , drop = FALSE], engine, y_type, n_sample
      )

      if (is.null(mat)) {
        cats <- result$categories
        mat  <- if (y_type == "binary") {
          matrix(NA_real_, nrow = n_draws, ncol = n_obs)
        } else {
          array(NA_real_, dim = c(n_draws, n_obs, dim(result$draws)[[3L]]))
        }
      }

      if (y_type == "binary") mat[, idx]   <- result$draws
      else                    mat[, idx, ] <- result$draws
    }

    if (is.null(mat)) next
    draws_out[[yy]] <- mat
    cats_out[[yy]]  <- cats
  }

  list(draws = draws_out, cats = cats_out)
}


# Extract fitted probabilities from a single model object.
# Returns list(draws, categories) where draws is:
#   binary:  matrix [n_draws x n_obs]
#   ordinal: array  [n_draws x n_obs x n_k]
.fitted_probs_one <- function(model, newdata, engine, y_type, n_sample) {

  if (engine == "frequentist") {

    if (y_type == "binary") {
      preds <- stats::predict(model, newdata = newdata, type = "response",
                              allow.new.levels = TRUE)
      return(list(draws      = matrix(as.numeric(preds), nrow = 1L),
                  categories = NULL))
    }

    # Ordinal (clm / clmm): predict() returns list with $fit [n_obs x n_k]
    .require_pkg("ordinal", "for ordinal fitted probabilities")
    pred_obj <- stats::predict(model, newdata = newdata, type = "prob")
    mat      <- if (is.list(pred_obj)) pred_obj$fit else as.matrix(pred_obj)
    cats     <- colnames(mat)
    arr      <- array(as.numeric(mat),
                      dim      = c(1L, nrow(mat), ncol(mat)),
                      dimnames = list(NULL, NULL, cats))
    return(list(draws = arr, categories = cats))
  }

  # Bayesian (brmsfit): posterior_epred() returns
  #   binary:  [n_draws x n_obs]
  #   ordinal: [n_draws x n_obs x n_k]
  .require_pkg("brms", "for Bayesian fitted probabilities")
  draws_arr <- brms::posterior_epred(
    object           = model,
    newdata          = newdata,
    ndraws           = n_sample,
    allow_new_levels = TRUE,
    re_formula       = NULL
  )

  if (length(dim(draws_arr)) == 2L)
    return(list(draws = draws_arr, categories = NULL))

  list(draws = draws_arr, categories = dimnames(draws_arr)[[3L]])
}


#' @export
print.besd_fitted <- function(x, ...) {
  engine   <- x$meta$engine
  y_type   <- x$meta$y_type
  outcomes <- x$meta$outcomes
  n_sample <- x$meta$n_sample
  first    <- x$draws[[outcomes[[1L]]]]
  n_obs    <- if (length(dim(first)) >= 2L) dim(first)[[2L]] else length(first)

  cat(sprintf(
    "<besd_fitted>  engine: %s | y_type: %s | outcomes: %d | obs: %d",
    engine, y_type, length(outcomes), n_obs
  ))
  if (engine == "bayes") cat(sprintf(" | draws: %d", n_sample))
  cat("\n")
  if (length(outcomes)) cat("Outcomes:", paste(outcomes, collapse = ", "), "\n")
  invisible(x)
}
