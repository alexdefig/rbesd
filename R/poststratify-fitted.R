# ── besd_fitted_probs() ────────────────────────────────────────────────────────
#
# Step 2 of the MrP pipeline:
#   besd_poststrat_frame()  build the census cells
#   besd_fitted_probs()     evaluate the model at those cells   <- this file
#   besd_poststratify()     weight the cell probabilities by population
#
# Poststratification is Bayesian-only (see .assert_mrp_engine), so prediction
# here goes through brms::posterior_epred() exclusively. There is deliberately
# no frequentist branch: a point estimate per cell cannot propagate uncertainty
# through the weighting step.

#' Evaluate a fitted BeSD model at poststratification cells
#'
#' Returns posterior draws of the predicted probabilities for every cell of a
#' [besd_poststrat_frame()]. This is the second step of the MrP pipeline,
#' sitting between [besd_poststrat_frame()] and [besd_poststratify()]: the
#' cells generally include covariate combinations that are rare or absent in
#' the survey, and this is where the model transfers information to them.
#'
#' `n_sample` draws from the posterior distribution are returned, so that
#' [besd_poststratify()] can propagate the full distribution through the
#' weighting step and report credible intervals.
#'
#' Poststratification requires a Bayesian fit. A frequentist model yields a
#' single point estimate per cell, so the poststratified estimates would carry
#' no uncertainty; this function errors rather than produce them. Refit with
#' `besd_regress(..., engine = "bayes")`.
#'
#' @param fit A `besd_fit` or `besd_fit_by_country` object from
#'   [besd_regress()], fitted with `engine = "bayes"`.
#' @param newdata A `besd_poststrat_frame` object from
#'   [besd_poststrat_frame()]. Required. Any other input will error with a
#'   clear message.
#' @param n_sample Integer. Number of posterior draws to extract. For large
#'   frames, keep this small (e.g. 50) to limit memory use.
#'
#' @return A `besd_fitted` object containing:
#' \describe{
#'   \item{`draws`}{Named list, one element per outcome. Binary: matrix
#'     `[n_sample x n_cells]`. Ordinal: 3-D array
#'     `[n_sample x n_cells x n_categories]`.}
#'   \item{`meta`}{Model metadata: `engine`, `scope`, `outcomes`, `y_type`,
#'     `y_types`, `n_sample`, and `categories` (ordinal labels or `NULL` for
#'     binary).}
#'   \item{`row_ids`}{Tibble with the country column and `.row_id` aligned to
#'     the rows of `newdata`.}
#' }
#'
#' @seealso [besd_poststrat_frame()] for the preceding step,
#'   [besd_poststratify()] for the following one, and [besd_regress()] for
#'   fitting the model.
#' @export
besd_fitted_probs <- function(fit, newdata, n_sample = 50L) {

  .assert_besd_fit(fit)
  if (missing(newdata))
    .stopf(
      paste0(
        "`newdata` is required: supply the `besd_poststrat_frame()` whose ",
        "cells the model should be evaluated at. `besd_fitted_probs()` is a ",
        "step of the MrP pipeline and does not predict on the training data."
      )
    )
  .assert_besd_poststrat_frame(newdata)
  .assert_mrp_engine(fit$prep$engine, "besd_fitted_probs()")

  prep     <- fit$prep
  y_types  <- fit$meta$y_types  %||% list()
  outcomes <- fit$meta$outcomes %||% character()
  n_sample <- as.integer(n_sample)

  draws_list <- list()
  cats_list  <- list()

  if (inherits(fit, "besd_fit")) {
    for (yy in outcomes) {
      m <- fit$fits[[yy]]
      if (is.null(m)) next
      result           <- .fitted_probs_one(m, newdata, n_sample)
      draws_list[[yy]] <- result$draws
      cats_list[[yy]]  <- result$categories
    }
  } else {
    result     <- .fitted_probs_by_country(fit, newdata, prep, y_types,
                                           outcomes, n_sample)
    draws_list <- result$draws
    cats_list  <- result$cats
  }

  country_col <- prep$country_col
  row_ids     <- tibble::tibble(
    .country = as.character(newdata[[country_col]]),
    .row_id  = seq_len(nrow(newdata))
  )
  names(row_ids)[[1L]] <- country_col

  structure(
    list(
      draws  = draws_list,
      meta   = list(
        engine     = prep$engine,
        scope      = prep$scope,
        outcomes   = outcomes,
        # first outcome; kept for backward compatibility. besd_poststratify()
        # reads the per-outcome `y_types` instead: a single fit can mix binary
        # and ordinal outcomes, whose draws have different dimensionality.
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
.fitted_probs_by_country <- function(fit, encoded, prep, y_types, outcomes,
                                     n_sample) {
  country_col <- prep$country_col
  countries   <- as.character(encoded[[country_col]])
  n_obs       <- nrow(encoded)

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

      result <- .fitted_probs_one(m, encoded[idx, , drop = FALSE], n_sample)

      if (is.null(mat)) {
        cats <- result$categories
        mat  <- if (y_type == "binary") {
          matrix(NA_real_, nrow = n_sample, ncol = n_obs)
        } else {
          array(NA_real_, dim = c(n_sample, n_obs, dim(result$draws)[[3L]]))
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


# Extract posterior draws of fitted probabilities from a single brmsfit.
# Returns list(draws, categories) where draws is:
#   binary:  matrix [n_draws x n_obs]
#   ordinal: array  [n_draws x n_obs x n_k]
.fitted_probs_one <- function(model, newdata, n_sample) {
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
    "<besd_fitted>  engine: %s | y_type: %s | outcomes: %d | cells: %d",
    engine, y_type, length(outcomes), n_obs
  ))
  if (engine == "bayes") cat(sprintf(" | draws: %d", n_sample))
  cat("\n")
  if (length(outcomes)) cat("Outcomes:", paste(outcomes, collapse = ", "), "\n")
  invisible(x)
}
