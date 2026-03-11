# ── besd_poststratify() ────────────────────────────────────────────────────────

#' Poststratify BeSD fitted probabilities to a target population
#'
#' Applies population counts from a [besd_poststrat_frame()] to fitted
#' probabilities from [besd_fitted_probs()] to produce poststratified
#' (population-representative) estimates. For Bayesian models, the full
#' posterior distribution is propagated through the weighting step, yielding
#' median estimates and credible intervals. For frequentist models, a single
#' weighted point estimate is returned with no uncertainty quantification.
#'
#' @param fitted A `besd_fitted` object from [besd_fitted_probs()].
#' @param poststrat_frame A `besd_poststrat_frame` object from
#'   [besd_poststrat_frame()]. Row order must correspond to the `newdata`
#'   used when calling [besd_fitted_probs()].
#' @param by Optional character vector of column names in `poststrat_frame`
#'   to stratify estimates by (e.g. `"dem_gen"` to return separate
#'   poststratified estimates per gender within each country). Columns must
#'   be present in `poststrat_frame`.
#' @param conf_level Numeric. Credible interval width for Bayesian models.
#'   Default `0.95`. Ignored for frequentist models.
#' @param overall Logical. If `TRUE`, an additional set of national-level
#'   poststratified estimates is appended, pooling across all countries using
#'   globally-normalised population weights. When `by` is also specified,
#'   national estimates are returned separately for each `by` subgroup.
#'   Default `FALSE`.
#' @param overall_label String used to label the country column in national
#'   output rows. Default `"Overall"`.
#'
#' @section Sub-national use and context-predictor limitation:
#' The supported use of poststratification in rbesd is **sub-national
#' (admin1-level) analysis within a single country**, where the "country"
#' grouping column corresponds to the first administrative unit (e.g. region
#' or state). Multi-country poststratification is not supported when the
#' upstream model contains context-specific predictors; [besd_poststrat_frame()]
#' will raise an error in that case. See its documentation for details.
#'
#' @return A `besd_poststrat` tibble with columns: grouping columns (country
#'   and any `by` variables with human-readable labels), `outcome`, `estimate`,
#'   `lower`, `upper`. For ordinal outcomes a `category` column is also
#'   included. `lower` and `upper` are `NA` for frequentist models. When
#'   `overall = TRUE`, national rows are appended with the country column set
#'   to `overall_label`.
#'
#' @seealso [besd_poststrat_frame()], [besd_fitted_probs()]
#' @export
besd_poststratify <- function(fitted, poststrat_frame, by = NULL,
                              conf_level = 0.95, overall = FALSE,
                              overall_label = "Overall") {

  .assert_besd_fitted(fitted)
  .assert_besd_poststrat_frame(poststrat_frame)
  .assert_is_scalar_number(conf_level, "conf_level")

  if (!is.null(by)) {
    missing_by <- setdiff(by, names(poststrat_frame))
    if (length(missing_by))
      .stopf(
        "Column(s) in `by` not found in `poststrat_frame`: %s",
        .pastec(missing_by)
      )
  }

  pop_col     <- attr(poststrat_frame, "pop_col")
  country_col <- attr(poststrat_frame, "country_col")
  level_map   <- attr(poststrat_frame, "level_map")
  pop         <- poststrat_frame[[pop_col]]
  engine      <- fitted$meta$engine
  y_type      <- fitted$meta$y_type
  outcomes    <- fitted$meta$outcomes

  # Country-level pass: group by country + by
  groups <- .ps_group_indices(poststrat_frame, c(country_col, by))
  rows   <- .ps_run_groups(groups, outcomes, fitted, pop, level_map,
                           country_col, NULL, conf_level, engine, y_type)

  # National pass: group by by only (or single group if by is NULL)
  if (overall) {
    national_groups <- .ps_group_indices(poststrat_frame, by)
    rows <- c(rows,
              .ps_run_groups(national_groups, outcomes, fitted, pop, level_map,
                             country_col, overall_label, conf_level, engine,
                             y_type))
  }

  if (!length(rows)) {
    out <- tibble::tibble(
      outcome  = character(),
      category = character(),
      estimate = numeric(),
      lower    = numeric(),
      upper    = numeric()
    )
    return(structure(out, class = c("besd_poststrat", class(out))))
  }

  out <- dplyr::bind_rows(rows)
  if (y_type == "binary") out$category <- NULL
  structure(out, class = c("besd_poststrat", class(out)))
}


# ── Internal helpers ───────────────────────────────────────────────────────────

# Build a list of groups from a data frame. Each group is a list with:
#   idx:    integer row indices belonging to this group
#   values: single-row data frame of the group's column values
# Groups are identified by pasting all group column values into a key string.
# When group_cols is empty (e.g. national pass with no `by`), all rows are
# returned as a single group with a 0-column values data frame.
.ps_group_indices <- function(df, group_cols) {
  if (!length(group_cols))
    return(list(list(idx = seq_len(nrow(df)),
                     values = df[1L, character(0), drop = FALSE])))

  group_df <- df[, group_cols, drop = FALSE]
  keys     <- do.call(
    paste,
    c(lapply(group_cols, function(col) as.character(df[[col]])), list(sep = "\r"))
  )
  lapply(unique(keys), function(k) {
    idx <- which(keys == k)
    list(idx = idx, values = group_df[idx[[1L]], , drop = FALSE])
  })
}


# Run the poststratification weighting loop over a pre-computed set of groups.
# Returns a list of row tibbles suitable for dplyr::bind_rows().
# overall_label: if non-NULL, the country column is added to group_vals with
#   this value (and placed first), marking rows as national-level estimates.
.ps_run_groups <- function(groups, outcomes, fitted, pop, level_map,
                           country_col, overall_label, conf_level, engine,
                           y_type) {
  rows <- list()

  for (yy in outcomes) {
    draws_arr <- fitted$draws[[yy]]
    if (is.null(draws_arr)) next
    cats <- fitted$meta$categories[[yy]]

    for (grp in groups) {
      idx <- grp$idx
      w_c <- pop[idx]
      if (!sum(w_c, na.rm = TRUE) > 0) next
      w_c <- w_c / sum(w_c)  # normalise to sum to 1 within group

      group_vals <- .ps_decode_groups(grp$values, level_map)
      if (!is.null(overall_label)) {
        group_vals[[country_col]] <- overall_label
        group_vals <- group_vals[, c(country_col, setdiff(names(group_vals), country_col)),
                                 drop = FALSE]
      }

      if (y_type == "binary") {
        ps_draws <- .ps_weighted_draws(draws_arr[, idx, drop = FALSE], w_c)
        summ     <- .ps_summarise_draws(ps_draws, conf_level, engine)
        rows[[length(rows) + 1L]] <- .ps_build_row(
          yy, group_vals, NA_character_, summ$estimate, summ$lower, summ$upper
        )
      } else {
        for (k in seq_along(cats)) {
          slice    <- draws_arr[, idx, k, drop = FALSE]
          draws_k  <- matrix(as.numeric(slice), nrow = dim(draws_arr)[[1L]])
          ps_draws <- .ps_weighted_draws(draws_k, w_c)
          summ     <- .ps_summarise_draws(ps_draws, conf_level, engine)
          rows[[length(rows) + 1L]] <- .ps_build_row(
            yy, group_vals, cats[[k]], summ$estimate, summ$lower, summ$upper
          )
        }
      }
    }
  }

  rows
}


# Compute the weighted sum of posterior draws across poststratification cells.
# draws_mat: [n_draws x n_cells]; weights: normalised to sum to 1.
# Returns a numeric vector of length n_draws.
.ps_weighted_draws <- function(draws_mat, weights) {
  as.numeric(draws_mat %*% weights)
}


# Summarise a vector of poststratified draws to a point estimate and interval.
# For frequentist models the single draw is returned as the estimate directly.
.ps_summarise_draws <- function(ps_draws, conf_level, engine) {
  if (engine == "frequentist")
    return(list(estimate = ps_draws[[1L]], lower = NA_real_, upper = NA_real_))

  probs <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)
  list(
    estimate = stats::median(ps_draws),
    lower    = stats::quantile(ps_draws, probs[[1L]], names = FALSE),
    upper    = stats::quantile(ps_draws, probs[[2L]], names = FALSE)
  )
}


# Decode opaque factor codes (__01, __02, ...) back to human-readable labels
# in a group-values data frame using the level_map from besd_poststrat_frame.
# Columns without a level_map entry (e.g. country, numeric) are left as-is.
.ps_decode_groups <- function(group_vals, level_map) {
  for (nm in names(group_vals)) {
    mp <- level_map[[nm]]
    if (is.null(mp)) next
    label_of         <- stats::setNames(mp$label, mp$code)
    group_vals[[nm]] <- unname(label_of[as.character(group_vals[[nm]])])
  }
  group_vals
}


# Build a single output row as a tibble, with group columns placed first
# followed by outcome, category, and the three estimate columns.
.ps_build_row <- function(outcome, group_vals, category, estimate, lower, upper) {
  dplyr::bind_cols(
    group_vals,
    tibble::tibble(
      outcome  = outcome,
      category = category,
      estimate = as.numeric(estimate),
      lower    = as.numeric(lower),
      upper    = as.numeric(upper)
    )
  )
}


#' @export
print.besd_poststrat <- function(x, ...) {
  cat(sprintf("<besd_poststrat>  %d rows\n", nrow(x)))
  NextMethod()
  invisible(x)
}
