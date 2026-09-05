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
#' @param post_probs Logical. If `TRUE`, the return value becomes a named list
#'   with two elements: `estimates` (the usual `besd_poststrat` tibble) and
#'   `post_probs` (a tibble with the same grouping columns, `item_id` and
#'   `response`, plus a `draws` list-column containing the full vector of
#'   poststratified posterior draws for each row). The `post_probs` tibble
#'   respects the same `by` and `overall` grouping as `estimates`. Default
#'   `FALSE`.
#' @param combine_top Logical. If `TRUE`, collapse the top-box levels
#'   (`toplevs` from the item dictionary) into a single combined response per
#'   item, mirroring `summary(x, combine_top = TRUE)`. For ordinal and
#'   categorical outcomes the posterior probability mass of the top-box
#'   categories is summed **within each draw** before summarising, so credible
#'   intervals are correct for the combined quantity. Binary outcomes are
#'   already reported as their top-box label and are unaffected. Multichoice
#'   items are dropped, as they are by [summary.besd_data()]. Requires
#'   non-empty `toplevs` in the dictionary for every ordinal/categorical
#'   outcome. Default `FALSE`.
#'
#' @section Sub-national use and context-predictor limitation:
#' The supported use of poststratification in rbesd is **sub-national
#' (admin1-level) analysis within a single country**, where the "country"
#' grouping column corresponds to the first administrative unit (e.g. region
#' or state). Multi-country poststratification is not supported when the
#' upstream model contains context-specific predictors; [besd_poststrat_frame()]
#' will raise an error in that case. See its documentation for details.
#'
#' @section Output contract:
#'   The returned table follows the same column contract as
#'   [summary.besd_data()], so poststratified estimates are drop-in compatible
#'   with `plot_besd_bars()`, `plot_besd_spider()`, `plot_besd_ranked()` and the
#'   BeSD Explorer. Specifically:
#'   \itemize{
#'     \item the geography column is named `country` regardless of what
#'       `country_col` was called in [as_besd()];
#'     \item items are reported as `item_id` / `item_type` / `response` rather
#'       than `outcome` / `category`. Binary outcomes take the dictionary
#'       top-box label (identical to `summary(combine_top = TRUE)`), and
#'       multichoice sub-outcomes are folded back onto their parent `item_id`
#'       with one row per level. With `combine_top = TRUE`, ordinal and
#'       categorical items collapse to a single top-box row per item, labelled
#'       the same way;
#'     \item estimates are `pct` / `lcl` / `ucl` on the 0--100 scale;
#'     \item `n` is `NA` (poststratified estimates carry no survey sample size)
#'       and `estimator` is `"mrp"`.
#'   }
#'   The object carries both the `besd_poststrat` and `besd_summary_tbl`
#'   classes, plus `besd_dict` / `dem_dict` attributes.
#'
#' @return When `post_probs = FALSE` (default): a `besd_poststrat` tibble with
#'   columns `country`, any `by` variables (with human-readable labels),
#'   `item_id`, `item_type`, `response`, `n`, `pct`, `lcl`, `ucl`, `estimator`.
#'   `lcl` and `ucl` are `NA` for frequentist models. When `overall = TRUE`,
#'   national rows are appended with the `country` column set to
#'   `overall_label`.
#'
#'   When `post_probs = TRUE`: a named list with:
#'   \describe{
#'     \item{`estimates`}{The `besd_poststrat` tibble described above.}
#'     \item{`post_probs`}{A tibble with the same grouping columns, `item_id`
#'       and `response`, plus a `draws` list-column. Each element of `draws` is
#'       a numeric vector of length `n_draws` containing the full distribution
#'       of poststratified posterior probabilities for that group × item ×
#'       response. Draws remain on the 0--1 probability scale (unlike `pct`),
#'       because they are intended for further arithmetic such as collapsing
#'       top-box categories before summarising.}
#'   }
#'
#' @seealso [besd_poststrat_frame()], [besd_fitted_probs()]
#' @export
besd_poststratify <- function(fitted, poststrat_frame, by = NULL,
                              conf_level = 0.95, overall = FALSE,
                              overall_label = "Overall", post_probs = FALSE,
                              combine_top = FALSE) {

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
  outcomes    <- fitted$meta$outcomes

  # Per-outcome types. `meta$y_type` is only the first outcome's type; using it
  # for every outcome mis-shapes the draws whenever one fit mixes binary and
  # ordinal outcomes. Fall back to it only for outcomes missing from `y_types`.
  y_types <- stats::setNames(
    lapply(outcomes, function(yy) {
      (fitted$meta$y_types %||% list())[[yy]] %||% fitted$meta$y_type %||% "binary"
    }),
    outcomes
  )

  # Dictionaries carried through besd_fitted_probs(); used to label binary
  # outcomes and to populate item_type, so output matches summary.besd_data().
  besd_dict  <- fitted$meta$besd_dict
  dem_dict   <- fitted$meta$dem_dict
  label_map  <- fitted$meta$outcome_label_map  %||% list()
  parent_map <- fitted$meta$outcome_parent_map %||% list()
  item_meta  <- .ps_item_meta(outcomes, besd_dict, label_map, parent_map)

  if (isTRUE(combine_top)) {
    # Multichoice sub-outcomes have no top box; summary.besd_data() drops the
    # item entirely under combine_top, so do the same here.
    outcomes <- outcomes[vapply(outcomes,
                                function(yy) is.null(label_map[[yy]]),
                                logical(1))]
    .ps_assert_toplevs(outcomes, y_types, item_meta)
    if (!length(outcomes))
      .stopf("`combine_top = TRUE` left no outcomes to poststratify (all multichoice).")
  }

  # Country-level pass: group by country + by
  groups <- .ps_group_indices(poststrat_frame, c(country_col, by))
  result <- .ps_run_groups(groups, outcomes, fitted, pop, level_map,
                           country_col, NULL, conf_level, engine, y_types,
                           post_probs, item_meta, combine_top)
  rows      <- result$rows
  draw_rows <- result$draw_rows

  # National pass: group by by only (or single group if by is NULL)
  if (overall) {
    national_groups <- .ps_group_indices(poststrat_frame, by)
    nat_result <- .ps_run_groups(national_groups, outcomes, fitted, pop,
                                 level_map, country_col, overall_label,
                                 conf_level, engine, y_types, post_probs,
                                 item_meta, combine_top)
    rows      <- c(rows, nat_result$rows)
    draw_rows <- c(draw_rows, nat_result$draw_rows)
  }

  if (!length(rows)) {
    estimates <- .ps_finalise(
      tibble::tibble(country = character(), item_id = character(),
                     item_type = character(), response = character(),
                     n = integer(), pct = numeric(), lcl = numeric(),
                     ucl = numeric(), estimator = character()),
      besd_dict, dem_dict
    )
    if (!post_probs) return(estimates)
    return(list(estimates = estimates,
                post_probs = tibble::tibble(country = character(),
                                            item_id = character(),
                                            response = character(),
                                            draws = list())))
  }

  estimates <- .ps_finalise(dplyr::bind_rows(rows), besd_dict, dem_dict)

  if (!post_probs) return(estimates)

  list(estimates = estimates, post_probs = dplyr::bind_rows(draw_rows))
}


# Resolve per-outcome metadata once, up front. Returns a named list keyed by
# model outcome name, each element giving the item_id, item_type and response
# label to report. Multichoice outcomes were expanded by besd_regress() into
# one binary sub-outcome per level; those are folded back onto the parent
# item_id with the level as the response, so the result matches the layout of
# summary.besd_data(). Plain binary outcomes take the dictionary top-box label,
# which is identical to what summary(combine_top = TRUE) emits.
.ps_item_meta <- function(outcomes, besd_dict, label_map, parent_map) {
  stats::setNames(lapply(outcomes, function(yy) {
    parent <- parent_map[[yy]] %||% yy
    idx    <- if (!is.null(besd_dict)) match(parent, besd_dict$item_id) else NA_integer_

    item_type <- if (!is.na(idx)) besd_dict$item_type[[idx]] else NA_character_

    tl <- if (!is.na(idx)) besd_dict$toplevs[[idx]] else NULL
    tl <- if (is.character(tl) && length(tl) && !any(is.na(tl))) tl else NULL

    levs <- if (!is.na(idx) && "levels" %in% names(besd_dict)) {
      as.character(besd_dict$levels[[idx]])
    } else NULL

    response <- if (!is.null(label_map[[yy]])) {
      # multichoice sub-outcome: the level it represents
      label_map[[yy]]
    } else if (!is.null(tl)) {
      paste(tl, collapse = ", ")
    } else {
      parent
    }

    list(item_id = parent, item_type = item_type %||% NA_character_,
         response = response, toplevs = tl, levels = levs)
  }), outcomes)
}


# Guard for combine_top: every non-binary outcome must have usable top-box
# levels in the dictionary. Mirrors the check summary.besd_data() applies.
.ps_assert_toplevs <- function(outcomes, y_types, item_meta) {
  for (yy in outcomes) {
    if ((y_types[[yy]] %||% "binary") == "binary") next
    if (is.null(item_meta[[yy]]$toplevs))
      .stopf("`combine_top = TRUE` requires non-empty `toplevs` in dict for `%s`.",
             item_meta[[yy]]$item_id %||% yy)
  }
  invisible(TRUE)
}


# Resolve which slices of an ordinal draws array make up the top box. Matches
# the model's category labels against the dictionary `toplevs` by label, and
# falls back to positional matching against the dictionary `levels` for engines
# that return positional labels ("1", "2", ...) rather than the factor levels.
.ps_top_indices <- function(cats, im, item_id) {
  k <- which(as.character(cats) %in% im$toplevs)
  if (!length(k) && !is.null(im$levels)) {
    pos <- match(im$toplevs, im$levels)
    pos <- pos[!is.na(pos)]
    k   <- pos[pos <= length(cats)]
  }
  if (!length(k))
    .stopf(paste0("`combine_top = TRUE`: could not match `toplevs` (%s) to the ",
                  "fitted response categories (%s) for `%s`."),
           .pastec(im$toplevs), .pastec(as.character(cats)), item_id)
  sort(unique(k))
}


# Apply the besd_summary_tbl contract to an assembled poststratification table:
# stable column order, both classes, and the dictionary attributes that
# plot_besd_*() and the Explorer read.
.ps_finalise <- function(x, besd_dict, dem_dict) {
  lead <- c("country", "item_id", "item_type", "response")
  lead <- intersect(lead, names(x))
  tail_cols <- c("n", "pct", "lcl", "ucl", "estimator")
  mid  <- setdiff(names(x), c(lead, tail_cols))   # any `by` grouping columns
  x    <- x[, c(lead, mid, intersect(tail_cols, names(x))), drop = FALSE]

  # Join the item dictionary metadata columns that summary.besd_data() carries.
  # plot_besd_bars() / plot_besd_spider() read these directly off the table, so
  # the dictionary attribute alone is not enough for drop-in compatibility.
  meta_cols <- c("domain", "question", "question_short")
  if (!is.null(besd_dict) && all(meta_cols %in% names(besd_dict))) {
    idx <- match(x$item_id, besd_dict$item_id)
    for (nm in meta_cols) x[[nm]] <- besd_dict[[nm]][idx]
  }

  x <- structure(x, class = unique(c("besd_poststrat", "besd_summary_tbl",
                                     class(tibble::as_tibble(x)))))
  attr(x, "besd_dict") <- besd_dict
  attr(x, "dem_dict")  <- dem_dict
  x
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
# Returns list(rows, draw_rows):
#   rows:      summary tibble rows for dplyr::bind_rows()
#   draw_rows: posterior draw tibble rows (populated only when post_probs TRUE)
# overall_label: if non-NULL, the country column is added to group_vals with
#   this value (and placed first), marking rows as national-level estimates.
# y_types: named list of per-outcome types. It must be consulted per outcome,
#   not once for the whole call: a single besd_regress() fit can mix binary and
#   ordinal outcomes, whose draws have different dimensionality.
.ps_run_groups <- function(groups, outcomes, fitted, pop, level_map,
                           country_col, overall_label, conf_level, engine,
                           y_types, post_probs, item_meta, combine_top = FALSE) {
  rows      <- list()
  draw_rows <- list()

  for (yy in outcomes) {
    draws_arr <- fitted$draws[[yy]]
    if (is.null(draws_arr)) next
    y_type <- y_types[[yy]] %||% "binary"
    cats <- fitted$meta$categories[[yy]]
    im   <- item_meta[[yy]] %||% list(item_id = yy, item_type = NA_character_,
                                      response = yy)

    # Resolve the top-box slices once per outcome, not per group.
    k_top <- if (isTRUE(combine_top) && y_type != "binary") {
      .ps_top_indices(cats, im, yy)
    } else NULL

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
      # Report the geography under the literal name `country`, matching
      # summary.besd_data(), regardless of what country_col was called.
      if (country_col %in% names(group_vals))
        names(group_vals)[match(country_col, names(group_vals))] <- "country"

      if (y_type == "binary") {
        ps_draws <- .ps_weighted_draws(draws_arr[, idx, drop = FALSE], w_c)
        summ     <- .ps_summarise_draws(ps_draws, conf_level, engine)
        rows[[length(rows) + 1L]] <- .ps_build_row(
          im, group_vals, im$response, summ$estimate, summ$lower, summ$upper
        )
        if (post_probs)
          draw_rows[[length(draw_rows) + 1L]] <- .ps_build_draw_row(
            im, group_vals, im$response, ps_draws
          )
      } else if (!is.null(k_top)) {
        # Top box: sum the probability mass of the top-box categories *within
        # each draw* before summarising. Summarising each category separately
        # and adding the results would be wrong, as medians and quantiles are
        # not additive.
        draws_k  <- .ps_collapse_cats(draws_arr, idx, k_top)
        ps_draws <- .ps_weighted_draws(draws_k, w_c)
        summ     <- .ps_summarise_draws(ps_draws, conf_level, engine)
        rows[[length(rows) + 1L]] <- .ps_build_row(
          im, group_vals, im$response, summ$estimate, summ$lower, summ$upper
        )
        if (post_probs)
          draw_rows[[length(draw_rows) + 1L]] <- .ps_build_draw_row(
            im, group_vals, im$response, ps_draws
          )
      } else {
        for (k in seq_along(cats)) {
          draws_k  <- .ps_collapse_cats(draws_arr, idx, k)
          ps_draws <- .ps_weighted_draws(draws_k, w_c)
          summ     <- .ps_summarise_draws(ps_draws, conf_level, engine)
          rows[[length(rows) + 1L]] <- .ps_build_row(
            im, group_vals, cats[[k]], summ$estimate, summ$lower, summ$upper
          )
          if (post_probs)
            draw_rows[[length(draw_rows) + 1L]] <- .ps_build_draw_row(
              im, group_vals, cats[[k]], ps_draws
            )
        }
      }
    }
  }

  list(rows = rows, draw_rows = draw_rows)
}


# Slice an ordinal draws array [n_draws x n_obs x n_k] down to the given cells
# and categories, summing across the categories within each draw x cell.
# Returns a [n_draws x length(idx)] matrix. With a single category this is just
# the extracted slice.
.ps_collapse_cats <- function(draws_arr, idx, k) {
  n_draws <- dim(draws_arr)[[1L]]
  if (length(k) == 1L)
    return(matrix(as.numeric(draws_arr[, idx, k, drop = FALSE]), nrow = n_draws))
  out <- matrix(0, nrow = n_draws, ncol = length(idx))
  for (j in seq_along(k))
    out <- out + matrix(as.numeric(draws_arr[, idx, k[[j]], drop = FALSE]),
                        nrow = n_draws)
  out
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


# Build a single summary output row as a tibble, matching the column contract
# of summary.besd_data(): group columns first, then item_id / item_type /
# response, then n / pct / lcl / ucl. Estimates are rescaled from probabilities
# to percentages (0-100) to match the survey path. `n` is NA because
# poststratified estimates have no survey sample size attached.
.ps_build_row <- function(item_meta, group_vals, response, estimate, lower, upper) {
  dplyr::bind_cols(
    group_vals,
    tibble::tibble(
      item_id   = item_meta$item_id,
      item_type = item_meta$item_type,
      response  = response,
      n         = NA_integer_,
      pct       = 100 * as.numeric(estimate),
      lcl       = 100 * as.numeric(lower),
      ucl       = 100 * as.numeric(upper),
      estimator = "mrp"
    )
  )
}


# Build a single posterior-draws output row as a tibble. Mirrors .ps_build_row
# but stores the full ps_draws vector in a list-column instead of summarising.
# Draws stay on the 0-1 probability scale: they are intended for further
# arithmetic (e.g. collapsing top-box categories) before any rescaling.
.ps_build_draw_row <- function(item_meta, group_vals, response, ps_draws) {
  dplyr::bind_cols(
    group_vals,
    tibble::tibble(
      item_id  = item_meta$item_id,
      response = response,
      draws    = list(as.numeric(ps_draws))
    )
  )
}


#' @export
print.besd_poststrat <- function(x, ...) {
  cat(sprintf("<besd_poststrat>  %d rows\n", nrow(x)))
  NextMethod()
  invisible(x)
}
