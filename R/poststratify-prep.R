# ── besd_poststrat_frame() ─────────────────────────────────────────────────────

#' Prepare a poststratification frame for use with BeSD models
#'
#' Validates, maps, and encodes a poststratification data frame (e.g. from
#' census or administrative data) for use with [besd_fitted_probs()] and
#' [besd_poststratify()]. Each row must represent a unique covariate
#' combination (cell) with an associated population count in `pop_col`.
#' Predictor columns are encoded to match the internal representation used
#' during model fitting, using the metadata stored in the `fit` object returned
#' from [besd_regress()].
#'
#' @param df A data frame or tibble. Each row should be a unique covariate
#'   combination. The `pop_col` column contains the population count for
#'   that cell (e.g. number of people with that combination of characteristics).
#' @param fit A `besd_fit` or `besd_fit_by_country` object from
#'   [besd_regress()].
#' @param mapping Optional named character vector mapping column names in `df`
#'   to predictor names used in the fitted model. Names are the user's column
#'   names in `df`; values are the corresponding predictor names as used in the
#'   model. Uses the same convention as the `mapping` argument in [as_besd()].
#' @param pop_col String. Name of the column in `df` containing population
#'   cell counts (e.g. number of census respondents in that cell). Must be
#'   numeric, non-negative, and have no missing values. Note: this is
#'   distinct from survey sampling weights (`weight_col` in [as_besd()]);
#'   see Details.
#'
#' @section Sampling weights vs population counts:
#' Survey sampling weights (specified via `weight_col` in [as_besd()]) correct
#' for unequal selection probabilities within the survey sample and are used
#' during model fitting. Population cell counts (`pop_col` here) are used only
#' during poststratification to reweight model predictions to the target
#' population structure. These are conceptually distinct and should not be
#' confused.
#'
#' @section Sub-national use and context-predictor limitation:
#' The supported use of poststratification in rbesd is **sub-national
#' (admin1-level) analysis within a single country**, where the "country"
#' grouping column corresponds to the first administrative unit (e.g. region
#' or state). In this setting all predictors are common across units, and the
#' full MrP pipeline works correctly.
#'
#' Multi-country poststratification is **not supported** when the fitted model
#' contains context-specific predictors (variables whose levels differ across
#' countries, expanded into per-country dummy columns by [besd_regress()]).
#' Such models cannot be correctly poststratified from a census frame because
#' the per-country dummy structure has no natural equivalent in population
#' data. An error is raised if this configuration is detected. To use
#' poststratification in a multi-country setting, refit the model treating the
#' relevant variables as common predictors.
#'
#' @return A `besd_poststrat_frame` object: a validated, encoded tibble with a
#'   `.row_id` column appended. The `pop_col`, `country_col`, and `level_map`
#'   are stored as attributes for downstream use by [besd_poststratify()].
#'
#' @seealso [besd_fitted_probs()], [besd_poststratify()]
#' @export
besd_poststrat_frame <- function(df, fit, mapping = NULL, pop_col) {

  .assert_besd_fit(fit)
  if (!is.data.frame(df))
    .stopf("`df` must be a data frame or tibble.")
  .assert_is_scalar_string(pop_col, "pop_col")

  prep        <- fit$prep
  country_col <- prep$country_col

  df <- .ps_rename_cols(df, mapping)

  if (!country_col %in% names(df))
    .stopf(
      "Country column `%s` not found in `df`. Use `mapping` to rename your column.",
      country_col
    )

  # Warn about countries not seen during fitting (fixed-effect predictions only)
  unknown_ctys <- setdiff(unique(as.character(df[[country_col]])), prep$countries)
  if (length(unknown_ctys))
    warning(
      sprintf(
        paste0(
          "%d country value(s) not seen during model fitting; ",
          "only population-level (fixed-effect) predictions will be used: %s"
        ),
        length(unknown_ctys), .pastec(unknown_ctys)
      ),
      call. = FALSE
    )

  # Validate pop_col before encoding (fail fast)
  if (!pop_col %in% names(df))
    .stopf("`pop_col` '%s' not found in `df`.", pop_col)
  pop <- df[[pop_col]]
  if (!is.numeric(pop))
    .stopf("`pop_col` '%s' must be numeric.", pop_col)
  if (anyNA(pop) || any(pop < 0))
    .stopf("`pop_col` '%s' must be non-negative with no missing values.", pop_col)

  df <- .ps_encode_predictors(df, prep)

  # Uniqueness check: each covariate combination should appear once
  pred_cols <- intersect(c(country_col, prep$preds_common), names(df))
  .ps_check_uniqueness(df, pred_cols)

  df$.row_id <- seq_len(nrow(df))

  structure(
    tibble::as_tibble(df),
    class       = c("besd_poststrat_frame", "tbl_df", "tbl", "data.frame"),
    pop_col     = pop_col,
    country_col = country_col,
    level_map   = prep$level_map  # retained for label decoding in besd_poststratify()
  )
}


# ── Internal helpers ───────────────────────────────────────────────────────────

# Rename df columns using a mapping vector (names = user cols, values = target
# predictor names). Silently skips entries where the source column is absent or
# the target name already exists, to avoid clobbering existing columns.
.ps_rename_cols <- function(df, mapping) {
  if (is.null(mapping) || !length(mapping)) return(df)
  for (i in seq_along(mapping)) {
    from <- names(mapping)[[i]]
    to   <- mapping[[i]]
    if (nzchar(from) && from %in% names(df) && !to %in% names(df))
      names(df)[names(df) == from] <- to
  }
  df
}


# Encode factor predictor columns from human-readable labels to the opaque
# codes (__01, __02, ...) used during model fitting, using prep$level_map.
# Unknown levels are recoded to NA with a warning.
# Errors if the fitted model contains context-specific predictors (expanded
# per-country dummies) — those cannot be correctly populated from a census
# frame and indicate an unsupported multi-country MrP configuration.
.ps_encode_predictors <- function(df, prep) {
  if (length(prep$added_ctx) > 0L) {
    ctx_vars <- unique(sub("^ctx_(.+)_[^_]+__[0-9]+$", "\\1", prep$added_ctx))
    .stopf(
      paste0(
        "The fitted model contains context-specific predictor(s) (%s), which ",
        "are expanded into per-country dummy columns that cannot be correctly ",
        "populated from a census frame. Poststratification is only supported ",
        "when all predictors are common (i.e. sub-national / single-country MrP). ",
        "Refit the model with these variable(s) as common predictors."
      ),
      .pastec(ctx_vars)
    )
  }

  for (nm in prep$preds_common) {
    mp <- prep$level_map[[nm]]
    if (is.null(mp)) next  # numeric predictor — no encoding needed

    if (!nm %in% names(df))
      .stopf(
        "Predictor `%s` is missing from the poststratification frame. ",
        "Supply it directly or use `mapping` to rename your column.", nm
      )

    v   <- as.character(df[[nm]])
    bad <- setdiff(stats::na.omit(unique(v)), mp$label)
    if (length(bad))
      warning(
        sprintf(
          "Predictor `%s`: %d level(s) not seen during fitting will be set to NA: %s",
          nm, length(bad), .pastec(bad)
        ),
        call. = FALSE
      )

    code_of  <- stats::setNames(mp$code, mp$label)
    encoded  <- ifelse(v %in% bad | is.na(v), NA_character_, unname(code_of[v]))
    df[[nm]] <- factor(encoded, levels = mp$code)
  }

  df
}


# Warn if rows are not unique across the specified predictor columns. Duplicate
# cells will inflate population counts and produce incorrect estimates.
.ps_check_uniqueness <- function(df, pred_cols) {
  if (!length(pred_cols)) return(invisible(NULL))
  n_rows   <- nrow(df)
  n_unique <- nrow(unique(df[, pred_cols, drop = FALSE]))
  if (n_unique < n_rows)
    warning(
      sprintf(
        paste0(
          "Poststratification frame has %d rows but only %d unique covariate ",
          "combinations. Duplicate rows will inflate population counts — ",
          "consider collapsing duplicates and summing their `%s` values."
        ),
        n_rows, n_unique, pred_cols[[1L]]
      ),
      call. = FALSE
    )
}


#' @export
print.besd_poststrat_frame <- function(x, ...) {
  cat(sprintf(
    "<besd_poststrat_frame>  %d cells | country_col: '%s' | pop_col: '%s'\n",
    nrow(x), attr(x, "country_col"), attr(x, "pop_col")
  ))
  NextMethod()
  invisible(x)
}
