
# ── utils-assert.R ─────────────────────────────────────────────────────────────
# Small internal assertion utilities used across the package. None are exported.
#
# .assert_besd()                  – stop if `x` is not a `besd_data` object
# .assert_valid_dict()            – stop if `x` not a valid dictionary
# .assert_has_cols()              – stop if required columns are absent from a data frame
# .assert_is_scalar_string()      – stop if `x` is not a non-empty length-1 string
# .assert_besd_fit()              – stop if `x` is not a besd_fit/besd_fit_by_country
# .assert_besd_fitted()           – stop if `x` is not a besd_fitted object
# .assert_besd_poststrat_frame()  – stop if `x` is not a besd_poststrat_frame object


# Assert that an object inherits from `besd_data`.
.assert_besd <- function(x) {
  if (!inherits(x, "besd_data")) {
    stop("`x` must be a `besd_data` object.", call. = FALSE)
  }
  invisible(x)
}


#' Validate that an object is a usable BeSD summary table
#' @keywords internal
.assert_besd_summary_tbl <- function(x,
                                     fn = NULL,
                                     require_attrs = TRUE) {
  fn_tag <- if (!is.null(fn)) paste0(fn, ": ") else ""
  
  # Basic structure
  if (!is.data.frame(x)) {
    stop(fn_tag, "Expected a BeSD summary tibble (data frame), got ",
         class(x)[[1]], ".", call. = FALSE)
  }
  
  # Required columns
  required <- c("item_id", "response", "pct", "item_type", "country")
  missing  <- setdiff(required, names(x))
  if (length(missing)) {
    stop(fn_tag,
         "Summary table is missing required column(s): ",
         paste(missing, collapse = ", "),
         ".\nDid you pass the output of summary(besd_data)?",
         call. = FALSE)
  }
  
  # Class tag (warn, not error — easy to lose after dplyr ops)
  if (!"besd_summary_tbl" %in% class(x)) {
    warning(fn_tag,
            "Object does not have class 'besd_summary_tbl'. ",
            "Some features (e.g. response ordering, reverse-flag top-box) ",
            "may not work correctly if the table was modified after calling ",
            "summary().",
            call. = FALSE)
  }
  
  # Attributes needed for reverse-flag and response ordering
  if (isTRUE(require_attrs)) {
    missing_attrs <- character()
    if (is.null(attr(x, "besd_dict"))) missing_attrs <- c(missing_attrs, "besd_dict")
    if (is.null(attr(x, "dem_dict")))  missing_attrs <- c(missing_attrs, "dem_dict")
    
    if (length(missing_attrs)) {
      warning(fn_tag,
              "Summary table is missing attribute(s): ",
              paste(missing_attrs, collapse = ", "),
              ".\nReverse-flag top-box selection and response ordering will ",
              "be disabled. This usually happens when dplyr verbs (filter, ",
              "mutate, etc.) strip attributes from the summary table.",
              call. = FALSE)
    }
  }
  
  invisible(x)
}

# Assert that `dict` passes .validate_dict(); stop with a consolidated error message 
# listing all problems if it does not. Returns invisible(TRUE) on success.
.assert_valid_dict <- function(dict, 
                               with_id = TRUE, ..., 
                               arg = deparse(substitute(dict))) {
  probs <- .validate_dict(dict, with_id = with_id, ...)
  if (length(probs)) {
    .stopf("Invalid dictionary `%s`:\n- %s", arg, paste(probs, collapse = "\n- "))
  }
  invisible(TRUE)
}

# Assert that `df` is a data.frame and that `cols` exist in it. Supports either a scalar 
# column name (scalar = TRUE) or a character vector. If null_ok = TRUE, NULL is allowed 
# and the check is skipped. If strict = TRUE (and scalar = FALSE), `cols` must already be 
# character.
.assert_has_cols <- function(df, cols, context = "data", nm = "cols", scalar = FALSE,
                             null_ok = FALSE, strict = FALSE) {
  if (!is.data.frame(df)) .stopf("`%s` must be a data.frame.", context)
  if (is.null(nm) || !nzchar(nm)) nm <- "cols"
  
  if (is.null(cols)) {
    if (isTRUE(null_ok)) return(invisible(TRUE))
    if (isTRUE(scalar)) .stopf("`%s` must be a non-empty scalar string.", nm)
    .stopf("`%s` must be NULL or a character vector.", nm)
  }
  
  if (isTRUE(scalar)) {
    if (!is.character(cols) || length(cols) != 1L || !nzchar(cols)) {
      .stopf("`%s` must be a non-empty scalar string.", nm)
    }
  } else if (isTRUE(strict) && !is.character(cols)) {
    .stopf("`%s` must be NULL or a character vector.", nm)
  } else {
    cols <- as.character(cols)
  }
  
  miss <- setdiff(cols, names(df))
  if (length(miss)) {
    .stopf(
      "%s is missing column(s): %s",
      context,
      paste(miss, collapse = ", ")
    )
  }
  invisible(TRUE)
}

# Assert that `x` is a non-empty, length-1 character string. Useful for user-facing
# scalar string arguments.
.assert_is_scalar_string <- function(x, nm = deparse(substitute(x))) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(
      sprintf("`%s` must be a non-empty length-1 character.", nm),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Assert that `x` is a single finite numeric value.
# Intended for validating scalar numeric arguments (e.g., conf_level, priors).
.assert_is_scalar_number <- function(x, nm) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop("`", nm, "` must be a single finite numeric.", call. = FALSE)
  }
  invisible(TRUE)
}


# Assert that `x` is a besd_fit or besd_fit_by_country object.
.assert_besd_fit <- function(x, nm = deparse(substitute(x))) {
  if (!inherits(x, c("besd_fit", "besd_fit_by_country"))) {
    .stopf("`%s` must be a besd_fit or besd_fit_by_country object.", nm)
  }
  invisible(x)
}


# Assert that `x` is a besd_fitted object (output of besd_fitted_probs()).
.assert_besd_fitted <- function(x, nm = deparse(substitute(x))) {
  if (!inherits(x, "besd_fitted")) {
    .stopf(
      "`%s` must be a besd_fitted object from `besd_fitted_probs()`.", nm
    )
  }
  invisible(x)
}


# Assert that `x` is a besd_poststrat_frame object (output of besd_poststrat_frame()).
.assert_besd_poststrat_frame <- function(x, nm = deparse(substitute(x))) {
  if (!inherits(x, "besd_poststrat_frame")) {
    .stopf(
      "`%s` must be a besd_poststrat_frame object from `besd_poststrat_frame()`.", nm
    )
  }
  invisible(x)
}