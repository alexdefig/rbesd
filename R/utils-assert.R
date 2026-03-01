
# ── utils-assert.R ─────────────────────────────────────────────────────────────
# Small internal assertion utilities used across the package. None are exported.
#
# .assert_besd()             – stop if `x` is not a `besd_data` object
# .assert_valid_dict()       - stop if `x` not a valid dictionary
# .assert_has_cols()         – stop if required columns are absent from a data frame
# .assert_is_scalar_string() – stop if `x` is not a non-empty length-1 string


# Assert that an object inherits from `besd_data`.
.assert_besd <- function(x) {
  if (!inherits(x, "besd_data")) {
    stop("`x` must be a `besd_data` object.", call. = FALSE)
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
