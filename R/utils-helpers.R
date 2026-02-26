
# ── utils-helpers.R ────────────────────────────────────────────────────────────
# Small internal utilities used across the package. None are exported.
#
# .BESD_SEP           – delimiter for packed multichoice strings
# %||%                – null-coalescing operator
# .require_pkg()      – stop with install hint if a package is missing
# .stopf()            – sprintf-style stop() with call. = FALSE
# .pastec()           – paste with ", " collapse for error messages
# .strip_non_alpnum() – normalise a string to lowercase alphanumeric
# .make_safe_names()  – convert arbitrary strings to unique R identifiers

# Unit separator (U+001F): chosen as the packed multichoice delimiter because
# it is a non-printable ASCII control character that cannot appear in survey
# response labels, eliminating false-positive splits.
.BESD_SEP <- "\u001F"

# Null-coalescing operator: return `x` if non-NULL, otherwise `y`.
`%||%` <- function(x, y) if (is.null(x)) y else x

# Stop with an informative install hint if `pkg` is not available. 
.require_pkg <- function(pkg, purpose = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0(
      "Package '", pkg, "' is required",
      if (!is.null(purpose)) paste0(" (", purpose, ")") else "",
      ". Install it with install.packages('", pkg, "')."
    )
    stop(msg, call. = FALSE)
  }
}

# sprintf-style wrapper around stop() that suppresses the call context from the 
# error message (keeps output tidy for users)
.stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

# Collapse a character vector to a single comma-separated string.
.pastec <- function(x) paste(x, collapse = ", ")

# Reduce a string to lowercase alphanumeric characters only, stripping
# whitespace, punctuation, and Unicode non-word characters. Used to
# normalise response labels before regex matching (e.g. catch-all detection).
.strip_non_alpnum <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^\\p{L}\\p{N}]+", "", x, perl = TRUE)
  tolower(x)
}

# Convert arbitrary strings to syntactically valid, unique R identifiers:
#   1. Replace non-alphanumeric Unicode characters with underscores
#   2. Collapse runs of underscores to one; strip leading/trailing underscores
#   3. Prefix digit-leading names with "x<sep>"
#   4. Replace empty results with "x"
#   5. Append numeric suffixes via make.unique() to guarantee uniqueness
.make_safe_names <- function(x, sep = "_") {
  if (!is.character(x)) x <- as.character(x)
  x <- gsub("[^\\p{L}\\p{N}]+", "_", x, perl = TRUE)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- ifelse(grepl("^[0-9]", x), paste0("x", sep, x), x)
  x[x == ""] <- "x"
  make.unique(x, sep = sep)
}