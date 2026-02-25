# small all-purpose general helpers: not exported

# Unit Separator: safe delimiter for packed multichoice strings
.BESD_SEP <- "\u001F"

# Null coallescing
`%||%` = function(x, y) if (is.null(x)) y else x

# Require / install package 
.require_pkg = function(pkg, purpose = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg = paste0(
      "Package '", pkg, "' is required",
      if (!is.null(purpose)) paste0(" (", purpose, ")") else "",
      ". Install it with install.packages('", pkg, "')."
    )
    stop(msg, call. = FALSE)
  }
}

# Stop error message wrapper with call = FALSE
.stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

# Paste with collapse = ", "
.pastec <- function(x) paste(x, collapse = ", ")

# "Normalise" a string by converting to alpha-numeric
.strip_non_alpnum <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^\\p{L}\\p{N}]+", "", x, perl = TRUE)
  tolower(x)
}

# Create "safe" (identifier-friendly) and unique names from an input vector:
#  (1) converts to character.
#  (2) replaces non-alphanumeric Unicode characters with underscores.
#  (3) collapses / trims underscores.
#  (4) prefixes names starting with digits with "x<sep>".
#  (5) substitutes empty results with "x".
#  (6) ensures uniqueness via make.unique().
.make_safe_names <- function(x, sep = "_") {
  if (!is.character(x)) x <- as.character(x)
  x <- gsub("[^\\p{L}\\p{N}]+", "_", x, perl = TRUE)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- ifelse(grepl("^[0-9]", x), paste0("x", sep, x), x)
  x[x == ""] <- "x"
  make.unique(x, sep = sep)
}

# Effective sample size
.effective_n <- function(w) {
  w <- as.numeric(w)
  w <- w[!is.na(w)]
  
  if (!length(w)) return(NA_real_)
  
  sw <- sum(w); sw2 <- sum(w^2)
  
  if (sw2 <= 0) return(NA_real_)
  
  (sw^2) / sw2
}  