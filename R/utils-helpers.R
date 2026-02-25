# small all-purpose general helpers: not exported

#' Unit Separator: safe delimiter for packed multichoice strings
.BESD_SEP <- "\u001F"

#' Require / install package 
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

#' Stop error message wrapper with call = FALSE
.stopf <- function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)

#' Paste with collapse = ", "
.pastec <- function(x) paste(x, collapse = ", ")

#' "Normalise" a string by converting to alpha-numeric
.strip_non_alpnum <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^\\p{L}\\p{N}]+", "", x, perl = TRUE)
  tolower(x)
}

#' Effective sample size
.effective_n <- function(w) {
  w <- as.numeric(w)
  w <- w[!is.na(w)]
  
  if (!length(w)) return(NA_real_)
  
  sw <- sum(w); sw2 <- sum(w^2)
  
  if (sw2 <= 0) return(NA_real_)
  
  (sw^2) / sw2
}  

# Null coallescing
`%||%` = function(x, y) if (is.null(x)) y else x

#' Dirichlet-marginal intervals for a vector of category proportions
#'
#' Takes a vector `p` (length K) and returns a Kx2 matrix (lcl/ucl) of marginal
#' intervals for each category under a symmetric Dirichlet(prior,...,prior) model
#' using effective counts p * n_eff.
#'
#' @keywords internal
.multinom_dirichlet_ci <- function(p, n_eff, conf_level = 0.95, prior = 0.5) {
  p <- as.numeric(p)
  
  if (!length(p) || is.na(n_eff) || n_eff <= 0) {
    return(matrix(NA_real_, nrow = length(p), ncol = 2L,
                  dimnames = list(NULL, c("lcl", "ucl"))))
  }
  
  p[!is.finite(p)] <- NA_real_
  p[p < 0] <- 0
  
  s <- sum(p, na.rm = TRUE)
  if (!is.finite(s) || s <= 0) {
    return(matrix(NA_real_, nrow = length(p), ncol = 2L,
                  dimnames = list(NULL, c("lcl", "ucl"))))
  }
  
  # Ensure effective counts sum to n_eff
  p <- p / s
  
  alpha <- 1 - conf_level
  K <- length(p)
  
  k_eff <- p * n_eff
  a <- prior + k_eff
  a0 <- K * prior + n_eff
  b <- a0 - a
  
  a <- pmax(a, .Machine$double.eps)
  b <- pmax(b, .Machine$double.eps)
  
  lcl <- stats::qbeta(alpha / 2, shape1 = a, shape2 = b)
  ucl <- stats::qbeta(1 - alpha / 2, shape1 = a, shape2 = b)
  
  cbind(lcl = lcl, ucl = ucl)
}

#' Jeffreys/Beta marginal CI for a single (possibly fractional) proportion
#' Used for multichoice tokens (non-mutually-exclusive).
#' @keywords internal
.beta_binom_ci <- function(p, n_eff, conf_level = 0.95, prior = 0.5) {
  if (is.na(p) || is.na(n_eff) || n_eff <= 0) {
    return(c(lcl = NA_real_, ucl = NA_real_))
  }
  p <- min(1, max(0, as.numeric(p)))
  
  alpha <- 1 - conf_level
  k_eff <- p * n_eff
  a <- prior + k_eff
  b <- prior + (n_eff - k_eff)
  
  a <- max(a, .Machine$double.eps)
  b <- max(b, .Machine$double.eps)
  
  c(
    lcl = stats::qbeta(alpha / 2, shape1 = a, shape2 = b),
    ucl = stats::qbeta(1 - alpha / 2, shape1 = a, shape2 = b)
  )
}
