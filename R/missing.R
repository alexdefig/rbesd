
# ── besd_missing_summary() ─────────────────────────────────────────────────────

#' Summarise missing data fractions for regression variables
#'
#' Reports per-variable and joint (listwise) missing fractions. The joint fraction is 
#' the proportion of rows that `complete.cases()` would drop across all variables 
#' together — the number that actually matters for complete-case regression.
#'
#' Call this after preparing your `besd_data` object but before `besd_regress()`, 
#' optionally re-running after applying `min_n_context` manually to see its impact on 
#' the joint fraction.
#'
#' @param x A `besd_data` object or plain data frame.
#' @param vars Character vector of variable names to check. If `NULL`, all cols checked.
#' @param country_col Name of the country column. Inferred automatically from
#'   `besd_data` objects; required for plain data frames.
#' @param threshold Fraction above which a variable or the joint row is flagged. 
#'    Default `0.05`.
#' @return A tibble with columns `variable`, `n_total`, `n_missing`, `pct_missing`, 
#'   `flagged`, and a list-column `by_country` (per-country breakdown for each variable; 
#'   `NULL` for the joint row). The final row is always `"(listwise joint)"` and reflects 
#'   the fraction lost to complete-case deletion across all variables jointly.
#' @export
besd_missing_summary <- function(x, vars = NULL, country_col = NULL,
                                 threshold = 0.05) {
  if (inherits(x, "besd_data")) {
    info        <- besd_info(x)
    country_col <- info$country_col
    df          <- tibble::as_tibble(x)
  } else {
    if (!is.data.frame(x)) .stopf("`x` must be a besd_data or data frame.")
    df <- tibble::as_tibble(x)
  }
  
  if (is.null(vars)) vars <- names(df)
  vars <- intersect(vars, names(df))
  if (!length(vars)) .stopf("None of the requested variables found in `x`.")
  
  rows <- lapply(vars, function(v) {
    n_total   <- nrow(df)
    n_missing <- sum(is.na(df[[v]]))
    pct       <- n_missing / n_total
    
    by_cty <- NULL
    if (!is.null(country_col) && country_col %in% names(df)) {
      by_cty <- df |>
        dplyr::group_by(country = .data[[country_col]]) |>
        dplyr::summarise(
          n_total     = dplyr::n(),
          n_missing   = sum(is.na(.data[[v]])),
          pct_missing = .data$n_missing / .data$n_total,
          .groups = "drop"
        ) |>
        dplyr::filter(.data$n_missing > 0) |>
        dplyr::arrange(dplyr::desc(.data$pct_missing))
    }
    
    tibble::tibble(
      variable    = v,
      n_total     = n_total,
      n_missing   = n_missing,
      pct_missing = pct,
      flagged     = pct >= threshold,
      by_country  = list(by_cty)
    )
  })
  
  # Joint (listwise) row — this is the fraction complete.cases() actually drops
  n_complete <- sum(stats::complete.cases(df[, vars, drop = FALSE]))
  n_lost     <- nrow(df) - n_complete
  pct_lost   <- n_lost / nrow(df)
  rows[["__joint__"]] <- tibble::tibble(
    variable    = "(listwise joint)",
    n_total     = nrow(df),
    n_missing   = n_lost,
    pct_missing = pct_lost,
    flagged     = pct_lost >= threshold,
    by_country  = list(NULL)
  )
  
  dplyr::bind_rows(rows)
}


# Internal: warn on joint listwise missingness after prep (NAs from min_n_context
# already applied). Keys off the joint row, not per-variable.
.warn_missingness <- function(df, vars, country_col, threshold = 0.05,
                              context = NULL) {
  vars <- intersect(vars, names(df))
  if (!length(vars)) return(invisible(NULL))
  
  smry   <- besd_missing_summary(df, vars = vars, country_col = country_col,
                                 threshold = threshold)
  joint  <- smry[smry$variable == "(listwise joint)", , drop = FALSE]
  
  if (!nrow(joint) || !isTRUE(joint$flagged)) return(invisible(NULL))
  
  # Also report per-variable lines for any variable that is itself flagged
  flagged_vars <- smry[smry$flagged & smry$variable != "(listwise joint)", ]
  
  var_lines <- if (nrow(flagged_vars)) {
    vapply(seq_len(nrow(flagged_vars)), function(i) {
      row    <- flagged_vars[i, ]
      top3   <- utils::head(row$by_country[[1]], 3)
      cty_str <- if (!is.null(top3) && nrow(top3)) {
        paste0(" [worst: ",
               paste0(top3$country, " ",
                      sprintf("%.0f%%", top3$pct_missing * 100),
                      collapse = ", "),
               "]")
      } else ""
      sprintf("  %s: %.1f%% missing%s",
              row$variable, row$pct_missing * 100, cty_str)
    }, character(1))
  } else character(0)
  
  ctx_str  <- if (!is.null(context)) paste0(" (", context, ")") else ""
  var_part <- if (length(var_lines)) {
    paste0("\nPer-variable:\n", paste(var_lines, collapse = "\n"))
  } else ""
  
  warning(
    sprintf(
      paste0("Complete-case deletion%s will remove %.1f%% of rows ",
             "(%d of %d). Estimates may be biased if missingness is ",
             "not random.%s\n",
             "Use besd_missing_summary() to investigate further."),
      ctx_str,
      joint$pct_missing * 100,
      joint$n_missing,
      joint$n_total,
      var_part
    ),
    call. = FALSE
  )
}