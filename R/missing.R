# ── besd_recode_missing() ──────────────────────────────────────────────────────

#' Recode missing-data tokens to NA
#'
#' Convenience helper for analysis workflows.
#'
#' @param x A `besd_data` object or data.frame.
#' @param tokens Missing tokens. Character vector applied to all items/columns,
#'   or named list with `.all` and/or per-item vectors.
#' @param items Optional subset of item_ids/column names to process.
#' @param dict Optional dictionary to identify multichoice items when `x` is a
#'   data.frame. If `x` is a `besd_data`, dictionaries are read from attributes.
#' @param multichoice_specs Optional list of multichoice specs (for `sep`) when
#'   `x` is a data.frame. If `x` is a `besd_data`, specs are read from
#'   `besd_info(x)$meta$multichoice_specs`.
#' @param drop_levels If TRUE (default) and a column is a factor, drop the
#'   missing-token levels.
#' @return `x`, with missing tokens set to NA.
#' @export
besd_recode_missing <- function(x,
                                tokens,
                                items             = NULL,
                                dict              = NULL,
                                multichoice_specs = list(),
                                drop_levels       = TRUE) {
  
  if (missing(tokens) || is.null(tokens)) .stopf("`tokens` must be provided.")
  
  if (is.list(tokens) && (is.null(names(tokens)) || any(names(tokens) == "")))
    .stopf(
      "`tokens` must be a char vector or *named* list (names = item_id and/or `.all`)."
    )
  
  is_besd <- inherits(x, "besd_data")
  
  if (is_besd) {
    info  <- besd_info(x)
    dict  <- dplyr::bind_rows(
      tibble::as_tibble(info$besd_dict),
      if (is.null(info$dem_dict)) NULL else tibble::as_tibble(info$dem_dict)
    )
    meta              <- info$meta %||% list()
    multichoice_specs <- meta$multichoice_specs %||% multichoice_specs
  } else {
    if (!is.data.frame(x)) .stopf("`x` must be a besd_data or a data.frame.")
    # best-effort: treat all columns as non-multichoice if no dict supplied
    dict <- if (is.null(dict)) {
      tibble::tibble(item_id = character(0), item_type = character(0), levels = list())
    } else {
      tibble::as_tibble(dict)
    }
  }
  
  df   <- tibble::as_tibble(x)
  cols <- if (is.null(items)) names(df) else intersect(names(df), items)
  
  for (nm in cols) {
    miss <- .besd_missing_tokens_for_item(tokens, nm)
    if (is.null(miss) || !length(miss)) next
    
    miss_keys <- .strip_non_alpnum(miss)
    mt        <- dict$item_type[match(nm, dict$item_id)]
    if (length(mt) == 0L || is.na(mt)) mt <- NA_character_
    v <- df[[nm]]
    
    if (!is.na(mt) && mt == "multichoice") {
      # Strip missing tokens from packed strings; NA if nothing remains
      spec  <- multichoice_specs[[nm]] %||% list()
      sep   <- spec$sep %||% .BESD_SEP
      v_chr <- trimws(as.character(v))
      v_chr[v_chr == ""] <- NA_character_
      
      df[[nm]] <- vapply(v_chr, function(s) {
        if (is.na(s)) return(NA_character_)
        toks <- strsplit(s, sep, fixed = TRUE)[[1]]
        toks <- toks[toks != "" & !(.strip_non_alpnum(toks) %in% miss_keys)]
        if (!length(toks)) NA_character_ else paste(unique(toks), collapse = sep)
      }, character(1))
      next
    }
    
    # Non-multichoice: NA out missing tokens, drop factor levels if requested.
    df[[nm]] <- .na_missing_tokens(v, miss_keys, drop_levels)
  }
  
  if (!is_besd) return(df)
  
  # Update dict$levels for any token that existed as a dict level and was dropped.
  # Tokens like "Don't Know" that were never dict levels pass through harmlessly.
  info <- besd_info(x)
  if (isTRUE(drop_levels)) {
    for (nm in cols) {
      miss <- .besd_missing_tokens_for_item(tokens, nm)
      if (length(miss)) {
        info <- .update_dict_levels(info, col = nm, remove = miss, add = character(0))
      }
    }
  }
  
  # Re-wrap as besd_data preserving all attributes
  new_besd_data(
    data        = df,
    besd_dict   = info$besd_dict,
    dem_dict    = info$dem_dict,
    country_col = info$country_col,
    weight_col  = info$weight_col,
    id_col      = info$id_col,
    mapping     = info$mapping,
    besd_items  = info$besd_items,
    dem_items   = info$dem_items,
    meta        = info$meta
  )
}

# ── besd_recode_levels() ───────────────────────────────────────────────────────

#' Recode factor levels in a column before or after calling as_besd()
#'
#' A lightweight helper for collapsing or renaming rare levels. Accepts either
#' a `besd_data` object (typical use: after [as_besd()], before [besd_regress()])
#' or a plain vector (use: before [as_besd()]).
#'
#' When `x` is a `besd_data` object, the dictionary levels for `col` are also
#' updated to reflect the recoding, so the object stays internally consistent.
#'
#' The target label must already exist in the data or dictionary. Recoding to a
#' brand-new label not in the dictionary will cause an unknown-value error if
#' the object is later passed back through [as_besd()].
#'
#' @param x A `besd_data` object or a factor/character vector.
#' @param recode Named character vector: names are old labels, values are new
#'   labels. E.g. `c("Luo" = "Other", "Kikuyu minority" = "Other")`.
#' @param col Column name to recode. Required when `x` is a `besd_data` object;
#'   ignored when `x` is a vector.
#' @param warn_new If `TRUE` (default), warn when a target label does not
#'   already exist in the data, as this may indicate a dictionary mismatch.
#'
#' @return `x` with levels recoded. When `x` is a `besd_data` object, the
#'   dictionary is updated and a `besd_data` object is returned. When `x` is a
#'   vector, a recoded vector of the same type is returned.
#' @examples
#' \dontrun{
#' # On a besd_data object (recommended)
#' y <- besd_recode_levels(
#'   y,
#'   col    = "dem_eth",
#'   recode = c("Luo" = "Other", "Kikuyu minority" = "Other")
#' )
#'
#' # On a raw vector before as_besd()
#' data_demo$dem_eth <- besd_recode_levels(
#'   data_demo$dem_eth,
#'   recode = c("Luo" = "Other", "Kikuyu minority" = "Other")
#' )
#' }
#' @export
besd_recode_levels <- function(x, recode, col = NULL, warn_new = TRUE) {
  
  if (!is.character(recode) || is.null(names(recode)) || any(names(recode) == ""))
    .stopf("`recode` must be a named character vector: c('old' = 'new', ...).")
  
  # ── besd_data path ─────────────────────────────────────────────────────────
  if (inherits(x, "besd_data")) {
    if (is.null(col) || !is.character(col) || length(col) != 1L || !nzchar(col))
      .stopf("`col` must be a single non-empty column name when `x` is a `besd_data`.")
    
    df <- tibble::as_tibble(x)
    if (!col %in% names(df))
      .stopf("Column `%s` not found in `x`.", col)
    
    df[[col]] <- besd_recode_levels(df[[col]], recode = recode, warn_new = warn_new)
    
    # Update dictionary levels for this column to stay consistent
    # NA targets (recode to NA) are dropped from `add` inside .update_dict_levels().
    info <- .update_dict_levels(
      besd_info(x),
      col    = col,
      remove = names(recode),
      add    = unique(unname(recode))
    )
    
    return(new_besd_data(
      data        = df,
      besd_dict   = info$besd_dict,
      dem_dict    = info$dem_dict,
      country_col = info$country_col,
      weight_col  = info$weight_col,
      id_col      = info$id_col,
      mapping     = info$mapping,
      besd_items  = info$besd_items,
      dem_items   = info$dem_items,
      meta        = info$meta
    ))
  }
  
  # ── Plain vector path ──────────────────────────────────────────────────────
  is_fct <- is.factor(x)
  x_chr  <- as.character(x)
  
  if (isTRUE(warn_new)) {
    existing <- unique(x_chr[!is.na(x_chr)])
    new_levs <- setdiff(unique(unname(recode)[!is.na(unname(recode))]), existing)
    if (length(new_levs)) {
      warning(
        sprintf(
          paste0(
            "besd_recode_levels(): target label(s) not found in existing data: %s. ",
            "Ensure these match your dictionary levels exactly."
          ),
          paste(paste0('"', new_levs, '"'), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  
  hits <- names(recode)[names(recode) %in% x_chr]
  for (old in hits) x_chr[x_chr == old] <- recode[[old]]
  
  if (!is_fct) return(x_chr)
  
  old_levs     <- levels(x)
  removed_levs <- names(recode)[names(recode) %in% old_levs]
  new_levs     <- unique(c(
    old_levs[!old_levs %in% removed_levs],
    unique(unname(recode)[names(recode) %in% removed_levs])
  ))
  new_levs <- new_levs[new_levs %in% unique(x_chr[!is.na(x_chr)])]
  factor(x_chr, levels = new_levs)
}


# ── besd_missing_summary() ─────────────────────────────────────────────────────

#' Summarise missing data fractions for regression variables
#'
#' Reports per-variable and joint (listwise) missing fractions. The joint fraction is 
#' the proportion of rows that `complete.cases()` would drop across all variables 
#' together — the number that actually matters for complete-case regression.
#'
#' Call this after preparing your `besd_data` object but before [besd_regress()], 
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
          n_total      = dplyr::n(),
          n_missing    = sum(is.na(.data[[v]])),
          prop_missing = .data$n_missing / .data$n_total,
          .groups = "drop"
        ) |>
        dplyr::filter(.data$n_missing > 0) |>
        dplyr::arrange(dplyr::desc(.data$prop_missing))
    }
    
    tibble::tibble(
      variable     = v,
      n_total      = n_total,
      n_missing    = n_missing,
      prop_missing = pct,
      flagged      = pct >= threshold,
      by_country   = list(by_cty)
    )
  })
  
  # Joint (listwise) row — this is the fraction complete.cases() actually drops
  n_complete <- sum(stats::complete.cases(df[, vars, drop = FALSE]))
  n_lost     <- nrow(df) - n_complete
  pct_lost   <- n_lost / nrow(df)
  rows[["__joint__"]] <- tibble::tibble(
    variable     = "(listwise joint)",
    n_total      = nrow(df),
    n_missing    = n_lost,
    prop_missing = pct_lost,
    flagged      = pct_lost >= threshold,
    by_country   = list(NULL)
  )
  
  dplyr::bind_rows(rows)
}


# ── besd_rare_levels() ─────────────────────────────────────────────────────────

#' Inspect rare factor levels before fitting
#'
#' Reports per-(predictor, country, level) observation counts so you can make
#' an informed choice about `min_n_context` before calling [besd_prepare()] or
#' [besd_regress()]. Levels with very few observations can cause near-complete
#' separation or wildly unstable estimates; levels below your chosen threshold
#' will be recoded to `NA` by `min_n_context` and excluded via listwise
#' deletion.
#'
#' Only unordered factor and character columns are examined — continuous and
#' ordered predictors are skipped with a message.
#'
#' @param x A `besd_data` object or plain data frame.
#' @param predictors Character vector of predictor column names to inspect.
#' @param country_col Name of the country column. Inferred automatically from
#'   `besd_data` objects; required for plain data frames.
#' @param min_n Integer threshold to flag against (default `10L`). Sets the
#'   `would_collapse` column — does not modify the data.
#' @param only_rare If `TRUE` (default), return only rows where `n < min_n`.
#'   Set to `FALSE` to see all (predictor, country, level) combinations.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{predictor}{Column name.}
#'     \item{country}{Country label.}
#'     \item{level}{Human-readable level label (or raw value if no label found).}
#'     \item{n}{Observation count in this cell.}
#'     \item{pct}{Fraction of that country's rows this cell represents.}
#'     \item{would_collapse}{`TRUE` if `n < min_n` — this level would be
#'       recoded to `NA` at the given threshold.}
#'     \item{n_rows_affected}{Number of rows that would be recoded to `NA`
#'       (equals `n` for levels that would collapse, `0L` otherwise).}
#'   }
#'   Ordered by `predictor`, `would_collapse` (TRUE first), then `n` ascending,
#'   so the most dangerous cells are at the top within each predictor.
#'   Returns a zero-row tibble if no factor predictors are found.
#'
#' @examples
#' \dontrun{
#' # Check before choosing min_n_context
#' besd_rare_levels(my_data, predictors = c("dem_eth", "dem_rel"))
#'
#' # See full breakdown including common levels
#' besd_rare_levels(my_data, predictors = "dem_eth", only_rare = FALSE)
#' }
#' @export
besd_rare_levels <- function(x, predictors, country_col = NULL,
                             min_n = 10L, only_rare = TRUE) {
  
  # ── Resolve data and country column ─────────────────────────────────────────
  if (inherits(x, "besd_data")) {
    info        <- besd_info(x)
    country_col <- info$country_col
    df          <- tibble::as_tibble(x)
    dem_dict    <- info$dem_dict %||% NULL
  } else {
    if (!is.data.frame(x)) .stopf("`x` must be a besd_data or data frame.")
    df       <- tibble::as_tibble(x)
    dem_dict <- NULL
  }
  
  if (is.null(country_col) || !nzchar(country_col)) {
    .stopf("`country_col` must be supplied for plain data frames.")
  }
  .assert_has_cols(df, c(country_col, predictors), "x")
  
  min_n <- as.integer(min_n)
  
  # ── Identify factor/character columns; skip others with a message ────────────
  skip <- predictors[vapply(predictors, function(p) {
    v <- df[[p]]
    !is.factor(v) && !is.character(v)
  }, logical(1))]
  if (length(skip)) {
    message(
      "Skipping non-factor predictor(s) (no discrete levels to inspect): ",
      paste(skip, collapse = ", ")
    )
  }
  factor_preds <- setdiff(predictors, skip)
  if (!length(factor_preds)) {
    message("No factor predictors found. Returning empty tibble.")
    return(.empty_rare_levels_tbl())
  }
  
  # ── Build label lookup from dem_dict if available ───────────────────────────
  .level_label <- function(pred, code) {
    # code here is the raw value in the column (may already be human-readable
    # for character columns, or an opaque __01 code for encoded factors)
    as.character(code)
  }
  
  countries <- sort(unique(as.character(df[[country_col]])))
  rows      <- list()
  
  for (pred in factor_preds) {
    v     <- df[[pred]]
    v_chr <- if (is.factor(v)) as.character(v) else v
    
    for (cc in countries) {
      ii      <- which(as.character(df[[country_col]]) == cc)
      n_cc    <- length(ii)
      v_cc    <- v_chr[ii]
      obs     <- v_cc[!is.na(v_cc)]
      
      if (!length(obs)) next
      
      tab <- sort(table(obs), decreasing = FALSE)
      
      for (lv in names(tab)) {
        cnt <- as.integer(tab[[lv]])
        rows[[length(rows) + 1L]] <- tibble::tibble(
          predictor       = pred,
          country         = cc,
          level           = .level_label(pred, lv),
          n               = cnt,
          pct             = cnt / n_cc,
          would_collapse  = cnt < min_n,
          n_rows_affected = if (cnt < min_n) cnt else 0L
        )
      }
    }
  }
  
  if (!length(rows)) return(.empty_rare_levels_tbl())
  
  out <- dplyr::bind_rows(rows)
  
  # Order: predictor, would_collapse descending (TRUE first), n ascending
  out <- dplyr::arrange(out, predictor, dplyr::desc(would_collapse), n)
  
  if (isTRUE(only_rare)) {
    out <- out[out$would_collapse, , drop = FALSE]
  }
  
  out
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
                      sprintf("%.0f%%", top3$prop_missing * 100),
                      collapse = ", "),
               "]")
      } else ""
      sprintf("  %s: %.1f%% missing%s",
              row$variable, row$prop_missing * 100, cty_str)
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
      joint$prop_missing * 100,
      joint$n_missing,
      joint$n_total,
      var_part
    ),
    call. = FALSE
  )
}

.empty_rare_levels_tbl <- function() {
  tibble::tibble(
    predictor       = character(),
    country         = character(),
    level           = character(),
    n               = integer(),
    pct             = numeric(),
    would_collapse  = logical(),
    n_rows_affected = integer()
  )
}


# Internal: message (not warning) listing countries where a context predictor
# has no observed levels after min_n collapsing. These countries simply
# contribute to the model without that predictor term — this is expected
# behaviour, not a data quality problem. Fires once per predictor, only when
# at least one country is fully absent.
.inform_context_coverage <- function(df, preds_context, country_col) {
  if (!length(preds_context)) return(invisible(NULL))
  
  countries <- sort(unique(as.character(df[[country_col]])))
  cc_vec    <- as.character(df[[country_col]])
  
  for (pred in preds_context) {
    v <- df[[pred]]
    v_chr <- if (is.factor(v)) as.character(v) else as.character(v)
    
    absent <- vapply(countries, function(cc) {
      obs <- v_chr[cc_vec == cc & !is.na(v_chr)]
      length(obs) == 0L
    }, logical(1))
    
    missing_countries <- countries[absent]
    if (!length(missing_countries)) next
    
    message(sprintf(
      paste0(
        "Context predictor `%s` has no observed levels in %d country/countries ",
        "(%s). Those countries will be fitted without this predictor term. ",
        "This is expected when a variable is not collected everywhere."
      ),
      pred,
      length(missing_countries),
      paste(missing_countries, collapse = ", ")
    ))
  }
  invisible(NULL)
}