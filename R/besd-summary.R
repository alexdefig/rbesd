
# ── summary() ──────────────────────────────────────────────────────────────────

#' Summarise BeSD items by country
#'
#' Computes weighted response percentages by country (or national total if no
#' country column is available). Optionally adds confidence intervals for each
#' response category.
#'
#' Confidence intervals are computed on proportions and returned as percentages
#' (`lcl`, `ucl` are on the 0--100 scale).
#'
#' @param object A `besd_data` object.
#' @param items Optional character vector of item IDs to summarise.
#' @param conf_level Confidence level for intervals (default 0.95).
#' @param include_demographics If `TRUE` and demographics exist, also compute
#'   raw (unweighted) percentages for demographic items and attach them as an
#'   attribute (see `besd_demographics()`).
#' @param exclude_missing_tokens If `TRUE`, any `missing_tokens` supplied to
#'   `as_besd()` are excluded from denominators even if they were kept as levels.
#' @param method Summary method. Default is `survey`. Included for forward
#'   compatibility. 
#' @param ... Unused; included for S3 compatibility.
#'
#' @return A tibble with columns including `country`, `item_id`, `response`,
#'   `pct`, `lcl`, `ucl`, and metadata columns from the item dictionary. If
#'   `include_demographics = TRUE`, a demographics tibble is attached as an
#'   attribute (see `besd_demographics()`).
#' @export
summary.besd_data <- function(object,
                              method = c("survey"),
                              items = NULL,
                              conf_level = 0.95,
                              include_demographics = TRUE,
                              exclude_missing_tokens = FALSE,
                              combine_top = FALSE,
                              ...) {
  # Check object is as_besd class
  .assert_besd(object)
  
  # BeSD info
  method <- match.arg(method)
  info <- besd_info(object)
  besd_dict <- tibble::as_tibble(info$besd_dict)
  dem_dict <- if (is.null(info$dem_dict)) NULL else tibble::as_tibble(info$dem_dict)
  
  # Which items to summarise
  besd_items <- info$besd_items %||% intersect(besd_dict$item_id, names(object))
  if (!is.null(items)) besd_items <- intersect(besd_items, items)
  if (!length(besd_items)) .stopf("No BeSD items present to summarise.")
  
  # Base data
  df <- tibble::as_tibble(object)
  country_col <- info$country_col
  stratum_col <- info$stratum_col
  psu_col     <- info$psu_col
  weight_col  <- info$weight_col
  
  # Allow 'national' summary even if country column is absent
  if (is.null(country_col) || !country_col %in% names(df)) {
    df$..country <- "national"
    country_col  <- "..country"
  }

  # Provide default design columns where missing. weight=1 is a no-op weight;
  # stratum=1 / psu=row-id collapse to an SRS design when survey runs.
  if (is.null(weight_col)  || !weight_col  %in% names(df)) {
    df$..weight <- 1
    weight_col  <- "..weight"
  }
  if (is.null(stratum_col) || !stratum_col %in% names(df)) {
    df$..stratum <- 1L
    stratum_col  <- "..stratum"
  }
  if (is.null(psu_col)     || !psu_col     %in% names(df)) {
    df$..psu <- seq_len(nrow(df))
    psu_col  <- "..psu"
  }
  
  # Missing tokens and multichoice specs
  meta <- info$meta %||% list()
  missing_tokens <- meta$missing_tokens %||% NULL
  multichoice_specs <- meta$multichoice_specs %||% list()
  
  # Summary
  out <- dplyr::bind_rows(lapply(besd_items, function(item_id) {
    meta_row <- besd_dict[besd_dict$item_id == item_id, , drop = FALSE]
    if (!nrow(meta_row)) return(NULL)
    
    .besd_summarise_one_item(
      df,
      method = method,
      item_id = item_id,
      meta_row = meta_row,
      country_col = country_col,
      stratum_col = stratum_col,
      psu_col     = psu_col,
      weight_col  = weight_col,
      conf_level = conf_level,
      multichoice_specs = multichoice_specs,
      missing_tokens = missing_tokens,
      exclude_missing_tokens = exclude_missing_tokens,
      combine_top = combine_top
    )
  }))
  
  out <- dplyr::as_tibble(out)
  class(out) <- unique(c("besd_summary_tbl", class(out)))
  
  # Attach dictionaries + meta for downstream plotting
  attr(out, "besd_dict") <- besd_dict
  attr(out, "dem_dict") <- dem_dict
  attr(out, "besd_meta") <- info$meta
  
  # Optional demographics (ALWAYS raw/unweighted)
  if (isTRUE(include_demographics) && !is.null(dem_dict)) {
    dem_items <- info$dem_items %||% intersect(dem_dict$item_id, names(object))
    
    dem_tbl <- .besd_summarise_demographics(
      df,
      dict = dem_dict,
      items = dem_items,
      country_col = country_col,
      missing_tokens = missing_tokens,
      exclude_missing_tokens = exclude_missing_tokens
    )
    
    attr(out, "demographics") <- dem_tbl
  }
  
  out
}

# ── Summaries ──────────────────────────────────────────────────────────────────
#' @keywords internal
.besd_summarise_one_item <- function(df, method = "survey", item_id, meta_row,
                                     country_col, stratum_col, psu_col, weight_col,
                                     conf_level = 0.95,
                                     multichoice_specs = list(),
                                     missing_tokens = NULL,
                                     exclude_missing_tokens = TRUE,
                                     combine_top = FALSE) {

  method     <- match.arg(method, c("survey"))
  item_type  <- meta_row$item_type[[1]]
  levels_std <- meta_row$levels[[1]]
  toplevs    <- if ("toplevs" %in% names(meta_row)) meta_row$toplevs[[1]] else NULL
  
  # Wwhen keeping missing tokens, extend levels_std to include them
  if (!exclude_missing_tokens) {
    miss_i <- .besd_missing_tokens_for_item(missing_tokens, item_id)
    if (!is.null(miss_i) && length(miss_i)) {
      levels_std <- unique(c(levels_std, as.character(miss_i)))
    }
  }
  
  # Required columns 
  df_sum <- tibble::tibble(
    .item    = as.character(df[[item_id]]),
    .country = df[[country_col]],
    .stratum = df[[stratum_col]],
    .psu     = df[[psu_col]],
    .weight  = df[[weight_col]]
  )

  # Missing-token handling: recode declared tokens to NA in .item
  if (exclude_missing_tokens) {
    miss_i <- .besd_missing_tokens_for_item(missing_tokens, item_id)
    if (!is.null(miss_i) && length(miss_i)) {
      miss_keys <- .strip_non_alpnum(miss_i)
      key_x     <- .strip_non_alpnum(df_sum$.item)
      df_sum$.item[!is.na(df_sum$.item) & (key_x %in% miss_keys)] <- NA_character_
    }
  }
  
  # Per-country row indices
  idx <- split(seq_len(nrow(df_sum)), df_sum$.country)

  # Method dispatch. Add new methods (e.g. "bayes") as additional branches.
  if (method == "survey") {

    if (!item_type %in% c("binary", "ordinal", "categorical", "multichoice")) {
      .stopf("Unsupported item_type in summary: `%s`.", item_type)
    }

    # combine_top requires non-empty toplevs and is not defined for multichoice
    if (isTRUE(combine_top)) {
      if (item_type == "multichoice") return(NULL)
      tl_ok <- is.character(toplevs) && length(toplevs) > 0 && !any(is.na(toplevs))
      if (!tl_ok) {
        .stopf("`combine_top = TRUE` requires non-empty `toplevs` in dict for `%s`.",
               item_id)
      }
    }

    spec <- multichoice_specs[[item_id]] %||% list()
    sep  <- spec$sep %||% .BESD_SEP

    rows <- lapply(names(idx), function(cty) {
      .besd_summarise_country_survey(
        dfi         = df_sum[idx[[cty]], , drop = FALSE],
        cty         = cty,
        item_id     = item_id,
        item_type   = item_type,
        levels_std  = levels_std,
        toplevs     = toplevs,
        sep         = sep,
        conf_level  = conf_level,
        combine_top = combine_top
      )
    })

    out   <- dplyr::bind_rows(rows)
    if (!nrow(out)) return(out)
    meta2 <- meta_row |> dplyr::select(domain, question, question_short)
    return(dplyr::bind_cols(out, meta2[rep(1, nrow(out)), , drop = FALSE]))
  }

  .stopf("Unsupported summary method: `%s`.", method)
}


# Per-country survey-design summary. Computes weighted proportions and
# bounded CIs for each level via survey::svyciprop. The design is chosen from
# what's populated in the country.
#' @keywords internal
.besd_summarise_country_survey <- function(dfi, cty, item_id, item_type,
                                           levels_std, toplevs = NULL,
                                           sep, conf_level,
                                           combine_top = FALSE) {
  dfi <- dfi[!is.na(dfi$.item) & !is.na(dfi$.weight), , drop = FALSE]
  n   <- nrow(dfi)

  # When combine_top, iterate over a single synthetic "level" (the top-box).
  # The response label is the comma-joined toplevs.
  if (isTRUE(combine_top)) {
    iter_levels <- list(toplevs)
    resp_label  <- paste(toplevs, collapse = ", ")
  } else {
    iter_levels <- as.list(levels_std)
    resp_label  <- NULL
  }

  if (!n) {
    empty_resp <- if (isTRUE(combine_top)) resp_label else levels_std
    return(tibble::tibble(
      country = cty, item_id = item_id, item_type = item_type,
      response = empty_resp, n = 0L,
      pct = NA_real_, lcl = NA_real_, ucl = NA_real_
    ))
  }

  has_psu    <- length(unique(dfi$.psu))     > 1
  has_strata <- length(unique(dfi$.stratum)) > 1

  old_opt <- options(survey.lonely.psu = "adjust")
  on.exit(options(old_opt), add = TRUE)

  build_design <- function(data) {
    ids_f    <- if (has_psu)    (~ .psu)     else (~ 1)
    strata_f <- if (has_strata) (~ .stratum) else NULL
    survey::svydesign(
      ids     = ids_f,
      strata  = strata_f,
      weights = ~ .weight,
      data    = data,
      nest    = has_psu && has_strata
    )
  }

  # Multichoice: pre-split each respondent's packed string once per country.
  tok_list <- if (item_type == "multichoice") {
    strsplit(dfi$.item, sep, fixed = TRUE)
  } else NULL

  dplyr::bind_rows(lapply(iter_levels, function(level) {
    dfi$.indicator <- if (item_type == "multichoice") {
      vapply(tok_list, function(v) any(level %in% v), logical(1))
    } else {
      dfi$.item %in% level
    }
    svy_lv <- build_design(dfi)
    est <- survey::svyciprop(
      ~ .indicator,
      design = svy_lv,
      method = "logit",
      level  = conf_level,
      df     = survey::degf(svy_lv)
    )
    ci <- attr(est, "ci")
    tibble::tibble(
      country   = cty,
      item_id   = item_id,
      item_type = item_type,
      response  = if (isTRUE(combine_top)) resp_label else level,
      n         = n,
      pct       = 100 * as.numeric(est),
      lcl       = 100 * ci[[1]],
      ucl       = 100 * ci[[2]]
    )
  }))
}


#' @keywords internal
.besd_summarise_demographics <- function(df, dict, items, country_col,
                                         missing_tokens = NULL,
                                         exclude_missing_tokens = TRUE) {
  if (!length(items)) {
    return(tibble::tibble())
  }
  
  rows <- lapply(items, function(item_id) {
    meta_row <- dict[dict$item_id == item_id, , drop = FALSE]
    if (!nrow(meta_row)) {
      return(NULL)
    }
    
    x <- df[[item_id]]
    cc <- df[[country_col]]
    
    x_chr <- as.character(x)
    if (exclude_missing_tokens) {
      miss_i <- .besd_missing_tokens_for_item(missing_tokens, item_id)
      if (!is.null(miss_i) && length(miss_i)) {
        miss_keys <- .strip_non_alpnum(miss_i)
        key_x <- .strip_non_alpnum(x_chr)
        x_chr[!is.na(x_chr) & (key_x %in% miss_keys)] <- NA_character_
      }
    }
    
    levs <- meta_row$levels[[1]]
    idx <- split(seq_len(nrow(df)), cc)
    
    out_cty <- lapply(names(idx), function(cty) {
      ii <- idx[[cty]]
      xi <- x_chr[ii]
      
      xi <- xi[!is.na(xi)]
      n_valid <- length(xi)
      
      if (!n_valid) {
        return(NULL)
      }
      
      dplyr::bind_rows(lapply(levs, function(lv) {
        n_lv <- sum(xi == lv, na.rm = TRUE)
        if (!n_lv) {
          return(NULL)
        }
        
        p <- n_lv / n_valid
        if (!is.finite(p) || p <= 0) {
          return(NULL)
        }
        
        tibble::tibble(
          country = cty,
          item_id = item_id,
          response = lv,
          n = n_lv,         # raw count for this level
          sum_w = n_valid,  # raw denominator (kept name for compatibility)
          pct = 100 * p
        )
      }))
    })
    
    out <- dplyr::bind_rows(out_cty)
    meta2 <- meta_row |> dplyr::select(domain, question, question_short)
    dplyr::bind_cols(out, meta2[rep(1, nrow(out)), , drop = FALSE])
  })
  
  dplyr::bind_rows(rows)
}
# ── besd_demographics() ────────────────────────────────────────────────────────

#' Extract demographic summaries
#'
#' When `summary(besd_data, include_demographics = TRUE)` is used, demographic
#' summaries are attached to the returned object as an attribute. This helper
#' retrieves that attribute.
#'
#' @param x A result from `summary(besd_data, include_demographics = TRUE)`.
#'
#' @return A tibble of demographic summaries, or `NULL` if not present.
#' @export
besd_summary_demographics <- function(x) {
  attr(x, "demographics")
}



# ── besd_summary_by() ──────────────────────────────────────────────────────────

#' Summarise BeSD items stratified by a demographic variable
#'
#' Produces a per-stratum summary table identical in structure to
#' `summary(besd_data)` but broken down by the levels of a single demographic
#' variable.  All existing logic (weights, CIs, missing-token handling, item
#' types) is preserved because the function works by temporarily using the
#' combined `(country × dem_level)` key as the grouping variable and then
#' splitting it back.
#'
#' @param object A `besd_data` object.
#' @param by_col  Character scalar.  Name of the demographic column in
#'   `object` to stratify by (e.g. `"dem_gen"`).
#' @param ...  Additional arguments forwarded to `summary.besd_data` (e.g.
#'   `conf_level`, `exclude_missing_tokens`).
#'
#' @return A tibble with the same columns as `summary(besd_data)` plus:
#'   \describe{
#'     \item{subgroup_var}{The column name passed to `by_col`.}
#'     \item{subgroup_label}{Human-readable label from `dem_dict` if available,
#'       otherwise equal to `subgroup_var`.}
#'     \item{subgroup_level}{The specific level of `by_col` for each row.}
#'   }
#' @export
besd_summary_by <- function(object, by_col, ...) {
  .assert_besd(object)
  info <- besd_info(object)
  df   <- tibble::as_tibble(object)

  if (!by_col %in% names(df)) {
    .stopf("Column '%s' not found in the besd_data object.", by_col)
  }

  country_col <- info$country_col
  if (is.null(country_col) || !country_col %in% names(df)) {
    df[["..country_tmp"]] <- "national"
    country_col <- "..country_tmp"
  }

  # Drop rows where by_col is NA before grouping
  keep <- !is.na(df[[by_col]])
  if (!any(keep)) .stopf("All values of '%s' are NA.", by_col)
  df <- df[keep, , drop = FALSE]

  # Use a separator that cannot appear in country names or factor labels
  sep <- "\u241F"
  df[["..strata_tmp"]] <- paste(
    as.character(df[[country_col]]),
    as.character(df[[by_col]]),
    sep = sep
  )

  # Construct a temporary besd_data object using the strata column as country
  tmp_obj <- new_besd_data(
    data        = df,
    besd_dict   = attr(object, "besd_dict"),
    dem_dict    = attr(object, "dem_dict"),
    country_col = "..strata_tmp",
    stratum_col = info$stratum_col,
    psu_col     = info$psu_col,
    weight_col  = info$weight_col,
    id_col      = info$id_col,
    besd_items  = info$besd_items,
    dem_items   = info$dem_items,
    meta        = info$meta %||% list()
  )

  sm <- summary(tmp_obj, include_demographics = FALSE, ...)

  # Split the strata key back into country and subgroup_level
  parts             <- strsplit(as.character(sm$country), sep, fixed = TRUE)
  sm$country        <- vapply(parts, `[[`, character(1), 1)
  sm$subgroup_var   <- by_col
  sm$subgroup_level <- vapply(
    parts, function(x) paste(x[-1], collapse = sep), character(1)
  )

  # Human-readable label from dem_dict if available
  dem_dict <- attr(object, "dem_dict")
  subgroup_label <- if (!is.null(dem_dict) && by_col %in% dem_dict$item_id) {
    row <- dem_dict[dem_dict$item_id == by_col, , drop = FALSE]
    val <- dplyr::coalesce(row$question_short[[1]], row$question[[1]], by_col)
    if (is.na(val)) by_col else val
  } else {
    by_col
  }
  sm$subgroup_label <- subgroup_label

  dplyr::relocate(sm, "subgroup_var", "subgroup_label", "subgroup_level",
                  .after = "country")
}


