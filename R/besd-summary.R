
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
#' CI method:
#' - `"dirichlet"`: For mutually-exclusive categorical/ordinal/binary items, uses
#'   Dirichlet-marginal intervals with effective sample size `n_eff` (Kish) and a
#'   symmetric Dirichlet prior controlled by `ci_prior`. For multichoice (non-
#'   exclusive) items, uses Jeffreys/Beta marginal intervals per token.
#' - `"none"`: returns `lcl`/`ucl` as `NA`.
#'
#' @param object A `besd_data` object.
#' @param items Optional character vector of item IDs to summarise.
#' @param conf_level Confidence level for intervals (default 0.95).
#' @param include_demographics If `TRUE` and demographics exist, also compute
#'   raw (unweighted) percentages for demographic items and attach them as an
#'   attribute (see `besd_demographics()`).
#' @param exclude_missing_tokens If `TRUE`, any `missing_tokens` supplied to
#'   `as_besd()` are excluded from denominators even if they were kept as levels.
#' @param ci_method Interval method for proportions: `"dirichlet"` or `"none"`.
#'   (Default is `"dirichlet"`.)
#' @param ci_prior Prior concentration for Dirichlet/Beta intervals (default 0.5).
#' @param ... Unused; included for S3 compatibility.
#'
#' @return A tibble with columns including `country`, `item_id`, `response`,
#'   `pct`, `lcl`, `ucl`, and metadata columns from the item dictionary. If
#'   `include_demographics = TRUE`, a demographics tibble is attached as an
#'   attribute (see `besd_demographics()`).
#' @export
summary.besd_data <- function(object, 
                              items = NULL, 
                              conf_level = 0.95,
                              include_demographics = TRUE, 
                              exclude_missing_tokens = FALSE,
                              ci_method = c("dirichlet", "none"), 
                              ci_prior = 0.5,
                              ...) {
  .assert_besd(object)
  
  ci_method <- match.arg(ci_method)
  
  info <- besd_info(object)
  
  besd_dict <- tibble::as_tibble(info$besd_dict)
  dem_dict <- if (is.null(info$dem_dict)) NULL else tibble::as_tibble(info$dem_dict)
  
  # Which items to summarise
  besd_items <- info$besd_items %||% intersect(besd_dict$item_id, names(object))
  if (!is.null(items)) {
    besd_items <- intersect(besd_items, items)
  }
  
  if (!length(besd_items)) {
    .stopf("No BeSD items present to summarise.")
  }
  
  # Base data
  df <- tibble::as_tibble(object)
  country_col <- info$country_col
  weight_col <- info$weight_col
  
  # Allow 'national' summary even if country column is absent
  if (is.null(country_col) || !(country_col %in% names(df))) {
    df$..country_tmp <- "national"
    country_col <- "..country_tmp"
  }
  
  # Provide default weights if missing
  if (is.null(weight_col) || !(weight_col %in% names(df))) {
    df$..weight_tmp <- 1
    weight_col <- "..weight_tmp"
  }
  
  meta <- info$meta %||% list()
  missing_tokens <- meta$missing_tokens %||% NULL
  multichoice_specs <- meta$multichoice_specs %||% list()
  
  out <- dplyr::bind_rows(lapply(besd_items, function(item_id) {
    meta_row <- besd_dict[besd_dict$item_id == item_id, , drop = FALSE]
    if (!nrow(meta_row)) {
      return(NULL)
    }
    
    .besd_summarise_one_item(
      df,
      item_id = item_id,
      meta_row = meta_row,
      country_col = country_col,
      weight_col = weight_col,
      conf_level = conf_level,
      multichoice_specs = multichoice_specs,
      missing_tokens = missing_tokens,
      exclude_missing_tokens = exclude_missing_tokens,
      ci_method = ci_method,
      ci_prior = ci_prior
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


# ── Helpers ────────────────────────────────────────────────────────────────────
#' @keywords internal
.besd_summarise_one_item <- function(df, item_id, meta_row, country_col, weight_col,
                                     conf_level = 0.95, multichoice_specs = list(),
                                     missing_tokens = NULL, exclude_missing_tokens = TRUE,
                                     ci_method = c("dirichlet", "none"),
                                     ci_prior = 0.5) {
  ci_method <- match.arg(ci_method)
  
  item_type <- meta_row$item_type[[1]]
  levels_std <- meta_row$levels[[1]]
  
  # Wwhen keeping missing tokens, extend levels_std to include them
  if (!exclude_missing_tokens) {
    miss_i <- .besd_missing_tokens_for_item(missing_tokens, item_id)
    if (!is.null(miss_i) && length(miss_i)) {
      levels_std <- unique(c(levels_std, as.character(miss_i)))
    }
  }
  
  x <- df[[item_id]]
  w <- if (!is.null(weight_col) && weight_col %in% names(df)) {
    df[[weight_col]]
  } else {
    rep(1, nrow(df))
  }
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
  
  idx <- split(seq_len(nrow(df)), cc)
  
  # Mutually-exclusive items: compute p-vector once, CI-vector once
  if (item_type %in% c("binary", "ordinal", "categorical")) {
    rows <- lapply(names(idx), function(cty) {
      ii <- idx[[cty]]
      xi <- x_chr[ii]
      wi <- w[ii]
      
      ok <- !is.na(xi) & !is.na(wi)
      xi <- xi[ok]
      wi <- wi[ok]
      
      n <- length(xi)
      sw <- if (n) sum(wi) else 0
      n_eff <- if (n) .effective_n(wi) else NA_real_
      
      if (!n || sw <= 0) {
        return(tibble::tibble(
          country = cty,
          item_id = item_id,
          item_type = item_type,
          response = levels_std,
          n = 0L,
          sum_w = 0,
          n_eff = NA_real_,
          pct = NA_real_,
          lcl = NA_real_,
          ucl = NA_real_
        ))
      }
      
      p_vec <- vapply(levels_std, function(lv) {
        sum(wi[xi == lv], na.rm = TRUE) / sw
      }, numeric(1))
      
      if (ci_method == "none") {
        ci_mat <- cbind(
          lcl = rep(NA_real_, length(p_vec)),
          ucl = rep(NA_real_, length(p_vec))
        )
      } else {
        ci_mat <- .multinom_dirichlet_ci(p_vec, n_eff, conf_level = conf_level, 
                                         prior = ci_prior)
      }
      
      tibble::tibble(
        country = cty,
        item_id = item_id,
        item_type = item_type,
        response = levels_std,
        n = n,
        sum_w = sw,
        n_eff = n_eff,
        n_resp = vapply(levels_std, function(lv) sum(xi == lv, na.rm = TRUE), integer(1)),
        pct = 100 * p_vec,
        lcl = 100 * ci_mat[, "lcl"],
        ucl = 100 * ci_mat[, "ucl"]
      )
    })
    
    out <- dplyr::bind_rows(rows)
    meta2 <- meta_row |> dplyr::select(domain, question, question_short)
    out <- dplyr::bind_cols(out, meta2[rep(1, nrow(out)), , drop = FALSE])
    return(out)
  }
  
  # Multichoice: per-token Bernoulli selection (not multinomial)
  if (item_type == "multichoice") {
    spec <- multichoice_specs[[item_id]] %||% list()
    sep <- spec$sep %||% .BESD_SEP
    
    rows <- lapply(names(idx), function(cty) {
      ii <- idx[[cty]]
      xi <- x_chr[ii]
      wi <- w[ii]
      
      ok <- !is.na(xi) & !is.na(wi)
      xi <- xi[ok]
      wi <- wi[ok]
      
      n <- length(xi)
      sw <- if (n) sum(wi) else 0
      n_eff <- if (n) .effective_n(wi) else NA_real_
      
      tok_list <- lapply(xi, function(s) strsplit(s, sep, fixed = TRUE)[[1]])
      
      dplyr::bind_rows(lapply(levels_std, function(tok) {
        if (!n || sw <= 0) {
          return(tibble::tibble(
            country = cty,
            item_id = item_id,
            item_type = item_type,
            response = tok,
            n = 0L,
            sum_w = 0,
            n_eff = NA_real_,
            pct = NA_real_,
            lcl = NA_real_,
            ucl = NA_real_
          ))
        }
        
        sel <- vapply(tok_list, function(v) tok %in% v, logical(1))
        p <- sum(wi[sel], na.rm = TRUE) / sw
        
        if (ci_method == "none") {
          ci <- c(lcl = NA_real_, ucl = NA_real_)
        } else {
          ci <- .beta_binom_ci(p, n_eff, conf_level = conf_level, prior = ci_prior)
        }
        
        tibble::tibble(
          country = cty,
          item_id = item_id,
          item_type = item_type,
          response = tok,
          n = n,
          sum_w = sw,
          n_eff = n_eff,
          n_resp = sum(sel, na.rm = TRUE),
          pct = 100 * p,
          lcl = 100 * ci[["lcl"]],
          ucl = 100 * ci[["ucl"]]
        )
      }))
    })
    
    out <- dplyr::bind_rows(rows)
    meta2 <- meta_row |> dplyr::select(domain, question, question_short)
    out <- dplyr::bind_cols(out, meta2[rep(1, nrow(out)), , drop = FALSE])
    return(out)
  }
  
  .stopf("Unsupported item_type in summary: `%s`.", item_type)
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


