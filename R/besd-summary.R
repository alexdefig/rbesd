
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


# ── Top-box computation ───────────────────────────────────────────────────────

#' Heuristic identification of top-box response levels
#'
#' Applies a sequence of regular expressions to a vector of response labels to
#' identify the most positive (top-box) response(s).  Used internally by
#' \code{besd_topbox()} but also available for direct use or testing.
#'
#' @param responses A character or factor vector of response option labels.
#'
#' @return A character vector containing the top-box response label(s): the
#'   last level for ordered or unordered factors, or the last unique value for
#'   plain character vectors.
#'
#' @examples
#' besd_guess_topbox_levels(c("No", "Unsure", "Yes"))
#' besd_guess_topbox_levels(c("Disagree", "Neutral", "Agree", "Strongly agree"))
#'
#' @export
besd_guess_topbox_levels <- function(responses) {
  if (!length(responses)) return(character())
  if (is.factor(responses)) return(tail(levels(responses), 1))
  tail(unique(as.character(responses)), 1)
}


#' Compute top-box percentages from a BeSD summary tibble
#'
#' Collapses a full response-distribution summary into a single top-box
#' percentage per country x item, respecting the \code{reverse} flag in the
#' item dictionary (reversed items use the most negative response as the
#' "positive" indicator).  Wilson-score 95\% confidence intervals are appended.
#'
#' @param sum_tbl A \code{besd_summary_tbl} (output of \code{summary()}).
#' @param topbox_levels Optional named list mapping item IDs to character
#'   vectors of response labels that should be counted as top-box.  Overrides
#'   automatic detection for the named items.
#' @param include_item_types Character vector of item types to retain before
#'   computing top-box scores.  Multichoice items are always excluded.
#'   Default \code{c("binary", "ordinal", "categorical", "unknown")}.
#'
#' @return A tibble with one row per country x item containing columns
#'   \code{country}, \code{item_id}, \code{pct} (top-box \%), \code{lcl},
#'   \code{ucl} (95\% CI bounds), \code{topbox_label}, \code{question_short},
#'   and domain metadata.
#'
#' @examples
#' data("data_demo", package = "rbesd")
#' x <- as_besd(data_demo, country_col = "country")
#' s <- summary(x)
#' tb <- besd_topbox(s)
#' tb
#'
#' @export
besd_topbox <- function(sum_tbl,
                        topbox_levels = NULL,
                        include_item_types = c("binary", "ordinal",
                                               "categorical", "unknown")) {
  .assert_besd_summary_tbl(sum_tbl, fn = "besd_topbox")
  sum_tbl <- .coerce_country(sum_tbl)

  besd_dict <- attr(sum_tbl, "besd_dict")
  dem_dict  <- attr(sum_tbl, "dem_dict")

  dict <- dplyr::bind_rows(
    if (is.null(besd_dict)) tibble::tibble()
    else tibble::as_tibble(besd_dict),
    if (is.null(dem_dict)) tibble::tibble()
    else tibble::as_tibble(dem_dict)
  )

  dd <- sum_tbl
  if (!is.null(include_item_types)) {
    dd <- dd |> dplyr::filter(.data$item_type %in% include_item_types)
  }
  dd <- dd |> dplyr::filter(.data$item_type != "multichoice")

  item_levels <- dd |>
    dplyr::group_by(.data$item_id) |>
    dplyr::summarise(.levels = list(unique(.data$response)), .groups = "drop")

  pick_levels <- function(item_id, levs) {
    if (!is.null(topbox_levels) && item_id %in% names(topbox_levels)) {
      return(topbox_levels[[item_id]])
    }

    if (nrow(dict) > 0 && item_id %in% dict$item_id) {
      item_dict <- dict[dict$item_id == item_id, ]
      is_reversed <- isTRUE(item_dict$reverse[1])

      if (is_reversed) {
        if (is.factor(levs)) {
          return(head(levels(levs), 1))
        } else {
          return(head(as.character(levs), 1))
        }
      }
    }

    besd_guess_topbox_levels(levs)
  }

  item_levels$topbox <- mapply(pick_levels, item_levels$item_id,
                               item_levels$.levels,
                               SIMPLIFY = FALSE, USE.NAMES = FALSE)

  dd2 <- dd |>
    dplyr::left_join(item_levels |> dplyr::select(.data$item_id,
                                                  .data$topbox),
                     by = "item_id") |>
    dplyr::mutate(is_topbox = purrr::map2_lgl(.data$response, .data$topbox,
                                              ~ .x %in% .y))

  out <- dd2 |>
    dplyr::group_by(.data$country, .data$item_id) |>
    dplyr::summarise(
      item_type = dplyr::first(.data$item_type),
      question_short = dplyr::first(.data$question_short),
      question = dplyr::first(if ("question" %in% names(dd2)) .data$question
                              else .data$question_short),
      domain = dplyr::first(if ("domain" %in% names(dd2)) .data$domain
                            else NA_character_),
      topbox_label = paste(unique(unlist(.data$topbox)), collapse = ", "),
      pct = sum(.data$pct[.data$is_topbox], na.rm = TRUE),
      sum_w = dplyr::first(if ("sum_w" %in% names(dd2)) .data$sum_w
                           else NA_real_),
      n_eff = dplyr::first(if ("n_eff" %in% names(dd2)) .data$n_eff
                           else NA_real_),
      n = dplyr::first(if ("n" %in% names(dd2)) .data$n else NA_real_),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      p = .data$pct / 100,
      n_denom = dplyr::coalesce(.data$n_eff, .data$n),
      se = sqrt(pmax(.data$p * (1 - .data$p), 0) / pmax(.data$n_denom, 1)),
      z = stats::qnorm(0.975),
      lcl = pmax(0, (.data$p - z * .data$se) * 100),
      ucl = pmin(100, (.data$p + z * .data$se) * 100)
    ) |>
    dplyr::select(-p, -n_denom, -se, -z)

  out
}


