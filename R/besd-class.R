# besd-class.R

#' Construct a `besd_data` object
#'
#' Internal constructor used by `as_besd()`. Attaches dictionaries and metadata
#' as attributes.
#'
#' @param data A data frame / tibble containing, at a minimum, BeSD item columns. If
#'  dem_dict is provided then these variables should also be provided.
#' @param besd_dict A BeSD dictionary describing items and levels, see
#'  `besd-dictionary.R`.
#' @param dem_dict Optional demographic dictionary (data frame) describing
#'   predictors, see `besd_dictionary.R`
#' @param country_col Name of the country column in `data`.
#' @param weight_col Optional name of the survey weight column.
#' @param id_col Optional name of a respondent ID column.
#' @param mapping Optional mapping object used by the package.
#' @param besd_items Optional character vector of BeSD item IDs to treat as items.
#' @param dem_items Optional character vector of demographic item IDs.
#' @param meta Named list of additional metadata (e.g. missing tokens, specs).
#'
#' @return An object of class `besd_data`.
#' @keywords internal
new_besd_data <- function(data, besd_dict, dem_dict, country_col, weight_col = NULL,
                          id_col = NULL, mapping = NULL, besd_items = NULL,
                          dem_items = NULL, meta = list()) {
  # Input arg checks
  if (!is.data.frame(data)) .stopf("`data` must be a data.frame or tibble.")
  if (!is.data.frame(besd_dict)) .stopf("`besd_dict` must be a data.frame.")
  if (!is.null(dem_dict) && !is.data.frame(dem_dict)) {
    .stopf("`dem_dict` must be NULL or a data.frame.")
  }
  
  .assert_has_cols(data, country_col, nm = "country_col", scalar = TRUE)
  .assert_has_cols(data, weight_col, nm = "weight_col", scalar = TRUE, null_ok = TRUE)
  .assert_has_cols(data, id_col, nm = "id_col", scalar = TRUE, null_ok = TRUE)
  .assert_has_cols(data, besd_items, nm = "besd_items", null_ok = TRUE, strict = TRUE)
  .assert_has_cols(data, dem_items, nm = "dem_items", null_ok = TRUE, strict = TRUE)
  
  if (!is.list(meta)) .stopf("`meta` must be a list.")
  
  # Construct an S3 `besd_data` object and store dicts + metadata as attributes
  structure(
    data,
    class = c("besd_data", class(data)),
    besd_dict = besd_dict,
    dem_dict = dem_dict,
    besd_items = besd_items,
    dem_items = dem_items,
    besd_country_col = country_col,
    besd_weight_col = weight_col,
    besd_id_col = id_col,
    besd_mapping = mapping,
    besd_meta = meta
  )
}

#' Validate a `besd_data` object
#'
#' Performs integrity checks on `besd_data` objects, including presence of required
#' attributes, dictionary consistency, and item-level type/level checks.
#'
#' @param x A `besd_data` object.
#' @param strict If `TRUE`, validation issues raise an error; otherwise warn.
#'
#' @return Invisibly returns `TRUE` if validation passes, otherwise `FALSE`.
#' @export
besd_validate <- function(x, strict = FALSE) {
  
  issues <- character(0)
  
  add_issue <- function(msg) issues <<- c(issues, msg)
  
  finish <- function() {
    if (!length(issues)) {
      return(invisible(TRUE))
    }
    msg <- paste(
      "BeSD validation issues:",
      paste0("- ", issues, collapse = "\n"),
      sep = "\n"
    )
    if (isTRUE(strict)) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
    invisible(FALSE)
  }
  
  if (!inherits(x, "besd_data")) {
    stop("`x` must be a `besd_data` object. Use `as_besd()`.", call. = FALSE)
  }
  
  dict <- attr(x, "besd_dict")
  dem_dict <- attr(x, "dem_dict")
  country_col <- attr(x, "besd_country_col")
  weight_col <- attr(x, "besd_weight_col")
  id_col <- attr(x, "besd_id_col")
  meta <- attr(x, "besd_meta")
  besd_items <- attr(x, "besd_items")
  dem_items <- attr(x, "dem_items")
  
  if (is.null(meta)) meta <- list()
  
  if (is.null(dict) || !is.data.frame(dict)) {
    add_issue("Missing or invalid `besd_dict` attribute (must be a data.frame).")
    return(finish())
  }
  
  req_cols <- c("item_id", "item_type", "levels")
  miss_cols <- setdiff(req_cols, names(dict))
  if (length(miss_cols)) {
    add_issue(
      sprintf(
        "Dictionary missing required column(s): %s",
        paste(miss_cols, collapse = ", ")
      )
    )
  }
  
  if (anyDuplicated(dict$item_id)) {
    dup <- unique(dict$item_id[duplicated(dict$item_id)])
    add_issue(
      sprintf("Dictionary contains duplicate item_id(s): %s",
              paste(dup, collapse = ", "))
    )
  }
  
  if (is.null(country_col) || !nzchar(country_col)) {
    add_issue("Missing `besd_country_col` attribute.")
  } else if (!country_col %in% names(x)) {
    add_issue(sprintf("Country column `%s` not found in data.", country_col))
  }
  
  if (!is.null(weight_col) && nzchar(weight_col) && !weight_col %in% names(x)) {
    add_issue(sprintf("Weight column `%s` not found in data.", weight_col))
  }
  
  if (!is.null(id_col) && nzchar(id_col) && !id_col %in% names(x)) {
    add_issue(sprintf("ID column `%s` not found in data.", id_col))
  }
  
  if (!is.null(dem_dict) && !is.data.frame(dem_dict)) {
    add_issue("`dem_dict` attribute is present but not a data.frame.")
  }
  
  allowed_types <- c("binary", "ordinal", "categorical", "multichoice")
  bad_types <- setdiff(unique(dict$item_type), allowed_types)
  if (length(bad_types)) {
    add_issue(
      sprintf(
        "Dictionary has unknown item_type(s): %s",
        paste(bad_types, collapse = ", ")
      )
    )
  }
  
  items <- intersect(dict$item_id, names(x))
  if (!length(items)) {
    add_issue("No dictionary items found in data (item_id not present as columns).")
    return(finish())
  }
  
  if (!is.null(besd_items)) {
    miss_besd <- setdiff(besd_items, names(x))
    if (length(miss_besd)) {
      add_issue(
        sprintf(
          "`besd_items` attribute lists missing columns: %s",
          paste(miss_besd, collapse = ", ")
        )
      )
    }
  }
  
  if (!is.null(dem_items)) {
    miss_dem <- setdiff(dem_items, names(x))
    if (length(miss_dem)) {
      add_issue(
        sprintf(
          "`dem_items` attribute lists missing columns: %s",
          paste(miss_dem, collapse = ", ")
        )
      )
    }
  }
  
  missing_tokens <- meta$missing_tokens
  missing_action <- meta$missing_action %||% "na"
  sep_default <- if (exists(".BESD_SEP", inherits = TRUE)) .BESD_SEP else " | "
  
  for (item_id in items) {
    meta_row <- dict[dict$item_id == item_id, , drop = FALSE]
    if (nrow(meta_row) != 1L) {
      add_issue(sprintf("Dictionary has %d rows for item_id `%s`.",
                        nrow(meta_row), item_id))
      next
    }
    
    item_type <- meta_row$item_type[[1]]
    levels_std <- meta_row$levels[[1]]
    
    if (is.null(levels_std) || !length(levels_std)) {
      add_issue(sprintf("Dictionary levels missing/empty for item `%s`.", item_id))
      next
    }
    
    v <- x[[item_id]]
    
    if (all(is.na(v))) {
      add_issue(sprintf("Item `%s` is entirely missing (all NA).", item_id))
    }
    
    miss_i <- .besd_missing_tokens_for_item(missing_tokens, item_id)
    keep_missing <- identical(missing_action, "keep") && !is.null(miss_i)
    
    allowed_levels <- levels_std
    if (isTRUE(keep_missing)) {
      allowed_levels <- unique(c(allowed_levels, as.character(miss_i)))
    }
    
    if (item_type %in% c("binary", "categorical")) {
      if (!is.factor(v) || is.ordered(v)) {
        add_issue(sprintf("Item `%s` should be an unordered factor.", item_id))
        next
      }
      
      bad_lev <- setdiff(levels(v), allowed_levels)
      if (length(bad_lev)) {
        add_issue(
          sprintf(
            "Item `%s` has factor level(s) not in dictionary: %s",
            item_id, paste(bad_lev, collapse = ", ")
          )
        )
      }
      
      if (identical(item_type, "binary") && nlevels(v) > 2L) {
        add_issue(sprintf("Binary item `%s` has >2 factor levels.", item_id))
      }
    }
    
    if (identical(item_type, "ordinal")) {
      if (!is.factor(v) || !is.ordered(v)) {
        add_issue(sprintf("Item `%s` should be an ordered factor.", item_id))
        next
      }
      
      bad_lev <- setdiff(levels(v), allowed_levels)
      if (length(bad_lev)) {
        add_issue(
          sprintf(
            "Item `%s` has ordered level(s) not in dictionary: %s",
            item_id, paste(bad_lev, collapse = ", ")
          )
        )
      }
    }
    
    if (identical(item_type, "multichoice")) {
      if (!is.character(v)) {
        add_issue(sprintf("Multichoice item `%s` should be character.", item_id))
        next
      }
      
      spec <- (meta$multichoice_specs %||% list())[[item_id]] %||% list()
      sep <- spec$sep %||% sep_default
      
      bad_tok <- character(0)
      for (s in v[!is.na(v)]) {
        toks <- strsplit(s, sep, fixed = TRUE)[[1]]
        toks <- toks[toks != ""]
        if (!length(toks)) next
        bad_tok <- c(bad_tok, setdiff(toks, allowed_levels))
      }
      bad_tok <- unique(bad_tok)
      
      if (length(bad_tok)) {
        add_issue(
          sprintf(
            "Multichoice item `%s` has token(s) not in dictionary: %s",
            item_id, paste(bad_tok, collapse = ", ")
          )
        )
      }
    }
  }
  
  if (!is.null(weight_col) && nzchar(weight_col) && weight_col %in% names(x)) {
    w <- x[[weight_col]]
    if (!is.numeric(w)) {
      add_issue(sprintf("Weight column `%s` should be numeric.", weight_col))
    } else {
      if (any(is.na(w))) add_issue("Weights contain NA.")
      if (any(w < 0, na.rm = TRUE)) add_issue("Weights contain negative values.")
      if (sum(w, na.rm = TRUE) <= 0) add_issue("Sum of weights is not positive.")
    }
  }
  
  finish()
}

#' Print a `besd_data` object
#'
#' Displays a short summary of the object, including row count and which columns
#' are treated as BeSD and demographic items.
#'
#' @param x A `besd_data` object.
#' @param ... Unused; included for S3 compatibility.
#'
#' @return `x`, invisibly.
#' @export
print.besd_data <- function(x, ...) {
  info <- besd_info(x)
  country_col <- info$country_col
  
  n_countries <- if (!is.null(country_col) && country_col %in% names(x)) {
    length(unique(x[[country_col]]))
  } else {
    NA
  }
  
  n_besd <- length(intersect(info$besd_dict$item_id, names(x)))
  n_dem <- if (is.null(info$dem_dict)) {
    0L
  } else {
    length(intersect(info$dem_dict$item_id, names(x)))
  }
  
  w_col <- if (is.null(info$weight_col)) "none" else info$weight_col
  i_col <- if (is.null(info$id_col)) "none" else info$id_col
  
  cat("<besd_data>\n")
  cat(sprintf("- Rows: %s\n", format(nrow(x), big.mark = ",")))
  cat(sprintf("- Countries: %s\n", n_countries))
  cat(sprintf("- BeSD items present: %s\n", n_besd))
  cat(sprintf("- Predictor items present: %s\n", n_dem))
  cat(sprintf("- Weight column: %s\n", w_col))
  cat(sprintf("- ID column: %s\n", i_col))
  
  invisible(x)
}

#' Access BeSD metadata
#'
#' Returns dictionaries, column names, and metadata stored as attributes on a
#' `besd_data` object.
#'
#' @param x A `besd_data` object.
#'
#' @return A named list with entries such as `besd_dict`, `dem_dict`,
#'   `country_col`, `weight_col`, and `meta`.
#' @export
besd_info <- function(x) {
  .assert_besd(x)
  list(
    besd_dict = attr(x, "besd_dict"),
    dem_dict = attr(x, "dem_dict"),
    besd_items = attr(x, "besd_items"),
    dem_items = attr(x, "dem_items"),
    country_col = attr(x, "besd_country_col"),
    weight_col = attr(x, "besd_weight_col"),
    id_col = attr(x, "besd_id_col"),
    mapping = attr(x, "besd_mapping"),
    meta = attr(x, "besd_meta")
  )
}


#' Print a BeSD summary tibble
#'
#' Prints the tibble and, if present, indicates how to access attached
#' demographic summaries.
#'
#' @param x A `besd_summary_tbl` object (from `summary(besd_data)`).
#' @param ... Passed to the next print method.
#'
#' @return `x`, invisibly.
#' @export
print.besd_summary_tbl <- function(x, ...) {
  NextMethod()
  
  dem <- attr(x, "demographics")
  if (!is.null(dem) && nrow(dem)) {
    cat(
      "\n(demographics available via `besd_demographics(x)` or ",
      "`attr(x, 'demographics')`)\n",
      sep = ""
    )
  }
  
  invisible(x)
}
