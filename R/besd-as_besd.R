
# ── as_besd() ──────────────────────────────────────────────────────────────────

#' Prepare and standardise WHO/UNICEF BeSD data
#'
#' @param df A data frame containing raw BeSD survey data.
#' @param besd_dict BeSD item dictionary.
#' @param dem_dict Optional: predictor dictionary.
#' @param mapping Named character vector: names=raw variables; values=standard item_id.
#' @param multichoice_specs Specs for multichoice when multiple raw cols map to one
#'  item_id.
#' @param country_col Country column name.
#' @param weight_col Optional name of the survey weight column in `df`. Weights are
#'   used by `summary.besd_data()` when computing weighted proportions, but are
#'   **not** currently used by `besd_regress()` (see its documentation).
#' @param id_col Optional respondent id column.
#' @param keep_original If TRUE, keep all original columns (plus harmonised ones).
#' @param missing_tokens Specifies which response values should be treated as
#'   missing data. Accepts two forms:
#'   - **Character vector**: the same tokens are applied to every item, e.g.
#'     `missing_tokens = c("Don't know", "Refused")`.
#'   - **Named list**: per-item token vectors, where names are `item_id` values.
#'     The special name `.all` defines a cross-item default that is *combined*
#'     (not replaced) with any item-specific entries. For example:
#'     ```r
#'     missing_tokens = list(
#'       .all      = "Don't know",        # applied to every item
#'       sp_peer   = c("Refused", "N/A"), # additionally for sp_peer only
#'       tf_safety = "Not applicable"     # additionally for tf_safety only
#'     )
#'     ```
#'     Here `sp_peer` treats `"Don't know"`, `"Refused"`, and `"N/A"` all as
#'     missing; `tf_safety` treats `"Don't know"` and `"Not applicable"` as
#'     missing; all other items treat only `"Don't know"` as missing.
#' @param missing_action One of `"na"` (default; recode missing tokens to `NA`
#'   after coercion) or `"keep"` (retain them as explicit trailing factor levels).
#'   Use `"keep"` when you want missing-response rows to appear in
#'   `summary.besd_data()` output as distinct categories.
#' @param unknown_action One of `"error"` (default; stop if values not in
#'   dictionary) or `"na"` (coerce unknown/unmapped values to `NA` with an
#'   optional warning). Note: `missing_tokens` are allow-listed and never
#'   treated as unknown, regardless of this setting.
#' @param warn_on_unknown If `TRUE` (default), emit a warning when
#'   `unknown_action = "na"` coerces unrecognised values.
#'
#' @section Missing data handling:
#' `as_besd()` distinguishes three kinds of non-response:
#'
#' 1. **System-missing** (`NA` in input): passed through as `NA` throughout;
#'    no special treatment required.
#' 2. **Missing tokens** — survey-specific non-response codes (`"Don't know"`,
#'    `"Refused"`, etc.) declared via `missing_tokens`. With
#'    `missing_action = "na"` these become `NA`; with `"keep"` they are
#'    retained as explicit factor levels so they remain visible in summaries.
#'    The choice depends on whether you want to count or display non-response.
#' 3. **Unknown values** — responses absent from the dictionary entirely
#'    (data-entry errors, unmapped codes). Controlled by `unknown_action`.
#'
#' In `summary.besd_data()`, the `n` column reflects valid responses *after*
#' missing-token exclusion (when `exclude_missing_tokens = TRUE`, the default).
#' There is currently no dedicated `n_missing` column; if you need to report
#' non-response rates, use `missing_action = "keep"` so that missing-token rows
#' appear explicitly in the summary output.
#'
#' @return A `besd_data` object.
#' @examples
#' \dontrun{
#' # 1. Uniform missing tokens across all items
#' x <- as_besd(
#'   df,
#'   missing_tokens = c("Don't know", "Refused"),
#'   missing_action = "na"
#' )
#'
#' # 2. Per-variable missing tokens with a cross-item default
#' x <- as_besd(
#'   df,
#'   missing_tokens = list(
#'     .all      = "Don't know",          # every item
#'     sp_peer   = c("Refused", "N/A"),   # sp_peer gets these too
#'     tf_safety = "Not applicable"       # tf_safety gets this too
#'   ),
#'   missing_action = "na"
#' )
#'
#' # 3. Keep missing tokens as explicit levels (they appear in summary output)
#' x <- as_besd(
#'   df,
#'   missing_tokens = "Don't know",
#'   missing_action = "keep"
#' )
#' summary(x)  # "Don't know" appears as a response row with its own pct
#'
#' # 4. Recode missing tokens later (e.g. after inspecting the data)
#' x <- as_besd(df, missing_action = "keep", missing_tokens = "Don't know")
#' x <- besd_recode_missing(x, tokens = "Don't know")
#' }
#' @export
as_besd <- function(df,
                    besd_dict         = besd_dictionary(),
                    dem_dict          = NULL,
                    mapping           = NULL,
                    multichoice_specs = list(),
                    country_col       = "country",
                    weight_col        = NULL,
                    id_col            = NULL,
                    keep_original     = FALSE,
                    missing_tokens    = NULL,
                    missing_action    = c("na", "keep"),
                    unknown_action    = c("error", "na"),
                    warn_on_unknown   = TRUE) {
  
  missing_action <- match.arg(missing_action)
  unknown_action <- match.arg(unknown_action)
  
  .assert_is_scalar_string(country_col, "country_col")
  .assert_has_cols(df, country_col, "df")
  .assert_valid_dict(besd_dict)
  if (!is.data.frame(df))   .stopf("`df` must be a data.frame.")
  if (!is.null(weight_col)) .assert_has_cols(df, weight_col, "df")
  if (!is.null(id_col))     .assert_has_cols(df, id_col, "df")
  if (!is.null(dem_dict))   .assert_valid_dict(dem_dict)
  
  # Validate missing_tokens: NULL | character vector | named list
  if (!is.null(missing_tokens) && !is.character(missing_tokens)) {
    if (!is.list(missing_tokens) ||
        is.null(names(missing_tokens)) ||
        any(names(missing_tokens) == "")) {
      .stopf("`missing_tokens` must be NULL, a character vector, or a *named* list.")
    }
  }
  
  dict <- tibble::as_tibble(besd_dict)
  if (!is.null(dem_dict)) dict <- dplyr::bind_rows(dict, tibble::as_tibble(dem_dict))
  .assert_valid_dict(dict)
  
  # Apply mapping only if provided; otherwise assume df already has item_id cols
  df2 <- if (is.null(mapping)) df else {
    .apply_dict_mapping(df, dict, mapping, multichoice_specs)
  }
  
  items <- intersect(dict$item_id, names(df2))
  if (!length(items)) {
    .stopf("No dict items found in `df` after mapping (or in raw df if mapping=NULL).")
  }
  
  # Step 1 — Coerce to dictionary types/levels only.
  # missing_tokens are passed solely so they are allow-listed and not flagged as
  # unknown values. Tokens are always kept as explicit trailing factor levels at
  # this stage; recoding them to NA is handled separately in Step 2.
  df2 <- .coerce_dict_items(
    df2,
    dict              = dict,
    items             = items,
    multichoice_specs = multichoice_specs,
    missing_tokens    = missing_tokens,
    unknown_action    = unknown_action,
    warn_on_unknown   = warn_on_unknown
  )
  
  # Step 2 — Handle missing tokens
  # missing_action == "keep": tokens remain as explicit factor levels; nothing to do.
  if (missing_action == "na" && !is.null(missing_tokens)) {
    df2 <- besd_recode_missing(
      x                 = df2,
      tokens            = missing_tokens,
      items             = items,
      dict              = dict,
      multichoice_specs = multichoice_specs,
      drop_levels       = TRUE
    )
  }
  
  # Keep all original data or only core columns
  if (!isTRUE(keep_original)) {
    keep <- unique(c(country_col, weight_col, id_col, items))
    keep <- keep[!is.null(keep) & !is.na(keep) & nzchar(keep)]
    df2  <- df2[, keep, drop = FALSE]
  }
  
  besd_items <- intersect(tibble::as_tibble(besd_dict)$item_id, names(df2))
  dem_items  <- if (is.null(dem_dict)) character(0) else {
    intersect(tibble::as_tibble(dem_dict)$item_id, names(df2))
  }
  
  new_besd_data(
    data        = tibble::as_tibble(df2),
    besd_dict   = besd_dict,
    dem_dict    = dem_dict,
    besd_items  = besd_items,
    dem_items   = dem_items,
    country_col = country_col,
    weight_col  = weight_col,
    id_col      = id_col,
    mapping     = mapping,
    meta = list(
      prepared_at       = as.character(Sys.time()),
      multichoice_specs = multichoice_specs,
      missing_tokens    = missing_tokens,
      missing_action    = missing_action,
      unknown_action    = unknown_action
    )
  )
}


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
  
  # Re-wrap as besd_data preserving all attributes
  info <- besd_info(x)
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

# ── Helpers ────────────────────────────────────────────────────────────────────

.apply_dict_mapping <- function(df, dict, mapping, multichoice_specs = list()) {
  
  if (is.null(mapping)) .stopf("No `mapping` provided.")
  
  if (!is.character(mapping) ||
      is.null(names(mapping)) || any(names(mapping) == "")) {
    .stopf("`mapping` must be *named* char vector: names = raw cols, values = item_id")
  }
  
  missing_raw <- setdiff(names(mapping), names(df))
  if (length(missing_raw))
    .stopf("Mapping refers to missing raw cols: %s", paste(missing_raw, collapse = ", "))
  
  map_groups <- split(names(mapping), mapping)  # item_id -> raw cols
  
  for (item_id in names(map_groups)) {
    if (!item_id %in% dict$item_id)
      .stopf("Mapped item_id `%s` not found in dictionary.", item_id)
    
    raws       <- map_groups[[item_id]]
    meta       <- dict[dict$item_id == item_id, , drop = FALSE]
    item_type  <- meta$item_type[[1]]
    levels_std <- meta$levels[[1]]
    
    if (length(raws) == 1L) { df[[item_id]] <- df[[raws]]; next }
    
    if (item_type != "multichoice")
      .stopf("Multiple raw cols mapped to `%s` but item_type is `%s`", item_id, item_type)
    
    spec          <- multichoice_specs[[item_id]] %||% list()
    df[[item_id]] <- .pack_multichoice_cols(df, raws, levels_std, spec)
  }
  
  df
}

# Coerce items to dictionary types and levels.
# missing_tokens are allow-listed so they are not flagged as unknown values.
# They are always retained as explicit trailing factor levels here; callers
# that want them recoded to NA should call besd_recode_missing() afterwards.
.coerce_dict_items <- function(df,
                               dict,
                               items,
                               multichoice_specs = list(),
                               missing_tokens    = NULL,
                               unknown_action    = c("error", "na"),
                               warn_on_unknown   = TRUE) {
  
  unknown_action <- match.arg(unknown_action)
  
  for (item in items) {
    meta   <- dict[dict$item_id == item, , drop = FALSE]
    miss_i <- .besd_missing_tokens_for_item(missing_tokens, item)
    df[[item]] <- .coerce_item(
      df[[item]], meta,
      multichoice_specs[[item]] %||% list(),
      missing_tokens  = miss_i,
      unknown_action  = unknown_action,
      warn_on_unknown = warn_on_unknown
    )
  }
  
  df
}


# Coerce a single item vector to its dictionary type.
# missing_tokens are kept as explicit trailing levels; recoding to NA is the
# sole responsibility of besd_recode_missing().
.coerce_item <- function(x,
                         meta_row,
                         spec            = list(),
                         missing_tokens  = NULL,
                         unknown_action  = c("error", "na"),
                         warn_on_unknown = TRUE) {
  
  unknown_action <- match.arg(unknown_action)
  item_type  <- meta_row$item_type[[1]]
  levels_std <- meta_row$levels[[1]]
  reverse    <- meta_row$reverse[[1]]
  miss_keys  <- .strip_non_alpnum(missing_tokens %||% character(0))
  
  if (item_type %in% c("binary", "ordinal", "categorical")) {
    
    x_chr <- trimws(as.character(x))
    x_chr[x_chr == ""] <- NA_character_
    key <- .strip_non_alpnum(x_chr)
    
    # Recode to canonical dictionary labels.
    # Missing-token values won't match any dictionary key so x_rec will be NA
    # for them — restore explicitly below before the unknown-value check.
    mp    <- .level_map(levels_std)
    x_rec <- ifelse(is.na(x_chr), NA_character_, mp[key])
    
    # Restore missing-token values so they are not treated as unknown.
    if (length(miss_keys)) {
      is_miss        <- !is.na(key) & key %in% miss_keys
      x_rec[is_miss] <- x_chr[is_miss]
    }
    
    # Unknown values: non-NA in input but still NA in x_rec after restoration.
    bad_idx <- !is.na(x_chr) & is.na(x_rec)
    bad     <- setdiff(unique(x_chr[bad_idx]), NA_character_)
    .check_unknown(bad, unknown_action, warn_on_unknown)
    if (length(bad) && unknown_action == "na") x_rec[bad_idx] <- NA_character_
    
    # Warn on dictionary levels absent from data (ignoring missing-token values).
    .warn_absent_levels(
      levels_std, x_rec, miss_keys,
      paste0("[", meta_row$item_id[[1]],
             "] Variable has dictionary level(s) not observed in data: %s")
    )
    
    levs <- if (isTRUE(reverse)) rev(levels_std) else levels_std
    
    # Append missing-token values as explicit trailing factor levels so they are
    # preserved until besd_recode_missing() removes them (if requested).
    miss_vals <- setdiff(
      unique(x_rec[!is.na(x_rec) & (.strip_non_alpnum(x_rec) %in% miss_keys)]),
      levs
    )
    if (length(miss_vals)) levs <- c(levs, miss_vals)
    
    if (item_type == "ordinal") return(factor(x_rec, levels = levs, ordered = TRUE))
    return(factor(x_rec, levels = levs, ordered = FALSE))
  }
  
  if (item_type == "multichoice") {
    sep <- spec$sep %||% .BESD_SEP
    return(.coerce_multichoice_packed(
      x, levels_std,
      sep             = sep,
      missing_tokens  = missing_tokens,
      unknown_action  = unknown_action,
      warn_on_unknown = warn_on_unknown
    ))
  }
  
  .stopf("Unsupported item_type `%s`.", item_type)
}


#
.coerce_multichoice_packed <- function(x,
                                       levels_std,
                                       sep             = .BESD_SEP,
                                       missing_tokens  = NULL,
                                       unknown_action  = c("error", "na"),
                                       warn_on_unknown = TRUE) {
  
  unknown_action <- match.arg(unknown_action)
  miss_keys <- .strip_non_alpnum(missing_tokens %||% character(0))
  
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  
  # Collect all tokens observed across rows.
  tokens_all <- unique(unlist(lapply(x_chr, function(s) {
    if (is.na(s)) return(character(0))
    strsplit(s, sep, fixed = TRUE)[[1]]
  })))
  tokens_all <- tokens_all[tokens_all != ""]
  
  # Allow-list: dictionary levels + missing tokens.
  # Missing tokens must not be flagged as unknown; besd_recode_missing() handles
  # them in a later, dedicated pass.
  allowed <- unique(c(levels_std, missing_tokens %||% character(0)))
  bad     <- setdiff(tokens_all, allowed)
  .check_unknown(
    bad, unknown_action, warn_on_unknown,
    err_fmt  = "Found multichoice token(s) not in dictionary levels: %s",
    warn_fmt = "Dropping %d unknown multichoice token(s): %s"
  )
  
  # Warn on dictionary levels not observed (exclude missing tokens from check).
  .warn_absent_levels(
    levels_std, setdiff(tokens_all, bad), miss_keys,
    "Some multichoice dictionary level(s) not observed in data: %s"
  )
  
  # Per-row: drop unknowns, deduplicate, order by dictionary (missing tokens trail).
  ord <- stats::setNames(seq_along(levels_std), levels_std)
  
  vapply(x_chr, function(s) {
    if (is.na(s)) return(NA_character_)
    v <- strsplit(s, sep, fixed = TRUE)[[1]]
    v <- v[v != ""]
    if (!length(v)) return(NA_character_)
    
    # Drop genuinely unknown tokens; missing tokens are kept as-is here.
    if (length(bad) && unknown_action == "na") v <- setdiff(v, bad)
    if (!length(v)) return(NA_character_)
    
    v <- unique(v)
    
    # Order: dictionary tokens first (by position), missing tokens last.
    o           <- ord[v]
    o[is.na(o)] <- max(ord, na.rm = TRUE) + seq_len(sum(is.na(o)))
    paste(v[order(o)], collapse = sep)
  }, character(1))
}


# Pack multiple binary indicator columns into a single delimited multichoice
# string. Handles text_prefix and binary encoding schemes.
.pack_multichoice_cols <- function(df, cols, levels_std, spec = list()) {
  
  encoding <- spec$encoding %||% "text_prefix"
  sep      <- spec$sep %||% .BESD_SEP
  get_chr  <- function(v) trimws(as.character(v))
  
  if (encoding == "text_prefix") {
    not_prefix <- spec$not_prefix %||% "Not "
    
    mat <- lapply(df[, cols, drop = FALSE], function(v) {
      v <- get_chr(v)
      v[v == ""] <- NA_character_
      v[!is.na(v) & startsWith(v, not_prefix)] <- NA_character_
      v
    })
    mat <- as.data.frame(mat, stringsAsFactors = FALSE)
    
    return(apply(mat, 1, function(row) {
      vals <- row[!is.na(row)]
      if (!length(vals)) NA_character_ else paste(unique(vals), collapse = sep)
    }))
  }
  
  if (encoding == "binary") {
    sel   <- c("yes", "selected", "true", "1")
    unsel <- c("no", "not selected", "false", "0")
    selected_values   <- tolower(spec$selected_values   %||% sel)
    unselected_values <- tolower(spec$unselected_values %||% unsel)
    
    ov <- intersect(selected_values, unselected_values)
    if (length(ov))
      .stopf(
        "Binary multichoice: selected_values overlaps unselected_values: %s", .pastec(ov)
      )
    
    col_labels <- spec$col_labels
    if (is.null(col_labels)) {
      if (length(cols) > length(levels_std))
        .stopf("Binary multichoice: need spec$col_labels when cols > length(levels_std).")
      col_labels <- levels_std[seq_along(cols)]
    }
    if (length(col_labels) != length(cols))
      .stopf("Binary multichoice: spec$col_labels must have same length as cols.")
    
    mat <- lapply(df[, cols, drop = FALSE], function(v) tolower(get_chr(v)))
    mat <- as.data.frame(mat, stringsAsFactors = FALSE)
    
    return(apply(mat, 1, function(row) {
      picked <- character(0)
      for (j in seq_along(row)) {
        lab <- col_labels[[j]]
        if (is.na(lab) || !nzchar(lab)) next
        v <- row[[j]]
        if (is.na(v) || v == "") next
        if (v %in% selected_values) {
          picked <- c(picked, lab)
        } else if (!v %in% unselected_values) {
          .stopf(
            "Binary multichoice: unrecognised `%s` (extend selected/unselected vals).", v
          )
        }
      }
      if (!length(picked)) NA_character_ else paste(unique(picked), collapse = sep)
    }))
  }
  
  .stopf("Unknown multichoice encoding: `%s`.", encoding)
}


# Resolve the missing token vector for a single item, combining the `.all`
# cross-item default with any item-specific tokens. Returns NULL if none 
.besd_missing_tokens_for_item <- function(missing_tokens, item_id) {
  if (is.null(missing_tokens))                                     return(NULL)
  if (is.character(missing_tokens))                                return(missing_tokens)
  if (!is.list(missing_tokens) || is.null(names(missing_tokens))) return(NULL)
  
  out <- NULL
  if (".all"  %in% names(missing_tokens)) out <- c(out, missing_tokens[[".all"]])
  if (item_id %in% names(missing_tokens)) out <- c(out, missing_tokens[[item_id]])
  
  out <- unique(as.character(out))
  out <- out[!is.na(out) & nzchar(out)]
  if (!length(out)) NULL else out
}


# Build a named lookup from dictionary levels to normalised alphanumeric keys
# for fuzzy response matching. Errors if two levels normalise identically.
.level_map <- function(levels_std) {
  keys <- .strip_non_alpnum(levels_std)
  if (anyDuplicated(keys)) {
    dup <- unique(keys[duplicated(keys)])
    .stopf("Dictionary levels are ambiguous under canonicalisation: %s", .pastec(dup))
  }
  stats::setNames(levels_std, keys)
}


# Emit an error or warning for unknown values; does not mutate anything.
# Callers are responsible for setting unknown values to NA after this call.
.check_unknown <- function(bad, unknown_action, warn_on_unknown,
                           err_fmt  = "Found value(s) not in dictionary levels: %s",
                           warn_fmt = "Coercing %d unknown value(s) to NA: %s") {
  if (!length(bad)) return(invisible(NULL))
  if (unknown_action == "error") .stopf(err_fmt, .pastec(sort(bad)))
  if (unknown_action == "na" && isTRUE(warn_on_unknown))
    warning(sprintf(warn_fmt, length(bad), .pastec(sort(bad))), call. = FALSE)
}

# Warn when standard dictionary levels are absent from the observed data.
# `observed` is the vector of values (or tokens) after coercion; miss_keys are
# the canonicalised missing-token keys to exclude from the presence check.
.warn_absent_levels <- function(levels_std, observed, miss_keys, warn_msg) {
  std_only    <- observed[!is.na(observed) &
                            !(.strip_non_alpnum(observed) %in% miss_keys)]
  missing_std <- setdiff(levels_std, std_only)
  if (length(missing_std))
    warning(sprintf(warn_msg, paste(missing_std, collapse = ", ")), call. = FALSE)
}


# Recode missing-token values to NA in a single vector. Drops the corresponding
# factor levels when drop_levels = TRUE.
.na_missing_tokens <- function(v, miss_keys, drop_levels = TRUE) {
  v_chr   <- as.character(v)
  is_miss <- !is.na(v_chr) & (.strip_non_alpnum(v_chr) %in% miss_keys)
  v_chr[is_miss] <- NA_character_
  if (!is.factor(v)) return(v_chr)
  keep_levs <- if (isTRUE(drop_levels)) {
    levels(v)[!(.strip_non_alpnum(levels(v)) %in% miss_keys)]
  } else {
    levels(v)
  }
  factor(v_chr, levels = keep_levs, ordered = is.ordered(v))
}