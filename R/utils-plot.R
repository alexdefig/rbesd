
#' IMMS palette
#' @export
besd_palette_imms <- function(n,
                              reverse = FALSE,
                              alpha = 1,
                              anchors = c("#CC278D", "#926F97", "#4F8D9A")) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    stop("n must be a single positive number", call. = FALSE)
  }
  cols <- grDevices::colorRampPalette(anchors)(as.integer(n))
  if (isTRUE(reverse)) cols <- rev(cols)
  grDevices::adjustcolor(cols, alpha.f = alpha)
}

#' @keywords internal
.as_palette <- function(palette, n, levels = NULL) {
  .besd_resolve_palette(palette = palette, n = n, levels = levels)
}

#' Built-in palette resolver
.besd_resolve_palette <- function(palette = c("imms", "okabe_ito"),
                                  n,
                                  levels = NULL) {
  if (is.null(palette)) palette <- "imms"
  
  if (is.character(palette) && length(palette) == 1) {
    pal_name <- tolower(palette)
    cols <- switch(
      pal_name,
      "imms" = besd_palette_imms(n),
      "okabe_ito" = besd_okabe_ito(n),
      "okabe-ito" = besd_okabe_ito(n),
      "okabe" = besd_okabe_ito(n),
      NULL
    )
    if (!is.null(cols)) {
      if (!is.null(levels)) names(cols) <- levels
      return(cols)
    }
  }
  
  if (is.function(palette)) {
    cols <- palette(n)
    if (!is.null(levels)) names(cols) <- levels
    return(cols)
  }
  
  if (is.character(palette)) {
    cols <- palette
    if (!is.null(levels) && is.null(names(cols))) {
      if (length(cols) < length(levels)) {
        cols <- grDevices::colorRampPalette(cols)(length(levels))
      }
      names(cols) <- levels
    }
    return(cols)
  }
  
  stop("palette must be NULL, a palette name, a function(n), or a ",
       "character vector of colours", call. = FALSE)
}


#' Apply dictionary-based response ordering
#' @keywords internal
.besd_apply_response_order <- function(sum_tbl) {
  besd_dict <- attr(sum_tbl, "besd_dict")
  dem_dict  <- attr(sum_tbl, "dem_dict")
  
  dict <- dplyr::bind_rows(
    if (is.null(besd_dict)) tibble::tibble() else tibble::as_tibble(besd_dict),
    if (is.null(dem_dict))  tibble::tibble() else tibble::as_tibble(dem_dict)
  )
  
  if (!nrow(dict)) return(sum_tbl)
  
  dict <- dict |>
    dplyr::filter(.data$item_id %in% unique(sum_tbl$item_id))
  
  lvl_keys <- unlist(
    Map(function(id, levs, rvv) {
      if (isTRUE(rvv)) levs <- rev(levs)
      paste(id, as.character(levs), sep = "___")
    },
    dict$item_id, dict$levels, dict$reverse),
    use.names = FALSE
  )
  
  sum_tbl <- sum_tbl |>
    dplyr::mutate(response_key = paste(.data$item_id, .data$response, 
                                       sep = "___"))
  
  extra <- setdiff(unique(sum_tbl$response_key), lvl_keys)
  lvl_keys <- c(lvl_keys, extra)
  
  sum_tbl |>
    dplyr::mutate(
      response_key = factor(.data$response_key, levels = lvl_keys),
      item_id = factor(.data$item_id, levels = dict$item_id)
    )
}

#' Default BeSD theme
#' @export
besd_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.title.y = ggplot2::element_text(face = "bold")
    )
}

#' @keywords internal
.wrap_lines <- function(x, width = 28, n_lines = 2) {
  x <- x %||% ""
  w <- stringr::str_wrap(x, width = width)
  parts <- strsplit(w, "\n", fixed = TRUE)
  vapply(parts, function(p) {
    if (length(p) <= n_lines) return(paste(p, collapse = "\n"))
    p <- p[seq_len(n_lines)]
    p[n_lines] <- stringr::str_trunc(p[n_lines], width = max(1, width - 1),
                                     side = "right", ellipsis = "…")
    paste(p, collapse = "\n")
  }, character(1))
}

#' @keywords internal
.coerce_country <- function(x) {
  if (!"country" %in% names(x)) x$country <- "national"
  x
}

