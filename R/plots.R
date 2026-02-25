#' Plotting helpers for BeSD summaries

# ---- Small utilities --------------------------------------------------------

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

#' @keywords internal
.as_palette <- function(palette, n, levels = NULL) {
  .besd_resolve_palette(palette = palette, n = n, levels = levels)
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
.require_cols <- function(df, cols, fn = "function") {
  miss <- setdiff(cols, names(df))
  if (length(miss)) {
    stop(fn, " requires column(s): ", paste(miss, collapse = ", "), 
         call. = FALSE)
  }
}

#' @keywords internal
.ensure_cols <- function(x) {
  if (!"question_short" %in% names(x)) x$question_short <- x$item_id
  if (!"question" %in% names(x)) x$question <- x$question_short
  if (!"item_type" %in% names(x)) x$item_type <- "unknown"
  x
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

# ---- Top-box selection ------------------------------------------------------

#' Heuristic for selecting "top-box" responses
#' @export
besd_guess_topbox_levels <- function(responses,
                                     prefer_regex = c(
                                       "^Yes$",
                                       "Strongly agree|Completely agree",
                                       "Agree a lot|Agree",
                                       "Very likely|Definitely",
                                       "A lot|High"
                                     )) {
  r <- unique(as.character(responses))
  if (!length(r)) return(character())
  
  if (is.factor(responses) && isTRUE(attr(responses, "ordered"))) {
    return(tail(levels(responses), 1))
  }
  
  for (pat in prefer_regex) {
    hit <- r[grepl(pat, r, ignore.case = TRUE)]
    if (length(hit)) return(hit)
  }
  
  if (is.factor(responses)) return(tail(levels(responses), 1))
  tail(r, 1)
}

#' Build a top-box tibble from a BeSD summary tibble
#' @export
besd_topbox <- function(sum_tbl,
                        topbox_levels = NULL,
                        prefer_regex = c(
                          "^Yes$",
                          "Strongly agree|Completely agree",
                          "Agree a lot|Agree",
                          "Very likely|Definitely",
                          "A lot|High"
                        ),
                        include_item_types = c("binary", "ordinal", 
                                              "categorical", "unknown")) {
  .require_cols(sum_tbl, c("item_id", "response", "pct"), "besd_topbox")
  sum_tbl <- .ensure_cols(.coerce_country(sum_tbl))
  
  # Get dictionary to check for reverse flag
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
  
  # CRITICAL: Respect reverse flag when picking top-box
  pick_levels <- function(item_id, levs) {
    # User override takes precedence
    if (!is.null(topbox_levels) && item_id %in% names(topbox_levels)) {
      return(topbox_levels[[item_id]])
    }
    
    # Check if this item has reverse = TRUE in dictionary
    if (nrow(dict) > 0 && item_id %in% dict$item_id) {
      item_dict <- dict[dict$item_id == item_id, ]
      is_reversed <- isTRUE(item_dict$reverse[1])
      
      if (is_reversed) {
        # For reversed items, pick the FIRST level (not last)
        # This is typically "No" for binary items where we want to 
        # measure lack of barriers
        if (is.factor(levs)) {
          return(head(levels(levs), 1))
        } else {
          return(head(as.character(levs), 1))
        }
      }
    }
    
    # Default behavior: pick top-box normally
    besd_guess_topbox_levels(levs, prefer_regex = prefer_regex)
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

# ---- Bar plots --------------------------------------------------------------

#' Bar plots for BeSD response distributions
#' @export
plot_besd_bars <- function(sum_tbl,
                           include_item_types = c("binary", "ordinal", 
                                                  "categorical", "unknown"),
                           include_multichoice = FALSE,
                           palette = NULL,
                           base_size = 12,
                           label_pct = TRUE,
                           label_min = 6,
                           wrap_width = 50) {
  
  .require_cols(sum_tbl, c("item_id", "response", "pct"), "plot_besd_bars")
  sum_tbl <- .ensure_cols(.coerce_country(sum_tbl))
  sum_tbl <- .besd_apply_response_order(sum_tbl)
  
  dd <- sum_tbl
  if (isTRUE(include_multichoice) && !is.null(include_item_types)) {
    include_item_types <- union(include_item_types, "multichoice")
  }
  if (!is.null(include_item_types)) {
    dd <- dd |> dplyr::filter(.data$item_type %in% include_item_types)
  }
  if (!isTRUE(include_multichoice)) {
    dd <- dd |> dplyr::filter(.data$item_type != "multichoice")
  }
  
  dd <- dd |>
    dplyr::mutate(
      item_lab = .wrap_lines(dplyr::coalesce(.data$question, .data$item_id),
                             width = wrap_width, n_lines = 3),
      country = as.factor(.data$country)
    )
  
  n_countries <- dplyr::n_distinct(dd$country)
  dd_nm <- dd |> dplyr::filter(.data$item_type != "multichoice")
  dd_mc <- dd |> dplyr::filter(.data$item_type == "multichoice")
  
  make_one_nm <- function(item) {
    di <- dd_nm |> dplyr::filter(.data$item_id == item)
    item_resp_levels <- unique(as.character(di$response))
    di <- di |> dplyr::mutate(response = factor(.data$response, 
                                                levels = item_resp_levels))
    item_pal <- .as_palette(palette, length(item_resp_levels), 
                            levels = item_resp_levels)
    
    if (n_countries <= 1) {
      p <- ggplot2::ggplot(di, ggplot2::aes(x = .data$response, 
                                            y = .data$pct, 
                                            fill = .data$response)) +
        ggplot2::geom_col(width = 0.8, colour = "white", linewidth = 0.3) +
        ggplot2::scale_fill_manual(values = item_pal, guide = "none") +
        ggplot2::labs(x = NULL, y = "Percent", 
                      title = unique(di$item_lab)) +
        besd_theme(base_size) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                           hjust = 1))
      if (isTRUE(label_pct)) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = dplyr::if_else(.data$pct >= label_min,
                                              sprintf("%.0f%%", .data$pct),
                                              "")),
          vjust = -0.2, size = base_size * 0.25
        )
      }
    } else {
      p <- ggplot2::ggplot(di, ggplot2::aes(x = .data$country, 
                                            y = .data$pct, 
                                            fill = .data$response)) +
        ggplot2::geom_col(position = "stack", width = 0.8, 
                          colour = "white", linewidth = 0.3) +
        ggplot2::scale_fill_manual(values = item_pal, name = "Response") +
        ggplot2::labs(x = NULL, y = "Percent", 
                      title = unique(di$item_lab)) +
        besd_theme(base_size) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                           hjust = 1))
      if (isTRUE(label_pct)) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = dplyr::if_else(.data$pct >= label_min,
                                              sprintf("%.0f%%", .data$pct),
                                              "")),
          position = ggplot2::position_stack(vjust = 0.5),
          size = base_size * 0.25,
          colour = "white"
        )
      }
    }
    p
  }
  
  make_one_mc_opt <- function(item, resp) {
    di <- dd_mc |> dplyr::filter(.data$item_id == item, 
                                 .data$response == resp)
    item_resp_levels <- unique(as.character(di$response))
    di <- di |> dplyr::mutate(response = factor(.data$response, 
                                                levels = item_resp_levels))
    item_pal <- .as_palette(palette, length(item_resp_levels), 
                            levels = item_resp_levels)
    
    ttl <- paste0(unique(di$item_lab), " — ", 
                  as.character(unique(di$response)))
    
    p <- ggplot2::ggplot(di, ggplot2::aes(x = .data$country, 
                                          y = .data$pct, 
                                          fill = .data$response)) +
      ggplot2::geom_col(width = 0.8, colour = "white", linewidth = 0.3) +
      ggplot2::scale_fill_manual(values = item_pal, guide = "none") +
      ggplot2::labs(x = NULL, y = "Percent selecting option", title = ttl) +
      besd_theme(base_size) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                         hjust = 1))
    if (isTRUE(label_pct)) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = dplyr::if_else(.data$pct >= label_min,
                                            sprintf("%.0f%%", .data$pct),
                                            "")),
        vjust = -0.2, size = base_size * 0.25
      )
    }
    p
  }
  
  mc_plots <- list()
  if (nrow(dd_mc) > 0) {
    mc_keys <- dd_mc |> dplyr::distinct(.data$item_id, .data$response)
    mc_names <- paste(mc_keys$item_id, as.character(mc_keys$response), 
                      sep = " | ")
    mc_plots <- stats::setNames(
      Map(make_one_mc_opt, mc_keys$item_id, mc_keys$response),
      mc_names
    )
  }
  
  nm_plots <- list()
  if (nrow(dd_nm) > 0) {
    nm_items <- unique(dd_nm$item_id)
    nm_plots <- stats::setNames(lapply(nm_items, make_one_nm), nm_items)
  }
  
  if (length(mc_plots) == 0) return(nm_plots)
  if (length(nm_plots) == 0) return(mc_plots)
  c(nm_plots, mc_plots)
}

# ---- Multichoice plots ------------------------------------------------------

#' Bar plots for multichoice summary items
#' @export
plot_besd_multichoice_bars <- function(sum_tbl,
                                       item_ids = NULL,
                                       top_n = 10,
                                       palette = NULL,
                                       base_size = 12,
                                       wrap_width = 32) {
  .require_cols(sum_tbl, c("item_id", "response", "pct", "item_type"),
                "plot_besd_multichoice_bars")
  sum_tbl <- .ensure_cols(.coerce_country(sum_tbl))
  
  dd <- sum_tbl |>
    dplyr::mutate(
      item_lab = .wrap_lines(dplyr::coalesce(.data$question_short, 
                                             .data$item_id),
                             width = wrap_width, n_lines = 3),
      country = as.factor(.data$country),
      response = as.character(.data$response)
    )
  
  if (!is.null(item_ids)) {
    dd <- dd |> dplyr::filter(.data$item_id %in% item_ids)
  }
  if (!nrow(dd)) stop("No multichoice rows found", call. = FALSE)
  
  n_countries <- dplyr::n_distinct(dd$country)
  countries <- levels(dd$country)
  pal <- .as_palette(palette, length(countries), levels = countries)
  
  dd_top <- dd |>
    dplyr::group_by(.data$country, .data$item_id) |>
    dplyr::slice_max(.data$pct, n = top_n, with_ties = FALSE) |>
    dplyr::ungroup()
  
  make_one <- function(item) {
    di <- dd_top |> dplyr::filter(.data$item_id == item)
    
    ord <- di |>
      dplyr::group_by(.data$response) |>
      dplyr::summarise(p = mean(.data$pct, na.rm = TRUE), 
                       .groups = "drop") |>
      dplyr::arrange(.data$p) |>
      dplyr::pull(.data$response)
    
    di <- di |> dplyr::mutate(response = factor(.data$response, 
                                                levels = ord))
    
    if (n_countries <= 1) {
      return(
        ggplot2::ggplot(di, ggplot2::aes(y = .data$response, 
                                         x = .data$pct)) +
          ggplot2::geom_col(width = 0.75, fill = "grey30") +
          ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", 
                                                          .data$pct)),
                             hjust = -0.05, size = base_size * 0.28) +
          ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"),
                                      limits = c(0, 100)) +
          ggplot2::labs(x = "Percent", y = NULL, 
                        title = unique(di$item_lab)) +
          besd_theme(base_size)
      )
    }
    
    ggplot2::ggplot(di, ggplot2::aes(y = .data$response, 
                                     x = .data$pct, 
                                     fill = .data$country)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75),
                        width = 0.7) +
      ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"),
                                  limits = c(0, 100)) +
      ggplot2::scale_fill_manual(values = pal, name = "Country") +
      ggplot2::labs(x = "Percent endorsing option", y = NULL,
                    title = unique(di$item_lab)) +
      besd_theme(base_size)
  }
  
  items <- unique(dd_top$item_id)
  stats::setNames(lapply(items, make_one), items)
}

# ---- Spider / radar plots ---------------------------------------------------
#' Spider (radar) plot for BeSD top-box by country
#' @export
plot_besd_spider <- function(sum_tbl,
                             compare = "all",
                             focal_country = NULL,
                             topbox_levels = NULL,
                             item_ids = NULL,
                             palette = NULL,
                             base_size = 12,
                             wrap_width = 22,
                             ncol = 3,
                             # Shrink spider + give labels room
                             spider_scale = 0.80,
                             label_padding = 12,
                             outer_padding = 12,
                             # horizontal-only spacing between facet panels (in "lines")
                             facet_padding = 1.2,
                             # if TRUE, plot multiple countries on one spider (no facets)
                             overlay = FALSE) {
  
  # Standard domain colours (kept internally; not an input arg)
  domain_colors <- c(
    "thinking and feeling" = "#F4E4B7",
    "social processes"     = "#C8E6C9",
    "practical issues"     = "#BBDEFB"
  )
  
  if (!is.numeric(spider_scale) || length(spider_scale) != 1 || spider_scale <= 0) {
    stopf("spider_scale must be a single positive number (e.g., 0.85).")
  }
  if (!is.numeric(label_padding) || length(label_padding) != 1) {
    stopf("label_padding must be a single number.")
  }
  if (!is.numeric(outer_padding) || length(outer_padding) != 1) {
    stopf("outer_padding must be a single number.")
  }
  if (!is.numeric(facet_padding) || length(facet_padding) != 1 || facet_padding < 0) {
    stopf("facet_padding must be a single non-negative number (in 'lines').")
  }
  if (!is.logical(overlay) || length(overlay) != 1) {
    stopf("overlay must be TRUE/FALSE.")
  }
  
  # Prepare data ---------------------------------------------------------------
  tb <- besd_topbox(.ensure_cols(.coerce_country(sum_tbl)), 
                    topbox_levels = topbox_levels)
  if (!nrow(tb)) stop("No data for spider plot", call. = FALSE)
  if (!is.null(item_ids)) tb <- tb |> dplyr::filter(.data$item_id %in% item_ids)
  
  tb <- tb |> dplyr::mutate(
    item_lab = paste0(.data$question_short, " (", .data$topbox_label, ")")
  )
  
  # Domain presence
  has_domain <- "domain" %in% names(tb) && any(!is.na(tb$domain))
  
  if (has_domain) {
    tb <- tb |>
      dplyr::mutate(
        domain = factor(.data$domain,
                        levels = c(
                          "thinking and feeling", 
                          "social processes", 
                          "practical issues"
                          )
                        )
      ) |>
      dplyr::arrange(.data$domain, .data$item_id)
  }
  
  # Filter countries based on compare argument --------------------------------
  countries <- unique(as.character(tb$country))
  
  if (length(compare) == 1 && compare == "all") {
    
    if (!is.null(focal_country) && !overlay) {
      dd <- tb |> dplyr::filter(.data$country == focal_country)
      use_facets <- FALSE
    } else {
      dd <- tb
      use_facets <- !overlay && length(unique(dd$country)) > 1
    }
    
  } else if (length(compare) == 1 && compare == "mean") {
    
    focal_country <- focal_country %||% countries[1]
    
    dd_mean <- tb |>
      dplyr::filter(.data$country != focal_country) |>
      dplyr::group_by(.data$item_id, .data$item_lab) |>
      dplyr::summarise(
        pct = mean(.data$pct),
        domain = if (has_domain) dplyr::first(.data$domain) else NA_character_,
        .groups = "drop"
      ) |>
      dplyr::mutate(country = "Mean (others)")
    
    dd <- dplyr::bind_rows(
      tb |> dplyr::filter(.data$country == focal_country),
      dd_mean
    )
    
    use_facets <- !overlay
    
  } else {
    # compare is a vector of countries:
    dd <- tb |> dplyr::filter(.data$country %in% compare)
    use_facets <- !overlay
  }
  
  if (!nrow(dd)) stop("No data after filtering", call. = FALSE)
  
  # Choose highlight country (used for overlay title + grey "reference" polygon)
  highlight_country <- NULL
  if (!is.null(focal_country) && focal_country %in% unique(as.character(dd$country))) {
    highlight_country <- focal_country
  } else if (overlay && length(compare) > 1 && !all(compare %in% c("all", "mean"))) {
    # for vector compare in overlay mode, honour the user's order
    highlight_country <- as.character(compare[1])
  } else {
    highlight_country <- unique(as.character(dd$country))[1]
  }
  # If compare[1] wasn't actually present after filtering, fall back safely
  if (!highlight_country %in% unique(as.character(dd$country))) {
    highlight_country <- unique(as.character(dd$country))[1]
  }
  
  # Axis order + wrapped labels (robust even if domain missing) ---------------
  axis_tbl <- dd |>
    dplyr::distinct(.data$item_id, .data$item_lab, dplyr::across(dplyr::any_of("domain")))
  
  if (!"domain" %in% names(axis_tbl)) axis_tbl$domain <- NA_character_
  axis_tbl <- axis_tbl |> dplyr::mutate(domain = as.character(.data$domain))
  
  if (has_domain) {
    axis_tbl <- axis_tbl |>
      dplyr::mutate(
        domain = factor(.data$domain,
                        levels = c(
                          "thinking and feeling", 
                          "social processes", 
                          "practical issues"
                        )
        )
      ) |>
      dplyr::arrange(.data$domain, .data$item_id)
  } else {
    axis_tbl <- axis_tbl |> dplyr::arrange(.data$item_id)
  }
  
  axis_tbl <- axis_tbl |>
    dplyr::mutate(item_lab_wrapped = .wrap_lines(.data$item_lab, wrap_width, 3))
  
  axis_levels <- axis_tbl$item_id
  n_items <- length(axis_levels)
  if (n_items < 3) stop("Need at least 3 items for a radar polygon", call. = FALSE)
  
  dd <- dd |>
    dplyr::left_join(axis_tbl |> dplyr::select(.data$item_id, .data$item_lab_wrapped), 
                     by = "item_id") |>
    dplyr::mutate(
      idx = match(.data$item_id, axis_levels),
      theta = 2 * pi * (.data$idx - 1) / n_items
    )
  
  # Spider radius scaling ------------------------------------------------------
  r_max <- 100 * spider_scale
  
  # Convert polar -> Cartesian (start at top, clockwise) -----------------------
  dd <- dd |>
    dplyr::mutate(
      r   = (.data$pct / 100) * r_max,
      ang = pi/2 - .data$theta,
      x   = .data$r * cos(.data$ang),
      y   = .data$r * sin(.data$ang)
    )
  
  # Close polygon safely by repeating first point ------------------------------
  dd_closed <- dd |>
    dplyr::group_by(.data$country) |>
    dplyr::arrange(.data$idx, .by_group = TRUE) |>
    dplyr::group_modify(~ dplyr::bind_rows(.x, dplyr::slice(.x, 1))) |>
    dplyr::ungroup()
  
  # Domain wedges (background) ------------------------------------------------
  domain_layers <- list()
  if (has_domain) {
    width <- 2 * pi / n_items
    
    dom_bounds <- dd |>
      dplyr::filter(!is.na(.data$domain)) |>
      dplyr::group_by(.data$domain) |>
      dplyr::summarise(
        start_idx = min(.data$idx),
        end_idx   = max(.data$idx),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        start_theta = 2*pi*(.data$start_idx - 1)/n_items - width/2,
        end_theta   = 2*pi*(.data$end_idx   - 1)/n_items + width/2
      )
    
    for (i in seq_len(nrow(dom_bounds))) {
      st <- dom_bounds$start_theta[i]
      en <- dom_bounds$end_theta[i]
      th <- seq(st, en, length.out = 200)
      
      wedge <- dplyr::bind_rows(
        data.frame(theta = st, r = 0),
        data.frame(theta = th, r = r_max),
        data.frame(theta = en, r = 0)
      ) |>
        dplyr::mutate(
          ang = pi/2 - .data$theta,
          x = .data$r * cos(.data$ang),
          y = .data$r * sin(.data$ang)
        )
      
      dom_name <- as.character(dom_bounds$domain[i])
      col <- domain_colors[[dom_name]]
      if (is.null(col) || is.na(col)) col <- "grey90"
      
      domain_layers[[length(domain_layers) + 1]] <- ggplot2::geom_polygon(
        data = wedge,
        ggplot2::aes(x = .data$x, y = .data$y),
        inherit.aes = FALSE,
        fill = col,
        alpha = 0.25,
        color = NA
      )
    }
  }
  
  # Grid (circles + spokes) ---------------------------------------------------
  grid_pct <- c(25, 50, 75, 100)
  grid_r <- (grid_pct / 100) * r_max
  
  grid_circles <- tidyr::expand_grid(
    r = grid_r,
    theta = seq(0, 2*pi, length.out = 361)
  ) |>
    dplyr::mutate(
      ang = pi/2 - .data$theta,
      x = .data$r * cos(.data$ang),
      y = .data$r * sin(.data$ang)
    )
  
  spokes <- data.frame(
    idx = seq_len(n_items),
    theta = 2*pi*(seq_len(n_items) - 1)/n_items
  ) |>
    dplyr::mutate(
      ang = pi/2 - .data$theta,
      x = 0, y = 0,
      xend = r_max * cos(.data$ang),
      yend = r_max * sin(.data$ang)
    )
  
  # Labels around circle ------------------------------------------------------
  label_r <- r_max + label_padding
  
  labels_df <- axis_tbl |>
    dplyr::mutate(
      idx   = match(.data$item_id, axis_levels),
      theta = 2*pi*(.data$idx - 1)/n_items,
      ang   = pi/2 - .data$theta,
      
      x = label_r * cos(.data$ang),
      y = label_r * sin(.data$ang),
      
      angle_raw = .data$ang * 180/pi,
      flip      = .data$angle_raw < -90 | .data$angle_raw > 90,
      angle     = ifelse(.data$flip, .data$angle_raw + 180, .data$angle_raw),
      
      is_vertical = abs(abs(.data$angle_raw) - 90) < 6,
      hjust = ifelse(.data$is_vertical, 0.5, ifelse(.data$flip, 1, 0))
    )
  
  # Radial tick labels --------------------------------------------------------
  radial_labels <- data.frame(r = grid_r, pct = grid_pct) |>
    dplyr::mutate(
      x = 3,
      y = .data$r,
      label = paste0(.data$pct, "%")
    )
  
  # Plot modes ----------------------------------------------------------------
  c_levels <- unique(as.character(dd$country))
  is_comparison <- !use_facets && length(c_levels) > 1
  is_single <- length(c_levels) == 1
  
  if (is_comparison) {
    dd_title_closed <- dd_closed |> dplyr::filter(.data$country == highlight_country)
    dd_other_closed <- dd_closed |> dplyr::filter(.data$country != highlight_country)
    dd_title_pts <- dd |> dplyr::filter(.data$country == highlight_country)
    dd_other_pts <- dd |> dplyr::filter(.data$country != highlight_country)
    pal <- .as_palette(palette, length(unique(dd_other_closed$country)))
  }
  
  p <- ggplot2::ggplot()
  
  # Domain background
  if (length(domain_layers)) {
    for (layer in domain_layers) p <- p + layer
  }
  
  # Grid
  p <- p +
    ggplot2::geom_path(
      data = grid_circles,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$r),
      inherit.aes = FALSE,
      color = "grey80", linewidth = 0.3
    ) +
    ggplot2::geom_segment(
      data = spokes,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      inherit.aes = FALSE,
      color = "grey85", linewidth = 0.3
    ) +
    ggplot2::geom_text(
      data = radial_labels,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      inherit.aes = FALSE,
      size = base_size * 0.25,
      color = "grey45"
    )
  
  # Data layers
  if (is_comparison) {
    p <- p +
      ggplot2::geom_polygon(
        data = dd_title_closed,
        ggplot2::aes(x = .data$x, y = .data$y, group = .data$country),
        fill = "#2C3E50", alpha = 0.10, color = "#2C3E50", linewidth = 1
      ) +
      ggplot2::geom_point(
        data = dd_title_pts,
        ggplot2::aes(x = .data$x, y = .data$y),
        size = 2.5, color = "#2C3E50"
      )
    
    if (nrow(dd_other_closed) > 0) {
      p <- p +
        ggplot2::geom_polygon(
          data = dd_other_closed,
          ggplot2::aes(x = .data$x, y = .data$y,
                       color = .data$country, fill = .data$country,
                       group = .data$country),
          alpha = 0.10, linewidth = 1
        ) +
        ggplot2::geom_point(
          data = dd_other_pts,
          ggplot2::aes(x = .data$x, y = .data$y, color = .data$country),
          size = 2.5
        ) +
        ggplot2::scale_color_manual(values = pal, name = "Comparison") +
        ggplot2::scale_fill_manual(values = pal, guide = "none")
    }
    
  } else {
    p <- p +
      ggplot2::geom_polygon(
        data = dd_closed,
        ggplot2::aes(x = .data$x, y = .data$y, group = .data$country),
        fill = "#2C3E50", alpha = 0.15, color = "#2C3E50", linewidth = 1
      ) +
      ggplot2::geom_point(
        data = dd,
        ggplot2::aes(x = .data$x, y = .data$y),
        size = 2.5, color = "#2C3E50"
      )
    
    if (use_facets) p <- p + ggplot2::facet_wrap(~ country, ncol = ncol)
  }
  
  # Item labels
  p <- p +
    ggplot2::geom_text(
      data = labels_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$item_lab_wrapped, 
                   hjust = .data$hjust),
      inherit.aes = FALSE,
      size = base_size * 0.25,
      color = "grey20"
    )
  
  # Finalize ------------------------------------------------------------------
  lim <- label_r + outer_padding
  
  # Title rules:
  # - single country -> that country
  # - overlay comparison -> highlight country (automatic; no extra arg)
  # - facets/multi -> no title
  plot_title <- if (is_single) {
    as.character(c_levels[1])
  } else if (overlay && is_comparison) {
    highlight_country
  } else {
    NULL
  }
  
  # Horizontal-only facet spacing:
  spacing_theme <- ggplot2::theme()
  if (use_facets) {
    default_y <- ggplot2::theme_get()$panel.spacing
    if (is.null(default_y)) default_y <- grid::unit(0.5, "lines")
    
    spacing_theme <- tryCatch(
      ggplot2::theme(
        panel.spacing.x = grid::unit(facet_padding, "lines"),
        panel.spacing.y = default_y
      ),
      error = function(e) {
        ggplot2::theme(panel.spacing = grid::unit(facet_padding, "lines"))
      }
    )
  }
  
  p +
    ggplot2::coord_equal(
      xlim = c(-lim, lim),
      ylim = c(-lim, lim),
      expand = FALSE,
      clip = "off"
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = plot_title) +
    ggplot2::theme_minimal(base_size = base_size) +
    spacing_theme +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      
      strip.background = ggplot2::element_rect(fill = NA, color = NA),
      strip.text = ggplot2::element_text(face = "bold", size = base_size),
      legend.position =
      if (is_comparison && exists("dd_other_closed") && nrow(dd_other_closed) > 0) {
        "bottom" 
      } else {
        "none"
      },
      legend.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.margin = ggplot2::margin(
        t = base_size,
        r = base_size * 2.5,
        b = base_size,
        l = base_size * 2.5
      )
    )
}


# ---- Demographics table plot ------------------------------------------------

#' Plot a demographic summary as a "pretty table"
#' @export
plot_besd_demographics_table <- function(dem_tbl,
                                         country,
                                         max_levels = 12,
                                         sort_by_pct = TRUE,
                                         fill_by = c("pct", "none"),
                                         fill_high = "#4F8D9A",
                                         base_size = 12,
                                         wrap_width = 30) {
  fill_by <- match.arg(fill_by)
  .require_cols(dem_tbl, c("country", "item_id", "response", "n", "pct"),
                "plot_besd_demographics_table")
  
  dd <- dem_tbl |> dplyr::filter(as.character(.data$country) == country)
  if (!nrow(dd)) stop("No rows found for country: ", country, 
                      call. = FALSE)
  
  dd <- dd |>
    dplyr::mutate(
      item_lab = .wrap_lines(.data$item_id, width = wrap_width, 
                             n_lines = 2),
      response = as.character(.data$response),
      cell = paste0(.data$n, " (", sprintf("%.0f%%", .data$pct), ")")
    )
  
  dd <- dd |>
    dplyr::group_by(.data$item_id) |>
    dplyr::arrange(if (isTRUE(sort_by_pct)) dplyr::desc(.data$pct) 
                   else .data$response, .by_group = TRUE) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$rank <= max_levels) |>
    dplyr::select(-.data$rank)
  
  dd <- dd |>
    dplyr::mutate(
      item_lab = factor(.data$item_lab, levels = unique(.data$item_lab)),
      response = factor(.data$response, levels = rev(unique(.data$response)))
    )
  
  p <- ggplot2::ggplot(dd, ggplot2::aes(x = .data$item_lab, 
                                        y = .data$response))
  
  if (fill_by == "pct") {
    p <- p + 
      ggplot2::geom_tile(ggplot2::aes(fill = .data$pct), 
                         colour = "white", linewidth = 0.3) +
      ggplot2::scale_fill_gradient(low = "white", high = fill_high, 
                                   name = "%")
  } else {
    p <- p + ggplot2::geom_tile(fill = "grey95", colour = "white", 
                                linewidth = 0.3)
  }
  
  p +
    ggplot2::geom_text(ggplot2::aes(label = .data$cell),
                       size = base_size * 0.25) +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = paste0("Demographic profile: ", country),
      subtitle = "Cells show n (percent)"
    ) +
    besd_theme(base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )
}


