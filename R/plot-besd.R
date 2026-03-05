#' Bar plots for BeSD response distributions
#' @export
plot_besd_bars <- function(sum_tbl,
                           include_item_types = c("binary", "ordinal", 
                                                  "categorical", "unknown"),
                           include_multichoice = FALSE,
                           sort_bars = FALSE,
                           palette = NULL,
                           base_size = 12,
                           label_pct = TRUE,
                           label_min = 6,
                           wrap_width = 50,
                           combine_domains = FALSE) {
  
  .assert_besd_summary_tbl(sum_tbl, fn = "plot_besd_bars")
  sum_tbl <- .coerce_country(sum_tbl)
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
  
  
  sort_countries <- function(di, response_level = NULL) {
    if (!is.null(response_level)) {
      di <- di |> dplyr::filter(as.character(.data$response) == response_level)
    }
    sort_vals <- di |>
      dplyr::select(.data$country, .data$pct) |>
      tibble::deframe()
    names(sort(sort_vals))
  }
  
  make_one_nm <- function(item) {
    di <- dd_nm |> dplyr::filter(.data$item_id == item)
    item_resp_levels <- unique(as.character(di$response))
    di <- di |> dplyr::mutate(response = factor(.data$response, 
                                                levels = item_resp_levels))
    item_pal <- .as_palette(palette, length(item_resp_levels), 
                            levels = item_resp_levels)
    
    
    if (n_countries <= 1) {
      p <- ggplot2::ggplot(
        di, 
        ggplot2::aes(x = if (sort_bars) {
          reorder(.data$response, .data$pct) 
        } else .data$response, y = .data$pct, fill = .data$response)) +
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
      
      if (sort_bars) {
        country_order <- sort_countries(di, item_resp_levels[length(item_resp_levels)])
        di <- di |> dplyr::mutate(country = factor(as.character(.data$country), 
                                                   levels = country_order))
      }
      
      p <- ggplot2::ggplot(di, ggplot2::aes(x = .data$country, 
                                            y = .data$pct, fill = .data$response)) +
        ggplot2::geom_col(position = "stack", width = 0.8, 
                          colour = "white", linewidth = 0.3) +
        ggplot2::scale_y_continuous(
          limits = c(0, 102),
          expand = c(0, 0)
        ) +
        ggplot2::scale_fill_manual(values = item_pal, name = "Response") +
        ggplot2::labs(x = NULL, y = "Percent", 
                      title = unique(di$item_lab)) +
        besd_theme(base_size) +
        ggplot2::theme(
          axis.text.x  = ggplot2::element_text(
            angle  = 45, hjust = 1,
            margin = ggplot2::margin(t = 2)
          )
        )
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
  
  make_mc_combined <- function() {
    # One figure for all multichoice items, faceted by item.
    # y = response option, x = pct selecting it.
    # Single country: fill by response (full palette per item via free colour
    #   scale is not possible in one ggplot, so use a single accent colour and
    #   rely on bar length + label to differentiate options).
    # Multiple countries: fill by country, dodged bars.
    
    dd_mc2 <- dd_mc |>
      dplyr::mutate(
        response  = as.character(.data$response),
        item_lab2 = .wrap_lines(
          dplyr::coalesce(.data$question, .data$item_id),
          width   = wrap_width,
          n_lines = 3
        )
      )
    
    # Within each item, order responses by mean pct descending (so the longest
    # bar sits at the top of each facet).
    resp_order <- dd_mc2 |>
      dplyr::group_by(.data$item_id, .data$response) |>
      dplyr::summarise(mean_pct = mean(.data$pct, na.rm = TRUE),
                       .groups  = "drop") |>
      dplyr::arrange(.data$item_id, .data$mean_pct) |>
      dplyr::pull(.data$response) |>
      unique()
    
    dd_mc2 <- dd_mc2 |>
      dplyr::mutate(response = factor(.data$response, levels = resp_order))
    
    n_mc_items <- dplyr::n_distinct(dd_mc2$item_id)
    
    if (n_countries <= 1) {
      # Single accent colour per facet is not straightforward in one ggplot;
      # use a neutral fill and rely on labels.
      accent <- .as_palette(palette, 1)[1]
      
      p <- ggplot2::ggplot(
        dd_mc2,
        ggplot2::aes(y = .data$response, x = .data$pct)
      ) +
        ggplot2::geom_col(fill = accent, width = 0.72,
                          colour = "white", linewidth = 0.3) +
        ggplot2::geom_text(
          ggplot2::aes(label = dplyr::if_else(
            .data$pct >= label_min, sprintf("%.0f%%", .data$pct), ""
          )),
          hjust  = -0.15,
          size   = base_size * 0.28,
          colour = "grey20"
        ) +
        ggplot2::scale_x_continuous(
          limits = c(0, 105),
          expand = c(0, 0),
          labels = function(x) paste0(x, "%"),
          name   = "Percent selecting option"
        ) +
        ggplot2::facet_wrap(
          ~ item_lab2,
          ncol   = 1,
          scales = "free_y"
        ) +
        ggplot2::labs(y = NULL) +
        besd_theme(base_size) +
        ggplot2::theme(
          strip.text         = ggplot2::element_text(
            face   = "bold",
            hjust  = 0,
            size   = base_size * 0.9
          ),
          strip.background   = ggplot2::element_rect(fill = "grey95",
                                                     colour = NA),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_line(colour = "grey90",
                                                     linewidth = 0.3),
          panel.grid.minor   = ggplot2::element_blank(),
          panel.spacing.y    = grid::unit(0.8, "lines"),
          legend.position    = "none"
        )
      
    } else {
      countries <- levels(dd_mc2$country)
      mc_pal    <- .as_palette(palette, length(countries), levels = countries)
      
      p <- ggplot2::ggplot(
        dd_mc2,
        ggplot2::aes(y = .data$response, x = .data$pct, fill = .data$country)
      ) +
        ggplot2::geom_col(
          position = ggplot2::position_dodge(width = 0.8),
          width    = 0.72,
          colour   = "white",
          linewidth = 0.3
        ) +
        ggplot2::scale_x_continuous(
          limits = c(0, 105),
          expand = c(0, 0),
          labels = function(x) paste0(x, "%"),
          name   = "Percent selecting option"
        ) +
        ggplot2::scale_fill_manual(values = mc_pal, name = "Country") +
        ggplot2::facet_wrap(
          ~ item_lab2,
          ncol   = 1,
          scales = "free_y"
        ) +
        ggplot2::labs(y = NULL) +
        besd_theme(base_size) +
        ggplot2::theme(
          strip.text         = ggplot2::element_text(
            face   = "bold",
            hjust  = 0,
            size   = base_size * 0.9
          ),
          strip.background   = ggplot2::element_rect(fill = "grey95",
                                                     colour = NA),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_line(colour = "grey90",
                                                     linewidth = 0.3),
          panel.grid.minor   = ggplot2::element_blank(),
          panel.spacing.y    = grid::unit(0.8, "lines"),
          legend.position    = "bottom",
          legend.title       = ggplot2::element_text(face = "bold")
        )
      
      if (isTRUE(label_pct)) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = dplyr::if_else(
            .data$pct >= label_min, sprintf("%.0f%%", .data$pct), ""
          )),
          position = ggplot2::position_dodge(width = 0.8),
          hjust    = -0.15,
          size     = base_size * 0.26,
          colour   = "grey20"
        )
      }
    }
    p
  }
  
  mc_plots <- list()
  if (nrow(dd_mc) > 0) {
    mc_plots <- list(multichoice = make_mc_combined())
  }
  
  nm_plots <- list()
  if (nrow(dd_nm) > 0) {
    
    if (n_countries <= 1) {
      # ---- Single-country: one combined patchwork, domains as inline headers -
      #
      # All items live in a single patchwork (ncol = 1).  When domain info is
      # present, a thin title-only panel is inserted before each domain group.
      # The key spacing fixes:
      #   • bar width = 0.88 so the bar fills most of the panel height
      #   • scale_y_discrete expand = (add = 0.12) removes default 0.6 padding
      #   • legend.margin top = -6 pt pulls the legend up against the bar
      #   • plot.margin bottom = 0 for all but the last item, top = 0 for all
      #     but the first — eliminates the gap between one item's legend and
      #     the next item's title
      
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("Package 'patchwork' is required for single-country bar plots. ",
             "Install it with: install.packages('patchwork')", call. = FALSE)
      }
      
      has_domain <- "domain" %in% names(dd_nm) && any(!is.na(dd_nm$domain))
      
      # ---- Ordered item table -----------------------------------------------
      if (has_domain) {
        item_order_tbl <- dd_nm |>
          dplyr::distinct(.data$item_id, .data$domain, .data$item_lab) |>
          dplyr::arrange(.data$domain, .data$item_id)
      } else {
        item_order_tbl <- dd_nm |>
          dplyr::distinct(.data$item_id, .data$item_lab) |>
          dplyr::mutate(domain = NA_character_) |>
          dplyr::arrange(.data$item_id)
      }
      
      if (sort_bars) {
        topbox_pcts <- dd_nm |>
          dplyr::group_by(.data$item_id) |>
          dplyr::slice_max(.data$pct, n = 1, with_ties = FALSE) |>
          dplyr::ungroup() |>
          dplyr::select(.data$item_id, .topbox_pct = .data$pct)
        item_order_tbl <- item_order_tbl |>
          dplyr::left_join(topbox_pcts, by = "item_id") |>
          dplyr::arrange(.data$domain, dplyr::desc(.data$.topbox_pct)) |>
          dplyr::select(-.data$.topbox_pct)
      }
      
      n_items_total <- nrow(item_order_tbl)
      
      # ---- Per-item horizontal stacked bar ----------------------------------
      # Each item gets its own full-range palette so all levels are distinct.
      # is_first / is_last control top/bottom margin and x-axis visibility.
      make_one_single <- function(item, is_first = FALSE, is_last = FALSE) {
        di <- dd_nm |> dplyr::filter(.data$item_id == item)
        
        if ("response_key" %in% names(di) && is.factor(di$response_key)) {
          rk_order        <- levels(droplevels(di$response_key))
          resp_order_item <- unique(sub(".*___", "", rk_order))
        } else {
          resp_order_item <- unique(as.character(di$response))
        }
        resp_order_item <- resp_order_item[
          resp_order_item %in% as.character(di$response)
        ]
        di <- di |> dplyr::mutate(
          response = factor(as.character(.data$response),
                            levels = resp_order_item)
        )
        
        item_pal <- .as_palette(palette, length(resp_order_item),
                                levels = resp_order_item)
        
        q_label <- unique(dplyr::coalesce(di$question, di$item_id))[1]
        q_label_wrapped <- .wrap_lines(q_label, width = 30, n_lines = 3)
        
        # Use the wrapped label as the single y-axis category so it renders
        # to the left of the bar rather than above it as a plot title.
        di <- di |> dplyr::mutate(.y_lab = q_label_wrapped)
        
        p <- ggplot2::ggplot(
          di,
          ggplot2::aes(x = .data$pct, y = .data$.y_lab, fill = .data$response)
        ) +
          ggplot2::geom_col(
            position  = ggplot2::position_stack(reverse = FALSE),
            width     = 0.55,
            colour    = "white",
            linewidth = 0.4
          ) +
          ggplot2::scale_x_continuous(
            limits = c(0, 101),
            expand = c(0, 0),
            breaks = c(0, 25, 50, 75, 100),
            labels = function(x) paste0(x, "%"),
            name   = if (is_last) "" else NULL
          ) +
          ggplot2::scale_y_discrete(
            expand = ggplot2::expansion(add = 0.4)
          ) +
          ggplot2::scale_fill_manual(values = item_pal, name = NULL) +
          ggplot2::guides(fill = ggplot2::guide_legend(
            byrow = TRUE,
            override.aes = list(size = 3)
          )) +
          ggplot2::labs(y = NULL, title = NULL) +
          besd_theme(base_size) +
          ggplot2::theme(
            plot.title         = ggplot2::element_blank(),
            axis.text.y        = ggplot2::element_text(
              size   = base_size * 0.72,
              face   = "bold",
              hjust  = 1,
              colour = "grey20",
              margin = ggplot2::margin(r = 6)
            ),
            axis.ticks.y       = ggplot2::element_blank(),
            axis.title.y       = ggplot2::element_blank(),
            axis.text.x        = if (is_last) {
              ggplot2::element_text(size = base_size * 0.78)
            } else {
              ggplot2::element_blank()
            },
            axis.ticks.x       = if (is_last) {
              ggplot2::element_line(colour = "grey70")
            } else {
              ggplot2::element_blank()
            },
            axis.title.x       = if (is_last) {
              ggplot2::element_text(face = "bold", size = base_size * 0.85)
            } else {
              ggplot2::element_blank()
            },
            legend.position    = "right",
            legend.justification = "left",
            legend.key.size    = grid::unit(0.32, "cm"),
            legend.text        = ggplot2::element_text(size = base_size * 0.75, 
                                                       hjust = 0),
            # Negative top margin pulls legend up against the bar bottom
            legend.margin      = ggplot2::margin(t = -6, b = 0),
            legend.box.margin  = ggplot2::margin(0, 0, 0, 0),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(colour = "grey90",
                                                       linewidth = 0.3),
            panel.grid.minor   = ggplot2::element_blank(),
            # Uniform top/bottom margins — no title above means no need for
            # extra top gap on the first item.
            plot.margin        = ggplot2::margin(
              t = 2, r = 12, b = 0, l = 8, unit = "pt"
            )
          )
        
        if (isTRUE(label_pct)) {
          p <- p + ggplot2::geom_text(
            ggplot2::aes(
              label = dplyr::if_else(.data$pct >= label_min,
                                     sprintf("%.0f%%", .data$pct), "")
            ),
            position = ggplot2::position_stack(vjust = 0.5, reverse = FALSE),
            size     = base_size * 0.25,
            colour   = "white",
            fontface = "bold"
          )
        }
        p
      }
      
      # ---- Domain section header (theme_void spacer) -------------------------
      make_domain_header <- function(domain_name) {
        lbl <- if (is.na(domain_name) || domain_name == "NA") "Other" else
          tools::toTitleCase(domain_name)
        ggplot2::ggplot() +
          ggplot2::annotate(
            "text", x = 0, y = 0.75,
            label    = toupper(lbl),
            hjust    = 0, fontface = "bold",
            size     = base_size * 0.40, colour = "grey25"
          ) +
          ggplot2::annotate(
            "segment",
            x = 0, xend = 1, y = 0.22, yend = 0.22,
            colour = "black", linewidth = 0.45
          ) +
          ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
          ggplot2::theme_void() +
          ggplot2::theme(
            plot.margin = ggplot2::margin(t = 10, b = 0, l = 0, unit = "pt")
          )
      }
      
      # ---- Interleave headers and item panels --------------------------------
      all_plots   <- list()
      all_heights <- numeric(0)
      prev_domain <- NULL
      header_h    <- 0.50   # taller to accommodate larger text without overlap
      item_h      <- 0.95   # bar + legend; reduced to keep overall plot compact
      
      for (i in seq_len(n_items_total)) {
        cur_dom <- as.character(item_order_tbl$domain[i])
        
        if (has_domain && (is.null(prev_domain) || cur_dom != prev_domain)) {
          all_plots   <- c(all_plots, list(make_domain_header(cur_dom)))
          all_heights <- c(all_heights, header_h)
          prev_domain <- cur_dom
        }
        
        is_first_item <- (i == 1)
        is_last_item  <- (i == n_items_total)
        
        all_plots   <- c(all_plots,
                         list(make_one_single(item_order_tbl$item_id[i],
                                              is_first = is_first_item,
                                              is_last  = is_last_item)))
        all_heights <- c(all_heights, item_h)
      }
      
      country_name <- as.character(unique(dd_nm$country))[1]   # ← move up here
      
      p_combined <- patchwork::wrap_plots(all_plots, ncol = 1,
                                          heights = all_heights) +
        patchwork::plot_annotation(
          title = country_name,
          theme = ggplot2::theme(
            plot.title = ggplot2::element_text(
              face   = "bold",
              size   = base_size * 2,
              hjust  = 0.5,
              margin = ggplot2::margin(b = 8)
            )
          )
        )
      nm_plots <- stats::setNames(list(p_combined), country_name)
      
    } else {
      # ---- Multiple countries: one plot per item (existing behaviour) ----
      nm_items <- unique(dd_nm$item_id)
      nm_plots <- stats::setNames(lapply(nm_items, make_one_nm), nm_items)
    }
  }
  
  if (length(mc_plots) == 0) return(nm_plots)
  if (length(nm_plots) == 0) return(mc_plots)
  c(nm_plots, mc_plots)
}


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
  
  .assert_besd_summary_tbl(sum_tbl, fn = "plot_besd_spider")
  
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
  tb <- besd_topbox(.coerce_country(sum_tbl), topbox_levels = topbox_levels)
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


#' Heuristic for selecting "top-box" responses
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
  .assert_besd_summary_tbl(sum_tbl, fn = "besd_topbox")
  sum_tbl <- .coerce_country(sum_tbl)
  
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