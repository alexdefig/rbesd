# ── Palette & theme utilities ─────────────────────────────────────────────────

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

#' @keywords internal
.besd_resolve_palette <- function(palette = "imms", n, levels = NULL) {
  if (is.null(palette)) palette <- "imms"

  if (is.character(palette) && length(palette) == 1) {
    pal_name <- tolower(palette)
    cols <- switch(pal_name, "imms" = besd_palette_imms(n), NULL)
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
                                     side = "right", ellipsis = "\u2026")
    paste(p, collapse = "\n")
  }, character(1))
}

#' @keywords internal
.coerce_country <- function(x) {
  if (!"country" %in% names(x)) x$country <- "national"
  x
}

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


# ── Bar plots ─────────────────────────────────────────────────────────────────

#' Bar plots for BeSD response distributions
#'
#' Produces stacked (multi-country) or single-country horizontal bar charts
#' summarising response distributions for all non-multichoice BeSD items.
#' For a single country, items are combined into a patchwork figure grouped by
#' domain; for multiple countries, one plot is returned per item.  Multichoice
#' items can optionally be included via \code{include_multichoice}.
#'
#' @param sum_tbl A \code{besd_summary_tbl} (output of \code{summary()}).
#' @param include_item_types Character vector of item types to include.
#'   Default \code{c("binary", "ordinal", "categorical", "unknown")}.
#' @param include_multichoice Logical; if \code{TRUE}, multichoice items are
#'   also plotted (delegated to an internal helper).  Default \code{FALSE}.
#' @param sort_bars Logical; if \code{TRUE}, countries (multi-country mode) or
#'   response options (single-country mode) are ordered by descending
#'   percentage.  Default \code{FALSE}.
#' @param palette Optional colour palette override passed to
#'   \code{.as_palette()}.  If \code{NULL}, the package default is used.
#' @param base_size Numeric; base font size (points) passed to
#'   \code{besd_theme()}. Default \code{12}.
#' @param label_pct Logical; if \code{TRUE} (default), percentage labels are
#'   printed on bars.
#' @param label_min Numeric; minimum percentage below which bar labels are
#'   suppressed to avoid clutter.  Default \code{6}.
#' @param wrap_width Integer; maximum character width for wrapping item
#'   question labels.  Default \code{50}.
#' @param combine_domains Logical; reserved for future use.  Default
#'   \code{FALSE}.
#'
#' @return A named list of \code{ggplot2} objects.  In single-country mode the
#'   list contains one combined \code{patchwork} figure keyed by country name;
#'   in multi-country mode it contains one plot per item ID.
#'
#' @examples
#' data("data_demo", package = "rbesd")
#' x <- as_besd(data_demo, country_col = "country")
#' s <- summary(x)
#' # Single country
#' plots <- plot_besd_bars(s[s$country == "Brazil", ])
#' # All countries, sorted
#' plots <- plot_besd_bars(s, sort_bars = TRUE)
#'
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

  make_mc_plots <- function() {
    dd_mc2 <- dd_mc |>
      dplyr::mutate(
        response  = as.character(.data$response),
        item_lab2 = .wrap_lines(
          dplyr::coalesce(.data$question, .data$item_id),
          width   = wrap_width,
          n_lines = 3
        )
      )

    resp_order <- dd_mc2 |>
      dplyr::group_by(.data$item_id, .data$response) |>
      dplyr::summarise(mean_pct = mean(.data$pct, na.rm = TRUE),
                       .groups  = "drop") |>
      dplyr::arrange(.data$item_id, .data$mean_pct) |>
      dplyr::pull(.data$response) |>
      unique()

    dd_mc2 <- dd_mc2 |>
      dplyr::mutate(response = factor(.data$response, levels = resp_order))

    mc_items <- unique(dd_mc2$item_id)

    if (n_countries <= 1) {
      accent <- .as_palette(palette, 1)[1]

      wrapped_levels <- .wrap_lines(levels(dd_mc2$response), width = 25)
      dd_mc2 <- dd_mc2 |>
        dplyr::mutate(response = factor(.wrap_lines(as.character(.data$response),
                                                    width = 25),
                                        levels = wrapped_levels))

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
          ncol   = 2,
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
          strip.background   = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_line(colour = "grey90",
                                                     linewidth = 0.3),
          panel.grid.minor   = ggplot2::element_blank(),
          panel.spacing      = grid::unit(0.8, "lines"),
          legend.position    = "none"
        )

      return(list(multichoice = p))
    }

    accent <- .as_palette(palette, 1)[1]

    make_one_mc <- function(item) {
      di <- dd_mc2 |> dplyr::filter(.data$item_id == item)

      if (sort_bars) {
        country_order <- di |>
          dplyr::group_by(.data$country) |>
          dplyr::summarise(mean_pct = mean(.data$pct, na.rm = TRUE),
                           .groups = "drop") |>
          dplyr::arrange(.data$mean_pct) |>
          dplyr::pull(.data$country) |>
          as.character()
        di <- di |>
          dplyr::mutate(country = factor(as.character(.data$country),
                                         levels = country_order))
      }

      n_resp <- dplyr::n_distinct(di$response)
      facet_ncol <- ceiling(sqrt(n_resp))

      p <- ggplot2::ggplot(
        di,
        ggplot2::aes(x = .data$country, y = .data$pct)
      ) +
        ggplot2::geom_col(fill = accent, width = 0.72,
                          colour = "white", linewidth = 0.3) +
        ggplot2::scale_y_continuous(
          limits = c(0, 105),
          expand = c(0, 0),
          labels = function(x) paste0(x, "%")
        ) +
        ggplot2::facet_wrap(
          ~ response,
          ncol   = facet_ncol,
          scales = "fixed",
          axes   = "all_x"
        ) +
        ggplot2::labs(x = NULL, y = "Percent selecting option",
                      title = unique(di$item_lab2)) +
        besd_theme(base_size) +
        ggplot2::theme(
          strip.text         = ggplot2::element_text(
            face  = "bold",
            hjust = 0.5,
            size  = base_size * 0.85
          ),
          strip.background   = ggplot2::element_blank(),
          axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1,
                                                     margin = ggplot2::margin(t = 2)),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(colour = "grey90",
                                                     linewidth = 0.3),
          panel.grid.minor   = ggplot2::element_blank(),
          panel.spacing      = grid::unit(0.8, "lines"),
          legend.position    = "none"
        )

      if (isTRUE(label_pct)) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = dplyr::if_else(
            .data$pct >= label_min, sprintf("%.0f%%", .data$pct), ""
          )),
          vjust  = -0.2,
          size   = base_size * 0.26,
          colour = "grey20"
        )
      }
      p
    }

    stats::setNames(lapply(mc_items, make_one_mc), mc_items)
  }

  mc_plots <- list()
  if (nrow(dd_mc) > 0) {
    mc_plots <- make_mc_plots()
  }

  nm_plots <- list()
  if (nrow(dd_nm) > 0) {

    if (n_countries <= 1) {
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("Package 'patchwork' is required for single-country bar plots. ",
             "Install it with: install.packages('patchwork')", call. = FALSE)
      }

      has_domain <- "domain" %in% names(dd_nm) && any(!is.na(dd_nm$domain))

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
            legend.margin      = ggplot2::margin(t = -6, b = 0),
            legend.box.margin  = ggplot2::margin(0, 0, 0, 0),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(colour = "grey90",
                                                       linewidth = 0.3),
            panel.grid.minor   = ggplot2::element_blank(),
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

      all_plots   <- list()
      all_heights <- numeric(0)
      prev_domain <- NULL
      header_h    <- 0.50
      item_h      <- 0.95

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

      country_name <- as.character(unique(dd_nm$country))[1]

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
      nm_items <- unique(dd_nm$item_id)
      nm_plots <- stats::setNames(lapply(nm_items, make_one_nm), nm_items)
    }
  }

  if (length(mc_plots) == 0) return(nm_plots)
  if (length(nm_plots) == 0) return(mc_plots)
  c(nm_plots, mc_plots)
}


# ── Spider / radar chart ──────────────────────────────────────────────────────

#' Spider (radar) chart of BeSD top-box scores by country
#'
#' Draws a radar/spider chart of top-box percentages across BeSD items.
#' Supports single-country, multi-country faceted, and overlay (all countries
#' on one spider) display modes.  Domain background wedges are drawn when
#' domain metadata is present.
#'
#' @param sum_tbl A top-box \code{besd_summary_tbl} produced by
#'   \code{summary(x, combine_top = TRUE)}.
#' @param compare Character vector of country names to include, or one of the
#'   special values \code{"all"} (default; plots all countries) or
#'   \code{"mean"} (plots \code{focal_country} against the mean of all others).
#' @param focal_country Optional character string; a country to highlight.  In
#'   facet mode only this country is shown; in overlay mode it is rendered in
#'   dark with other countries coloured.
#' @param item_ids Optional character vector; restricts the chart to the
#'   specified item IDs.
#' @param palette Optional colour palette for comparison countries.  If
#'   \code{NULL}, the package default is used.
#' @param base_size Numeric; base font size (points).  Default \code{12}.
#' @param wrap_width Integer; maximum character width for axis label wrapping.
#'   Default \code{22}.
#' @param ncol Integer; number of columns when faceting multiple countries.
#'   Default \code{3}.
#' @param spider_scale Numeric (0, 1]; scales the radar polygon radius
#'   relative to the plot area, leaving room for axis labels.  Default
#'   \code{0.80}.
#' @param label_padding Numeric; distance between the outer ring and item axis
#'   labels.  Default \code{12}.
#' @param outer_padding Numeric; distance from axis labels to the plot
#'   boundary.  Default \code{12}.
#' @param facet_padding Numeric; horizontal spacing between facet panels in
#'   \code{"lines"} units.  Default \code{1.2}.
#' @param overlay Logical; if \code{TRUE}, all selected countries are drawn on
#'   a single spider instead of being faceted.  Default \code{FALSE}.
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' data("data_demo", package = "rbesd")
#' x <- as_besd(data_demo, country_col = "country")
#' s_tb <- summary(x, combine_top = TRUE)
#' # Single country
#' plot_besd_spider(s_tb, focal_country = "Brazil")
#' # Overlay comparison
#' plot_besd_spider(s_tb, compare = c("Brazil", "Canada"), overlay = TRUE)
#'
#' @export
plot_besd_spider <- function(sum_tbl,
                             compare = "all",
                             focal_country = NULL,
                             item_ids = NULL,
                             palette = NULL,
                             base_size = 12,
                             wrap_width = 22,
                             ncol = 3,
                             spider_scale = 0.80,
                             label_padding = 12,
                             outer_padding = 12,
                             facet_padding = 1.2,
                             overlay = FALSE) {

  .assert_besd_summary_tbl(sum_tbl, fn = "plot_besd_spider")

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

  tb <- .coerce_country(sum_tbl)
  if (!nrow(tb)) stop("No data for spider plot", call. = FALSE)
  if (!is.null(item_ids)) tb <- tb |> dplyr::filter(.data$item_id %in% item_ids)

  tb <- tb |> dplyr::mutate(
    item_lab = paste0(.data$question_short, " (", .data$response, ")")
  )

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
    dd <- tb |> dplyr::filter(.data$country %in% compare)
    use_facets <- !overlay
  }

  if (!nrow(dd)) stop("No data after filtering", call. = FALSE)

  highlight_country <- NULL
  if (!is.null(focal_country) && focal_country %in% unique(as.character(dd$country))) {
    highlight_country <- focal_country
  } else if (overlay && length(compare) > 1 && !all(compare %in% c("all", "mean"))) {
    highlight_country <- as.character(compare[1])
  } else {
    highlight_country <- unique(as.character(dd$country))[1]
  }
  if (!highlight_country %in% unique(as.character(dd$country))) {
    highlight_country <- unique(as.character(dd$country))[1]
  }

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

  r_max <- 100 * spider_scale

  dd <- dd |>
    dplyr::mutate(
      r   = (.data$pct / 100) * r_max,
      ang = pi/2 - .data$theta,
      x   = .data$r * cos(.data$ang),
      y   = .data$r * sin(.data$ang)
    )

  dd_closed <- dd |>
    dplyr::group_by(.data$country) |>
    dplyr::arrange(.data$idx, .by_group = TRUE) |>
    dplyr::group_modify(~ dplyr::bind_rows(.x, dplyr::slice(.x, 1))) |>
    dplyr::ungroup()

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

  radial_labels <- data.frame(r = grid_r, pct = grid_pct) |>
    dplyr::mutate(
      x = 3,
      y = .data$r,
      label = paste0(.data$pct, "%")
    )

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

  if (length(domain_layers)) {
    for (layer in domain_layers) p <- p + layer
  }

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
        ggplot2::scale_color_manual(values = pal, name = "") +
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

  p <- p +
    ggplot2::geom_text(
      data = labels_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$item_lab_wrapped,
                   hjust = .data$hjust),
      inherit.aes = FALSE,
      size = base_size * 0.25,
      color = "grey20"
    )

  lim <- label_r + outer_padding

  plot_title <- if (is_single) {
    as.character(c_levels[1])
  } else if (overlay && is_comparison) {
    highlight_country
  } else {
    NULL
  }

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


# ── Ranked lollipop chart ─────────────────────────────────────────────────────

#' Ranked lollipop chart — all countries for one BeSD item or domain
#'
#' Produces a horizontal lollipop chart with all countries ranked by
#' top-box percentage for a single item or domain average.  An optional
#' reference line marks the global mean, and a focal country can be
#' highlighted in a contrasting colour.
#'
#' @param besd_sum A top-box \code{besd_summary_tbl} produced by
#'   \code{summary(x, combine_top = TRUE)}.
#' @param item_id  A single item ID string (must match \code{besd_sum$item_id}).
#'   Mutually exclusive with \code{domain}.
#' @param domain   A domain string (e.g. \code{"thinking and feeling"}).
#'   When supplied, countries are ranked by their mean top-box \% across all
#'   items in that domain.  Mutually exclusive with \code{item_id}.
#' @param highlight_country Optional character string; name of a country to
#'   highlight with a red dot and stem.
#' @param show_ci  Logical; if \code{TRUE} (default), horizontal 95\% CI error
#'   bars are drawn when \code{lcl}/\code{ucl} columns are present.
#' @param base_size Numeric; base font size (points) passed to
#'   \code{besd_theme()}. Default \code{12}.
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' data("data_demo", package = "rbesd")
#' x <- as_besd(data_demo, country_col = "country")
#' s_tb <- summary(x, combine_top = TRUE)
#' plot_besd_ranked(s_tb, item_id = "tf_benefits")
#' plot_besd_ranked(s_tb, domain = "thinking and feeling",
#'                  highlight_country = "Brazil")
#'
#' @export
plot_besd_ranked <- function(besd_sum,
                              item_id           = NULL,
                              domain            = NULL,
                              highlight_country = NULL,
                              show_ci           = TRUE,
                              base_size         = 12) {

  if (is.null(item_id) && is.null(domain)) {
    stop("Provide either item_id or domain.", call. = FALSE)
  }
  if (!is.null(item_id) && !is.null(domain)) {
    stop("Provide only one of item_id or domain, not both.", call. = FALSE)
  }

  tb <- .coerce_country(besd_sum)

  if (!is.null(item_id)) {
    dd <- tb |> dplyr::filter(.data$item_id == !!item_id)
    if (!nrow(dd)) stop("item_id '", item_id, "' not found in top-box output.",
                         call. = FALSE)
    plot_title <- unique(dplyr::coalesce(dd$question_short, dd$item_id))[1]

  } else {
    dd <- tb |>
      dplyr::filter(!is.na(.data$domain) &
                      tolower(.data$domain) == tolower(domain)) |>
      dplyr::group_by(.data$country) |>
      dplyr::summarise(
        pct  = mean(.data$pct, na.rm = TRUE),
        lcl  = mean(.data$lcl, na.rm = TRUE),
        ucl  = mean(.data$ucl, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(item_id = domain, question_short = domain)
    plot_title <- tools::toTitleCase(domain)
  }

  dd <- dd |>
    dplyr::arrange(.data$pct) |>
    dplyr::mutate(
      country      = factor(as.character(.data$country),
                            levels = as.character(.data$country)),
      is_highlight = !is.null(highlight_country) &
                      as.character(.data$country) == highlight_country,
      dot_colour   = dplyr::if_else(.data$is_highlight, "#E74C3C", "#2C3E50"),
      dot_size     = dplyr::if_else(.data$is_highlight, 4.5, 3)
    )

  global_mean <- mean(dd$pct, na.rm = TRUE)

  p <- ggplot2::ggplot(dd,
    ggplot2::aes(x = .data$pct, y = .data$country)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = .data$pct,
                   y = .data$country, yend = .data$country,
                   colour = .data$dot_colour),
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data$dot_colour, size = .data$dot_size)
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::geom_vline(xintercept = global_mean,
                        linetype = "dashed",
                        colour = "grey55",
                        linewidth = 0.5) +
    ggplot2::annotate("text",
                      x = global_mean + 1, y = 0.7,
                      label = sprintf("Mean: %.0f%%", global_mean),
                      hjust = 0, size = base_size * 0.28,
                      colour = "grey45") +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      labels = function(x) paste0(x, "%"),
      name   = "Top-box %",
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(y = NULL, title = plot_title) +
    besd_theme(base_size) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey90",
                                                  linewidth = 0.3),
      panel.grid.minor   = ggplot2::element_blank(),
      legend.position    = "none"
    )

  if (isTRUE(show_ci) && all(c("lcl", "ucl") %in% names(dd))) {
    p <- p + ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$lcl, xmax = .data$ucl),
      height    = 0.35,
      colour    = "grey65",
      linewidth = 0.45
    )
  }

  p
}


# ── Demographics table ────────────────────────────────────────────────────────

#' Heatmap table of demographic composition for one country
#'
#' Renders a tile-based heatmap showing response counts and percentages for
#' each demographic variable for a single country.  Cells are optionally
#' shaded by percentage to aid visual scanning.
#'
#' @param dem_tbl A demographic summary tibble, as returned by
#'   \code{besd_summary_demographics()}.  Must contain columns
#'   \code{country}, \code{item_id}, \code{response}, \code{n}, and
#'   \code{pct}.
#' @param country Character string; the country to display.
#' @param max_levels Integer; maximum number of response levels to show per
#'   demographic variable.  Additional levels are dropped.  Default \code{12}.
#' @param sort_by_pct Logical; if \code{TRUE} (default), levels within each
#'   variable are ordered by descending percentage.  If \code{FALSE}, ordered
#'   alphabetically by response label.
#' @param fill_by One of \code{"pct"} (default) or \code{"none"}.  When
#'   \code{"pct"}, tiles are filled with a white-to-teal gradient proportional
#'   to the cell percentage.  When \code{"none"}, tiles are uniform grey.
#' @param fill_high Character; hex colour for the high end of the fill
#'   gradient.  Default \code{"#4F8D9A"} (teal).
#' @param base_size Numeric; base font size (points) passed to
#'   \code{besd_theme()}. Default \code{12}.
#' @param wrap_width Integer; maximum character width for wrapping item ID
#'   labels on the x-axis.  Default \code{30}.
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' data("data_demo", package = "rbesd")
#' x   <- as_besd(data_demo, country_col = "country", dem_dict = dem_dictionary())
#' s   <- summary(x)
#' dem <- besd_summary_demographics(s)
#' plot_besd_demographics_table(dem, country = "Brazil")
#'
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
  .assert_has_cols(dem_tbl, c("country", "item_id", "response", "n", "pct"),
                   context = "plot_besd_demographics_table")

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
