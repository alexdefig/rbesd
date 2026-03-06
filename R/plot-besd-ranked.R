#' Ranked lollipop chart — all countries for one BeSD item or domain
#'
#' Produces a horizontal lollipop chart with all countries ranked by
#' top-box percentage for a single item or domain average.  An optional
#' reference line marks the global mean, and a focal country can be
#' highlighted in a contrasting colour.
#'
#' @param besd_sum A \code{besd_summary_tbl}.
#' @param item_id  A single item ID string (must match \code{besd_sum$item_id}).
#'   Mutually exclusive with \code{domain}.
#' @param domain   A domain string (e.g. \code{"thinking and feeling"}).
#'   When supplied, countries are ranked by their mean top-box % across all
#'   items in that domain.  Mutually exclusive with \code{item_id}.
#' @param highlight_country Optional country name to highlight in red.
#' @param show_ci  Logical; draw 95 \% CI error bars when available.
#'   Default \code{TRUE}.
#' @param base_size Base font size passed to \code{besd_theme()}.
#'
#' @return A \code{ggplot2} object.
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

  tb <- besd_topbox(.coerce_country(besd_sum))

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

  # Sort countries by pct
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
    # Stem
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = .data$pct,
                   y = .data$country, yend = .data$country,
                   colour = .data$dot_colour),
      linewidth = 0.7
    ) +
    # Dot
    ggplot2::geom_point(
      ggplot2::aes(colour = .data$dot_colour, size = .data$dot_size)
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_size_identity() +
    # Global mean reference
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

  # CI bars
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
