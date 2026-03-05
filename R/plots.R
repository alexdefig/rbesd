


# ---- Multichoice plots ------------------------------------------------------

#' Bar plots for multichoice summary items
#' @export
plot_besd_multichoice_bars <- function(sum_tbl,
                                       item_ids = NULL,
                                       top_n = 10,
                                       palette = NULL,
                                       base_size = 12,
                                       wrap_width = 32) {
  .assert_besd_summary_tbl(sum_tbl, "plot_besd_multichoice_bars")
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