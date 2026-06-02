# ── App helpers ───────────────────────────────────────────────────────────────
# Sourced by app.R before the UI/server are defined.

# ── 1. Load data ──────────────────────────────────────────────────────────────
# User data files (.rds) come from the directory set by launch_explorer().
# iso_lookup.csv is a static reference file bundled with the app.
.data_dir <- shiny::getShinyOption("besd_data_dir", default = "data")

besd_sum      <- readRDS(file.path(.data_dir, "besd_sum.rds"))
demo_sum      <- readRDS(file.path(.data_dir, "demo_sum.rds"))
iso_lookup    <- read.csv("data/iso_lookup.csv", stringsAsFactors = FALSE)
breakdown_sum <- {
  p <- file.path(.data_dir, "breakdown_sum.rds")
  if (file.exists(p)) readRDS(p) else tibble::tibble()
}

# ── 2. Pre-compute top-box ────────────────────────────────────────────────────
topbox_all <- readRDS(file.path(.data_dir, "topbox_all.rds"))
countries  <- sort(unique(as.character(besd_sum$country)))

# Authoritative ordinal ordering and top-box level sets come from the embedded
# dictionary (stored as a besd_sum attribute), which helpers read for level
# order.
besd_dict <- attr(besd_sum, "besd_dict")

# Coerce a dictionary cell to a character vector of levels. Handles both
# representations: a list-column whose element is already a character vector,
# and a comma-joined scalar string ("a, b, c").
.dict_vec <- function(x) {
  if (length(x) == 0L) return(character(0L))
  v <- if (is.list(x)) x[[1L]] else x
  v <- as.character(v)
  if (length(v) == 1L && grepl(",", v)) v <- trimws(strsplit(v, ",")[[1L]])
  v[!is.na(v) & nzchar(v)]
}

# Ordered response levels for an item, from the dictionary (negative→positive).
dict_item_levels <- function(item_id) {
  if (is.null(besd_dict)) return(character(0L))
  .dict_vec(besd_dict$levels[besd_dict$item_id == item_id])
}

# The dictionary's top-box level set for an item (already direction-aware).
dict_top_levels <- function(item_id) {
  if (is.null(besd_dict)) return(character(0L))
  .dict_vec(besd_dict$toplevs[besd_dict$item_id == item_id])
}

# ── 3. Dropdown choices (item-level only, grouped by domain) ──────────────────
item_meta <- topbox_all |>
  dplyr::distinct(.data$item_id, .data$question_short, .data$domain) |>
  dplyr::arrange(.data$domain, .data$item_id)

.make_optgroup <- function(dom) {
  rows <- item_meta[!is.na(item_meta$domain) & item_meta$domain == dom, ]
  stats::setNames(rows$item_id, rows$question_short)
}

domains_present <- c("thinking and feeling", "social processes", "practical issues") |>
  (\(ord) ord[ord %in% unique(item_meta$domain[!is.na(item_meta$domain)])])()

map_choices <- stats::setNames(
  lapply(domains_present, .make_optgroup),
  tools::toTitleCase(domains_present)
)
ranked_choices <- map_choices

# ── 4. World map polygons (filter bad iso codes) ──────────────────────────────
world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::mutate(
    iso3 = dplyr::if_else(.data$iso_a3 == "-99", .data$iso_a3_eh, .data$iso_a3)
  ) |>
  dplyr::filter(.data$iso3 != "-99") |>
  dplyr::select("iso3", "name", "geometry")

# ── 5. Map helpers ────────────────────────────────────────────────────────────

# Return the most-positive ("top-box") response labels for an ordinal item.
# Reads the level set straight from the dictionary's toplevs, which is already
# direction-aware (handles reverse-coded scales). Restricted to levels actually
# present in the data. Falls back to the top 2 ordinal levels if the dictionary
# is unavailable.
get_top2_levels <- function(metric) {
  present <- unique(as.character(besd_sum$response[besd_sum$item_id == metric]))
  top     <- dict_top_levels(metric)
  top     <- top[top %in% present]
  if (length(top) >= 1L) return(top)

  all_levs <- item_responses(metric)
  if (length(all_levs) <= 1L) return(all_levs)
  tail(all_levs, 2L)
}

# Per-country combined % for the top 2 ordinal response levels, taken from the
# topbox_all summary (the combined top-box pct per country x item).
map_scores_top2 <- function(metric) {
  topbox_all |>
    dplyr::filter(.data$item_id == !!metric) |>
    dplyr::select("country", mean_pct = "pct")
}

# Build the palette, joined sf, and legend params for a given metric + level.
# Returns a list used by both the initial renderLeaflet and leafletProxy updates.
make_map_layers <- function(metric, level, top2 = FALSE) {
  scores      <- if (isTRUE(top2)) map_scores_top2(metric) else map_scores(metric, level)
  legend_label <- if (isTRUE(top2)) "Top 2 responses" else level
  vals       <- scores$mean_pct[!is.na(scores$mean_pct)]
  data_range <- if (length(vals) >= 2) range(vals) else c(0, 100)
  span       <- max(data_range[2] - data_range[1], 5)
  pad        <- span * 0.08
  pal_domain <- c(max(0, data_range[1] - pad), min(100, data_range[2] + pad))

  map_sf <- world_sf |>
    dplyr::left_join(
      scores |> dplyr::left_join(iso_lookup, by = "country"),
      by = c("iso3" = "iso3")
    )

  pal <- leaflet::colorNumeric(
    palette  = c("#CC278D", "#926F97", "#4F8D9A"),
    domain   = pal_domain,
    na.color = "#e8eaed"
  )
  pal_legend <- leaflet::colorNumeric(
    palette  = rev(c("#CC278D", "#926F97", "#4F8D9A")),
    domain   = pal_domain,
    na.color = "#e8eaed"
  )

  list(map_sf = map_sf, pal = pal, pal_legend = pal_legend,
       pal_domain = pal_domain, level = legend_label)
}

# Apply polygon + legend layers to a leaflet map or proxy.
apply_map_layers <- function(lf, layers) {
  lf |>
    leaflet::addPolygons(
      data         = layers$map_sf,
      fillColor    = ~layers$pal(mean_pct),
      fillOpacity  = 1,
      color        = "white",
      weight       = 0.7,
      layerId      = ~iso3,
      label = ~lapply(
        dplyr::if_else(
          !is.na(mean_pct),
          paste0("<b>", dplyr::coalesce(country, name), "</b>: ",
                 round(mean_pct, 1), "%"),
          name
        ),
        shiny::HTML
      ),
      labelOptions = leaflet::labelOptions(
        style     = list("font-family" = "Poppins, sans-serif",
                         "font-size"   = "13px",
                         "padding"     = "5px 9px",
                         "box-shadow"  = "0 2px 6px rgba(0,0,0,.15)"),
        direction = "auto"
      ),
      highlight = leaflet::highlightOptions(
        weight       = 2,
        color        = "#1d1d22",
        fillOpacity  = 0.95,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      pal      = layers$pal_legend,
      values   = layers$pal_domain,
      title    = paste0(layers$level, " (%)"),
      position = "bottomleft",
      layerId  = "legend",
      opacity  = 1,
      labFormat = leaflet::labelFormat(
        transform = function(x) sort(x, decreasing = TRUE)
      )
    )
}

# Map score resolver ───────────────────────────────────────────────────────────

# Return the ordered response levels for a given item_id.
# Respects factor ordering if response_key is present, otherwise orders by
# mean % descending (most-endorsed response first).
item_responses <- function(item_id) {
  dd      <- besd_sum |> dplyr::filter(.data$item_id == !!item_id)
  present <- unique(as.character(dd$response))

  ordered_resp <- dict_item_levels(item_id)
  if (length(ordered_resp) > 0L) {
    keep <- ordered_resp[ordered_resp %in% present]
    if (length(keep) > 0L) return(keep)
  }

  # Fallback (dictionary unavailable): order by mean % descending.
  dd |>
    dplyr::group_by(.data$response) |>
    dplyr::summarise(m = mean(.data$pct, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$m)) |>
    dplyr::pull(.data$response) |>
    as.character()
}

# Compute per-country % for a specific item × response combination.
map_scores <- function(metric, level) {
  besd_sum |>
    dplyr::filter(.data$item_id == !!metric,
                  as.character(.data$response) == as.character(level)) |>
    dplyr::select("country", mean_pct = "pct")
}

# Initial level choices (used to pre-populate the dropdown on startup).
.initial_item_id    <- item_meta$item_id[1]
initial_level_choices <- item_responses(.initial_item_id)
# Default to the top-box response so startup matches previous behaviour.
.topbox_default <- topbox_all |>
  dplyr::filter(.data$item_id == .initial_item_id) |>
  dplyr::pull(.data$response) |>
  unique()
# response holds the comma-joined top-box levels in ascending (negative→
# positive) order; take the last token, i.e. the most-positive level.
.topbox_default <- trimws(strsplit(.topbox_default[1], ",")[[1]])
.topbox_default <- .topbox_default[length(.topbox_default)]
initial_level_selected <- if (length(.topbox_default) > 0 &&
                               !is.na(.topbox_default) &&
                               .topbox_default %in% initial_level_choices)
  .topbox_default else initial_level_choices[1]

# ── 6. Interactive plotly bar chart for response distributions ────────────────
#' Build a horizontal stacked bar chart (plotly) for a single country.
#' One bar per item; responses coloured by within-item ordinal position
#' (magenta = negative end, teal = positive end).
make_profile_bar <- function(besd_sum_cty, domain_filter = "all") {

  dd <- besd_sum_cty |>
    dplyr::filter(.data$item_type %in% c("binary", "ordinal",
                                          "categorical", "unknown"))

  if (domain_filter != "all") {
    dd <- dd |> dplyr::filter(!is.na(.data$domain) &
                                 .data$domain == domain_filter)
  }

  if (nrow(dd) == 0) {
    return(
      plotly::plot_ly() |>
        plotly::layout(
          title = list(text = "No data for selected domain",
                       font = list(family = "Poppins, sans-serif", size = 14)),
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
        )
    )
  }

  # Item ordering: by item_id within the (already domain-filtered) data
  item_order <- dd |>
    dplyr::distinct(.data$item_id) |>
    dplyr::arrange(.data$item_id) |>
    dplyr::pull(.data$item_id)

  # Helper: get per-item response ordering from the dictionary (negative→positive).
  get_resp_levels <- function(di) {
    present <- unique(as.character(di$response))
    iid     <- unique(as.character(di$item_id))[1L]
    levs    <- dict_item_levels(iid)
    keep    <- levs[levs %in% present]
    if (length(keep) > 0L) keep else present
  }

  # Build one plotly trace per (item × response) in correct stack order
  fig <- plotly::plot_ly()

  for (item in item_order) {
    di        <- dplyr::filter(dd, .data$item_id == item)
    resp_levs <- get_resp_levels(di)
    n_resp    <- length(resp_levs)
    item_cols <- grDevices::colorRampPalette(
      c("#CC278D", "#926F97", "#4F8D9A"))(n_resp)

    q_raw    <- unique(dplyr::coalesce(di$question, di$item_id))[1]
    q_label  <- paste(strwrap(q_raw, width = 50), collapse = "\n")

    for (j in seq_along(resp_levs)) {
      resp  <- resp_levs[j]
      di_r  <- dplyr::filter(di, as.character(.data$response) == resp)
      if (nrow(di_r) == 0) next

      pct_val   <- di_r$pct
      txt_label <- if (!is.na(pct_val) && pct_val >= 9) {
        sprintf("%.0f%%", pct_val)
      } else ""

      fig <- fig |> plotly::add_trace(
        x             = pct_val,
        y             = q_label,
        type          = "bar",
        orientation   = "h",
        name          = resp,
        legendgroup   = item,
        showlegend    = FALSE,
        text          = txt_label,
        textposition  = "inside",
        insidetextanchor = "middle",
        textfont      = list(color = "white", size = 9,
                             family = "Poppins, sans-serif"),
        marker        = list(
          color = item_cols[j],
          line  = list(color = "white", width = 0.6)
        ),
        hovertemplate = paste0(
          "<b>", resp, "</b>: %{x:.1f}%<extra></extra>"
        )
      )
    }
  }

  fig |>
    plotly::layout(
      barmode  = "stack",
      xaxis    = list(
        title      = "",
        range      = c(0, 101),
        gridcolor  = "#e9ecef",
        zeroline   = FALSE
      ),
      yaxis    = list(
        title      = "",
        automargin = TRUE,
        tickfont   = list(size = 10, family = "Poppins, sans-serif")
      ),
      font         = list(family = "Poppins, sans-serif", size = 11),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin       = list(l = 20, r = 20, t = 10, b = 50),
      showlegend   = FALSE
    ) |>
    plotly::config(displayModeBar = FALSE)
}

# ── 7. Bar chart height (pixels) ──────────────────────────────────────────────
bar_plot_height <- function(country_name, domain_filter = "all") {
  dd <- besd_sum |>
    dplyr::filter(
      .data$country == country_name,
      .data$item_type %in% c("binary", "ordinal", "categorical", "unknown")
    )
  if (domain_filter != "all") {
    dd <- dd |> dplyr::filter(!is.na(.data$domain) &
                                 .data$domain == domain_filter)
  }
  n_items <- dplyr::n_distinct(dd$item_id)
  as.integer(max(160L, n_items * 32L + 60L))
}

# ── 8. Breakdown tab metadata ──────────────────────────────────────────────────

# Extract dem_dict from besd_sum attributes for labelling; fall back to
# breakdown_sum column values if attributes were stripped.
.dem_dict_ref <- attr(besd_sum, "dem_dict")

breakdown_var_meta <- if (nrow(breakdown_sum) > 0) {
  vars <- unique(breakdown_sum$subgroup_var)
  labels <- unique(breakdown_sum[, c("subgroup_var", "subgroup_label")])
  stats::setNames(labels$subgroup_label, labels$subgroup_var)
} else {
  character(0)
}

# Grouped item choices reused for the breakdown item selector (same as map_choices)
breakdown_item_choices <- map_choices

# ── 9. Country-comparison stacked bar (all countries, one item) ───────────────
#' Horizontal stacked bar chart comparing all countries for one BeSD item.
#' The focal country is rendered at full opacity; all others are dimmed.
make_country_comparison_bar <- function(besd_sum, item_id, focal_country) {
  .iid <- as.character(item_id)
  .cty <- as.character(focal_country)

  dd <- besd_sum |>
    dplyr::filter(
      as.character(.data$item_id) == .iid,
      .data$item_type %in% c("binary", "ordinal", "categorical", "unknown")
    )

  if (nrow(dd) == 0) {
    return(
      plotly::plot_ly() |>
        plotly::layout(
          title = list(text = "No data for this item.",
                       font = list(family = "Poppins, sans-serif", size = 12)),
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
          plot_bgcolor = "white", paper_bgcolor = "white"
        )
    )
  }

  # Response ordering: use factor levels if present, else order from data
  if ("response_key" %in% names(dd) && is.factor(dd$response_key)) {
    levs       <- levels(droplevels(dd$response_key))
    resp_order <- unique(sub(".*___", "", levs))
    resp_order <- resp_order[resp_order %in% as.character(dd$response)]
  } else {
    resp_order <- as.character(unique(dd$response))
  }

  n_resp    <- length(resp_order)
  base_cols <- grDevices::colorRampPalette(c("#CC278D", "#926F97", "#4F8D9A"))(n_resp)

  # Sort countries by top-response % ascending (highest ends up at top)
  top_resp  <- resp_order[n_resp]
  cty_order <- dd |>
    dplyr::filter(as.character(.data$response) == top_resp) |>
    dplyr::arrange(.data$pct) |>
    dplyr::pull(.data$country) |>
    as.character()
  if (!length(cty_order)) cty_order <- sort(unique(as.character(dd$country)))

  # Convert hex colour to rgba string with given alpha
  to_rgba <- function(hex, a) {
    rgb <- grDevices::col2rgb(hex)
    sprintf("rgba(%d,%d,%d,%.2f)", rgb[1L], rgb[2L], rgb[3L], a)
  }

  fig <- plotly::plot_ly()

  for (j in seq_along(resp_order)) {
    resp    <- resp_order[j]
    dr      <- dplyr::filter(dd, as.character(.data$response) == resp)
    if (nrow(dr) == 0L) next

    is_focal  <- as.character(dr$country) == .cty
    dr_others <- dr[!is_focal, ]
    dr_focal  <- dr[is_focal, ]

    # Non-focal countries: alpha fill, no legend entry
    if (nrow(dr_others) > 0) {
      pv <- dr_others$pct
      fig <- fig |> plotly::add_trace(
        x                = pv,
        y                = as.character(dr_others$country),
        type             = "bar",
        orientation      = "h",
        name             = resp,
        legendgroup      = resp,
        marker           = list(color      = to_rgba(base_cols[j], 0.70),
                                line       = list(color = "white", width = 0.6)),
        text             = ifelse(pv >= 8, sprintf("%.0f%%", pv), ""),
        textposition     = "inside",
        insidetextanchor = "middle",
        textfont         = list(color = "white", size = 9,
                                family = "Poppins, sans-serif"),
        hovertemplate    = paste0("<b>", resp, "</b>: %{x:.1f}%<extra>%{y}</extra>"),
        showlegend       = FALSE
      )
    }

    # Focal country: solid fill, owns the legend entry for this response
    if (nrow(dr_focal) > 0) {
      pv <- dr_focal$pct
      fig <- fig |> plotly::add_trace(
        x                = pv,
        y                = as.character(dr_focal$country),
        type             = "bar",
        orientation      = "h",
        name             = resp,
        legendgroup      = resp,
        marker           = list(color = base_cols[j],
                                line  = list(color = "white", width = 0.6)),
        text             = ifelse(pv >= 8, sprintf("%.0f%%", pv), ""),
        textposition     = "inside",
        insidetextanchor = "middle",
        textfont         = list(color = "white", size = 9,
                                family = "Poppins, sans-serif"),
        hovertemplate    = paste0("<b>", resp, "</b>: %{x:.1f}%<extra>%{y}</extra>"),
        showlegend       = TRUE
      )
    } else {
      # Focal country has no data for this response — still need a legend swatch
      fig <- fig |> plotly::add_trace(
        x           = NA_real_,
        y           = .cty,
        type        = "bar",
        orientation = "h",
        name        = resp,
        legendgroup = resp,
        marker      = list(color = base_cols[j]),
        showlegend  = TRUE,
        hoverinfo   = "none"
      )
    }
  }

  fig |> plotly::layout(
    barmode = "stack",
    bargap = 0.12,
    xaxis = list(
      title      = "",
      range      = c(0, 102),
      tickvals   = c(0, 25, 50, 75, 100),
      gridcolor  = "#e9ecef",
      zeroline   = FALSE
    ),
    yaxis = list(
      title         = "",
      categoryorder = "array",
      categoryarray = cty_order,
      automargin    = TRUE,
      tickfont      = list(size = 10, family = "Poppins, sans-serif")
    ),
    font          = list(family = "Poppins, sans-serif", size = 11),
    plot_bgcolor  = "white",
    paper_bgcolor = "white",
    showlegend    = FALSE,
    margin        = list(l = 0, r = 5, t = 5, b = 10, pad = 0)
  ) |>
    plotly::config(displayModeBar = FALSE)
}

# ── 10. Breakdown stacked-bar chart ───────────────────────────────────────────
#' Build a horizontal stacked bar chart for one demographic variable breakdown.
#' Each bar is a demographic group level; bars are stacked by response.
#' Sample sizes (n) are annotated at the right of each bar row.
make_breakdown_bar <- function(breakdown_data, country, item_id, subgroup_var) {

  # Capture args as plain strings before entering dplyr data-mask context.
  # (dplyr prefers column names over function-argument names when they match,
  #  so we must use .env$ or local copies to avoid the mask shadowing them.)
  .cty <- as.character(country)
  .iid <- as.character(item_id)
  .sgv <- as.character(subgroup_var)

  dd <- breakdown_data |>
    dplyr::filter(
      as.character(.data$country)      == .cty,
      as.character(.data$item_id)      == .iid,
      as.character(.data$subgroup_var) == .sgv
    )

  if (nrow(dd) == 0) {
    return(
      plotly::plot_ly() |>
        plotly::layout(
          title = list(text = "No breakdown data available for this selection.",
                       font = list(family = "Poppins, sans-serif", size = 12)),
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
          plot_bgcolor = "white", paper_bgcolor = "white"
        )
    )
  }

  # Response ordering: respect factor levels from besd_sum if present
  ref_rows <- besd_sum |>
    dplyr::filter(as.character(.data$country) == .cty,
                  as.character(.data$item_id)  == .iid)

  if ("response_key" %in% names(ref_rows) && is.factor(ref_rows$response_key)) {
    levs       <- levels(droplevels(ref_rows$response_key))
    resp_order <- unique(sub(".*___", "", levs))
    resp_order <- resp_order[resp_order %in% as.character(dd$response)]
  } else {
    resp_order <- as.character(unique(dd$response))
  }

  n_resp    <- length(resp_order)
  resp_cols <- grDevices::colorRampPalette(c("#CC278D", "#926F97", "#4F8D9A"))(n_resp)

  # Subgroup level ordering: by total % of the last (most positive) response
  top_resp <- resp_order[n_resp]
  grp_order <- dd |>
    dplyr::filter(as.character(.data$response) == top_resp) |>
    dplyr::arrange(.data$pct) |>
    dplyr::pull(.data$subgroup_level) |>
    as.character()

  if (!length(grp_order)) {
    grp_order <- sort(unique(as.character(dd$subgroup_level)))
  }

  # Wrap long y-axis labels for display (plotly uses <br> for line breaks)
  wrap_label <- function(x) paste(strwrap(x, width = 30), collapse = "<br>")
  all_levels <- unique(as.character(dd$subgroup_level))
  wrap_map   <- stats::setNames(vapply(all_levels, wrap_label, character(1)),
                                all_levels)

  # N per subgroup level (constant across responses for a given group)
  n_per_grp <- dd |>
    dplyr::distinct(.data$subgroup_level, .data$n) |>
    dplyr::mutate(subgroup_level = as.character(.data$subgroup_level))

  fig <- plotly::plot_ly()

  for (j in seq_along(resp_order)) {
    resp <- resp_order[j]
    dr   <- dplyr::filter(dd, as.character(.data$response) == resp)
    if (nrow(dr) == 0) next

    pct_val <- dr$pct

    fig <- fig |> plotly::add_trace(
      x                = pct_val,
      y                = wrap_map[as.character(dr$subgroup_level)],
      type             = "bar",
      orientation      = "h",
      name             = resp,
      marker           = list(color = resp_cols[j],
                              line  = list(color = "white", width = 0.6)),
      text             = ifelse(pct_val >= 8, sprintf("%.0f%%", pct_val), ""),
      textposition     = "inside",
      insidetextanchor = "middle",
      textfont         = list(color = "white", size = 9,
                              family = "Poppins, sans-serif"),
      hovertemplate    = paste0(
        "<b>", resp, "</b>: %{x:.1f}%<extra>%{y}</extra>"
      ),
      showlegend = TRUE
    )
  }

  # Annotate n= to the right of each bar row
  annotations <- lapply(seq_len(nrow(n_per_grp)), function(i) {
    list(
      x         = 102,
      y         = wrap_map[n_per_grp$subgroup_level[i]],
      text      = paste0("n=", n_per_grp$n[i]),
      showarrow = FALSE,
      xref      = "x", yref = "y",
      xanchor   = "left",
      font      = list(size = 11, color = "#888",
                       family = "Poppins, sans-serif")
    )
  })

  plot_h <- max(160, length(grp_order) * 52 + 80)

  fig |> plotly::layout(
    barmode     = "stack",
    bargap      = 0.25,
    xaxis = list(
      title      = "",
      range      = c(0, 118),
      tickvals   = c(0, 25, 50, 75, 100),
      ticksuffix = "",
      gridcolor  = "#e9ecef",
      zeroline   = FALSE
    ),
    yaxis = list(
      title         = "",
      categoryorder = "array",
      categoryarray = wrap_map[grp_order],
      automargin    = TRUE,
      tickfont      = list(size = 10, family = "Poppins, sans-serif")
    ),
    annotations   = annotations,
    font          = list(family = "Poppins, sans-serif", size = 11),
    plot_bgcolor  = "white",
    paper_bgcolor = "white",
    showlegend    = TRUE,
    legend        = list(orientation = "h", x = 0, y = -0.18,
                         font = list(size = 11, family = "Poppins, sans-serif")),
    margin        = list(l = 20, r = 60, t = 10, b = 50)
  ) |>
    plotly::config(displayModeBar = FALSE)
}
