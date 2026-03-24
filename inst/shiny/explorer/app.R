library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(dplyr)
library(rbesd)
library(rnaturalearth)
library(sf)
library(reactable)
library(shinyjs)

source("R/helpers.R")

# ── Theme ─────────────────────────────────────────────────────────────────────
app_theme <- bslib::bs_theme(
  version      = 5,
  primary      = "#008e9c",
  secondary    = "#ed2290",
  success      = "#2ecc71",
  warning      = "#f39c12",
  "navbar-bg"  = "#1d1d22",
  base_font    = bslib::font_google("Poppins"),
  heading_font = bslib::font_google("Poppins"),
  "font-size-base" = "0.93rem"
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- bslib::page_navbar(
  title = shiny::tags$span(
    shiny::tags$img(
      src    = "imms-logo.svg",
      height = "32px",
      style  = "margin-right:10px; vertical-align:middle;"
    ),
    shiny::tags$b("BeSD", style = "color:#008e9c;"),
    shiny::span(" Explorer", style = "color:#ffffff;")
  ),
  id       = "main_nav",
  theme    = app_theme,
  lang     = "en",
  fillable = FALSE,
  shinyjs::useShinyjs(),
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    shiny::tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&display=swap"
    ),
    shiny::tags$style(
      ".bslib-sidebar-layout { --_sidebar-bg: #f4f4f4; }
       .bslib-sidebar-layout > .sidebar { background-color: #f4f4f4 !important; }
      "
    ),
    shiny::tags$script(shiny::HTML(
      "Shiny.addCustomMessageHandler('radarZoom', function(scale) {
        var el = document.getElementById('spider_plot');
        if (el) {
          el.style.transform = 'scale(' + scale + ')';
          el.style.transformOrigin = 'top center';
        }
      });"
    ))
  ),

  # ── Tab 1: World Map ────────────────────────────────────────────────────────
  bslib::nav_panel(
    title = shiny::span(shiny::icon("globe"), " Global Overview"),
    value = "Global Overview",

    bslib::layout_sidebar(
      fillable = FALSE,
      sidebar = bslib::sidebar(
        width = 250,
        bg    = "#f4f4f4",
        shiny::tags$div(class = "sidebar-label", "Country"),
        shiny::selectInput(
          "download_country", label = NULL,
          choices  = countries,
          selected = countries[1],
          width    = "100%"
        ),
        shiny::tags$div(class = "sidebar-label mt-2", "Include in report"),
        shiny::checkboxGroupInput(
          "report_sections", label = NULL,
          choices  = c("Country profile"        = "profile",
                       "Demographic breakdowns" = "demographics"),
          selected = "profile"
        ),
        shiny::tags$div(class = "sidebar-label mt-2", "Format"),
        shiny::selectInput(
          "report_format", label = NULL,
          choices = c("HTML" = "html", "PDF" = "pdf"),
          width   = "100%"
        ),
        shiny::downloadButton(
          "download_report",
          label = "Download Report",
          class = "btn-sm btn-outline-primary w-100"
        ),
        shiny::hr(class = "my-2"),
        shiny::tags$p(
          class = "text-muted",
          style = "font-size:0.78rem;",
          shiny::icon("circle-info"),
          " Visit the ",
          shiny::tags$b("Country Profile"),
          " and ",
          shiny::tags$b("Demographics"),
          " tabs to configure country comparisons and demographic options before downloading."
        )
      ),

    bslib::card(
      class = "border-0 shadow-sm",
      bslib::card_header(
        class = "d-flex align-items-center gap-3 flex-wrap",
        shiny::div(
          class = "fw-semibold text-dark me-auto",
          shiny::icon("globe-africa", class = "text-primary me-1"),
          "BeSD indicator map"
        ),
        shiny::div(
          style = "min-width:300px; max-width:480px;",
          shiny::selectInput(
            "map_metric",
            label   = NULL,
            choices = map_choices,
            width   = "100%"
          )
        ),
        shiny::div(
          style = "min-width:160px; max-width:260px;",
          shiny::selectInput(
            "map_level",
            label    = NULL,
            choices  = initial_level_choices,
            selected = initial_level_selected,
            width    = "100%"
          )
        ),
        shiny::div(
          id    = "top2box_div",
          style = "display:none;",
          shiny::checkboxInput("top2box", "Combine top two responses", value = FALSE)
        ),
        shiny::div(
          class = "text-muted small",
          shiny::icon("circle-info"), " Click a country to view its profile"
        ),
        shiny::div(
          class = "w-100 text-muted",
          style = "font-size: 0.85rem; font-weight: normal;",
          shiny::icon("circle-info"),
          " Item: ",
          shiny::textOutput("map_card_question", inline = TRUE)
        )
      ),
      leaflet::leafletOutput("world_map", height = "420px"),
    ),

    # ── Country Rankings (below map) ───────────────────────────────────────
    bslib::card(
      class       = "border-0 shadow-sm mt-3",
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex align-items-center gap-2 flex-wrap",
        shiny::icon("ranking-star", class = "text-primary me-1"),
        shiny::span(class = "fw-semibold", "BeSD indicator rankings"),
        shiny::div(
          class = "w-100 text-muted",
          style = "font-size: 0.85rem; font-weight: normal;",
          shiny::icon("circle-info"),
          " Item: ",
          shiny::textOutput("map_question", inline = TRUE)
        )
      ),
      plotly::plotlyOutput("ranked_plot", height = "420px"),
      shiny::uiOutput("ranked_ci_note")
    )

    ) # end layout_sidebar
  ),

  # ── Tab 2: Country Profile ──────────────────────────────────────────────────
  bslib::nav_panel(
    title = shiny::span(shiny::icon("flag"), " Country Profile"),
    value = "Country Profile",

    bslib::layout_sidebar(
      fillable = FALSE,
      sidebar = bslib::sidebar(
        width = 226,
        bg    = "#f4f4f4",
        shiny::tags$div(class = "sidebar-label", "Country"),
        shiny::selectInput(
          "profile_country", label = NULL,
          choices  = countries,
          selected = countries[1]
        ),
      ),

      # ── Sample composition (full width, top) ────────────────────────────────
      bslib::card(
        class       = "border-0 shadow-sm mb-3",
        full_screen = TRUE,
        bslib::card_header(
          shiny::icon("users", class = "text-primary me-1"),
          shiny::span(class = "fw-semibold",
                      shiny::textOutput("title_sample", inline = TRUE))
        ),
        reactable::reactableOutput("demo_table", height = "397px"),
        bslib::card_footer(
          class = "small text-muted",
          style = "font-weight: normal;",
          shiny::icon("circle-info"),
          " N varies between demographics because missing responses are excluded per variable."
        )
      ),

      # ── Response distribution bar chart (full width) ────────────────────────
      bslib::card(
        class = "border-0 shadow-sm mb-3",
        bslib::card_header(
          class = "d-flex align-items-center gap-2 flex-wrap",
          shiny::icon("chart-bar", class = "text-primary"),
          shiny::span(class = "fw-semibold",
                      shiny::textOutput("title_bar", inline = TRUE)),
          shiny::div(
            class = "ms-auto",
            style = "min-width:170px; max-width:220px;",
            shiny::selectInput(
              "domain_filter", label = NULL,
              choices = c(
                "All domains"        = "all",
                "Thinking & Feeling" = "thinking and feeling",
                "Social Processes"   = "social processes",
                "Practical Issues"   = "practical issues"
              ),
              selected = "all", width = "100%"
            )
          )
        ),
        shiny::uiOutput("bar_plot_ui")
      ),

      # ── Global comparison (6) + Radar (6) side by side ──────────────────────
      bslib::layout_columns(
        style = "grid-template-columns: 57fr 43fr;",
        gap   = "1rem",

        bslib::card(
          class       = "border-0 shadow-sm",
          full_screen = TRUE,
          bslib::card_header(
            class = "d-flex align-items-center gap-2 flex-wrap",
            shiny::icon("circle-nodes", class = "text-primary me-1"),
            shiny::span(class = "fw-semibold",
                        shiny::textOutput("title_radar", inline = TRUE)),
            shiny::div(
              class = "ms-auto d-flex align-items-center gap-2",
              shiny::tags$small("Compare:", class = "text-muted"),
              shiny::div(
                style = "min-width:180px;",
                shiny::selectInput(
                  "compare_country", label = NULL,
                  choices  = countries,
                  selected = NULL,
                  multiple = TRUE,
                  width    = "100%"
                )
              ),
              shiny::tags$div(
                class = "btn-group btn-group-sm",
                shiny::actionButton("radar_zoom_in",  "+",
                                    class = "btn btn-outline-secondary",
                                    style = "padding: 2px 8px; font-weight: 600;"),
                shiny::actionButton("radar_zoom_out", "\u2212",
                                    class = "btn btn-outline-secondary",
                                    style = "padding: 2px 8px; font-weight: 600;")
              )
            )
          ),
          shiny::uiOutput("spider_plot_ui"),
          bslib::card_footer(
            class = "small text-muted",
            style = "font-weight: normal;",
            shiny::icon("circle-info"),
            " Each spoke shows the % of caregivers giving a positive response: for scale items both most-favourable categories are summed; for yes/no items the most favourable category is shown."
          )
        ),

        bslib::card(
          class = "border-0 shadow-sm",
          bslib::card_header(
            class = "d-flex align-items-center gap-2 flex-wrap",
            shiny::icon("globe-africa", class = "text-primary"),
            shiny::span(class = "fw-semibold",
                        shiny::textOutput("title_comparison", inline = TRUE)),
            shiny::div(
              class = "ms-auto",
              style = "min-width:200px; max-width:380px;",
              shiny::selectInput(
                "comparison_item", label = NULL,
                choices = map_choices, width = "100%"
              )
            ),
            shiny::div(
              class = "w-100 small text-muted",
              style = "font-weight: normal;",
              shiny::icon("circle-info"),
              " Item: ",
              shiny::textOutput("comparison_question", inline = TRUE)
            )
          ),
          shiny::uiOutput("comparison_plot_ui")
        )
      )
    )
  ),

  # ── Tab 3: BeSD by Demographic ───────────────────────────────────────────────
  bslib::nav_panel(
    title = shiny::span(shiny::icon("users"), " BeSD by Demographic"),
    value = "Demographic Breakdown",

    bslib::layout_sidebar(
        fillable = FALSE,
        sidebar = bslib::sidebar(
          width = 226,
          bg    = "#f4f4f4",
          shiny::tags$div(class = "sidebar-label", "Countries"),
          shiny::checkboxInput("bk_all_countries", "Show all countries",
                               value = FALSE),
          shiny::conditionalPanel(
            condition = "!input.bk_all_countries",
            shiny::selectInput(
              "bk_country", label = NULL,
              choices  = countries,
              selected = countries[1],
              multiple = TRUE
            )
          ),
          shiny::tags$div(class = "sidebar-label", "BeSD item"),
          shiny::selectInput(
            "bk_item", label = NULL,
            choices = breakdown_item_choices,
            width   = "100%"
          ),
          shiny::hr(class = "my-2"),
          shiny::tags$div(class = "sidebar-label", "Demographic variables"),
          shiny::checkboxGroupInput(
            "bk_dem_vars",
            label    = NULL,
            choices  = setNames(names(breakdown_var_meta),
                                unname(breakdown_var_meta)),
            selected = names(breakdown_var_meta)
          )
        ),

        shiny::div(
          shiny::uiOutput("breakdown_panels")
        )
    )
  ),

)


# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Auto-close session after 2 minutes of user inactivity
  shinyjs::runjs("
    var idleTime = 0;
    setInterval(function() {
      idleTime++;
      if (idleTime >= 2) {
        Shiny.setInputValue('timeoutInput', true);
      }
    }, 60000);
    $(document).on('mousemove keypress click scroll', function() {
      idleTime = 0;
    });
  ")
  shiny::observeEvent(input$timeoutInput, {
    session$close()
  })

  # Shared reactive: which country is in focus
  selected_country <- shiny::reactiveVal(countries[1])

  shiny::observeEvent(input$profile_country, {
    selected_country(input$profile_country)
    radar_scale(1.0)
    session$sendCustomMessage("radarZoom", 1.0)
  })

  # ── World Map ──────────────────────────────────────────────────────────────

  # TRUE when the selected map metric is an ordinal item type
  is_ordinal <- shiny::reactive({
    any(besd_sum$item_id == input$map_metric &
          besd_sum$item_type == "ordinal", na.rm = TRUE)
  })

  output$world_map <- leaflet::renderLeaflet({
    layers <- make_map_layers(.initial_item_id, initial_level_selected)
    base <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 1)) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.VoyagerNoLabels) |>
      leaflet::setView(lng = 20, lat = 10, zoom = 2) |>
      leaflet::setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)
    apply_map_layers(base, layers)
  })

  # When item changes: update the level dropdown, defaulting to top-box response
  shiny::observeEvent(input$map_metric, {
    levs     <- item_responses(input$map_metric)
    top_resp <- topbox_all |>
      dplyr::filter(.data$item_id == input$map_metric) |>
      dplyr::pull(.data$topbox_label) |>
      unique()
    # topbox_label may be comma-separated; take the first token
    top_resp <- trimws(strsplit(top_resp[1], ",")[[1]])[1]
    sel <- if (length(top_resp) > 0 && !is.na(top_resp) && top_resp %in% levs) top_resp else levs[1]
    shiny::updateSelectInput(session, "map_level", choices = levs, selected = sel)
    # Always redraw map on metric change — level observer won't fire if sel is
    # unchanged (same label across items), leaving the map stale.
    layers <- make_map_layers(input$map_metric, sel, top2 = isTRUE(input$top2box))
    proxy  <- leaflet::leafletProxy("world_map") |>
      leaflet::clearShapes() |>
      leaflet::clearControls()
    apply_map_layers(proxy, layers)
    # Show/hide the top-2-box checkbox depending on item type
    shinyjs::toggle(id = "top2box_div", condition = is_ordinal())
    if (!is_ordinal()) shiny::updateCheckboxInput(session, "top2box", value = FALSE)
    shinyjs::toggleState(id = "map_level", condition = !isTRUE(input$top2box))
  })

  # When level changes: redraw map (ignoreInit avoids double-render on startup)
  shiny::observeEvent(input$map_level, {
    shiny::req(nzchar(input$map_level))
    layers <- make_map_layers(input$map_metric, input$map_level,
                              top2 = isTRUE(input$top2box))
    proxy  <- leaflet::leafletProxy("world_map") |>
      leaflet::clearShapes() |>
      leaflet::clearControls()
    apply_map_layers(proxy, layers)
  }, ignoreInit = TRUE)

  # When top-2-box toggle changes: disable/enable level dropdown and redraw map
  shiny::observeEvent(input$top2box, {
    shinyjs::toggleState(id = "map_level", condition = !isTRUE(input$top2box))
    layers <- make_map_layers(input$map_metric, input$map_level,
                              top2 = isTRUE(input$top2box))
    proxy  <- leaflet::leafletProxy("world_map") |>
      leaflet::clearShapes() |>
      leaflet::clearControls()
    apply_map_layers(proxy, layers)
  }, ignoreInit = TRUE)

  # Map click → navigate to country profile
  shiny::observeEvent(input$world_map_shape_click, {
    iso <- input$world_map_shape_click$id
    cty <- iso_lookup$country[iso_lookup$iso3 == iso]
    if (length(cty) == 1 && cty %in% countries) {
      selected_country(cty)
      shiny::updateSelectInput(session, "profile_country", selected = cty)
      bslib::nav_select("main_nav", "Country Profile")
    }
  })

  # Ranked plot click → navigate to country profile
  shiny::observeEvent(plotly::event_data("plotly_click", source = "ranked_plot"), {
    click <- plotly::event_data("plotly_click", source = "ranked_plot")
    cty   <- as.character(click$y)
    if (length(cty) == 1 && cty %in% countries) {
      selected_country(cty)
      shiny::updateSelectInput(session, "profile_country", selected = cty)
      bslib::nav_select("main_nav", "Country Profile")
    }
  })

  # ── Country Profile ────────────────────────────────────────────────────────

  output$bar_country_label  <- shiny::renderText(selected_country())
  output$title_sample       <- shiny::renderText(paste0(selected_country(), ": Sample composition"))
  output$title_bar          <- shiny::renderText(paste0(selected_country(), ": BeSD response profile"))
  output$title_radar        <- shiny::renderText(paste0(selected_country(), ": Radar profile"))
  output$title_comparison   <- shiny::renderText(paste0(selected_country(), ": Global comparison"))

  # Dynamic-height container(s) for bar chart
  # When "all domains" is selected, render one plot per domain stacked vertically.
  output$bar_plot_ui <- shiny::renderUI({
    cty <- selected_country()
    if (input$domain_filter == "all") {
      domain_ids <- c(
        "thinking and feeling" = "bar_plot_tf",
        "social processes"     = "bar_plot_sp",
        "practical issues"     = "bar_plot_pi"
      )
      domain_labels <- c(
        "thinking and feeling" = "Thinking & Feeling",
        "social processes"     = "Social Processes",
        "practical issues"     = "Practical Issues"
      )
      plots <- lapply(names(domain_ids), function(dom) {
        h <- bar_plot_height(cty, dom)
        shiny::tagList(
          shiny::tags$p(
            class = "fw-semibold text-muted mb-1 mt-2",
            style = "font-size: 0.8rem; text-transform: uppercase; letter-spacing: 0.05em;",
            domain_labels[[dom]]
          ),
          plotly::plotlyOutput(domain_ids[[dom]], height = paste0(h, "px"))
        )
      })
      do.call(shiny::tagList, plots)
    } else {
      h <- bar_plot_height(cty, input$domain_filter)
      plotly::plotlyOutput("bar_plot", height = paste0(h, "px"))
    }
  })

  # Single-domain plot (used when a specific domain is selected)
  output$bar_plot <- plotly::renderPlotly({
    shiny::req(input$domain_filter != "all")
    cty <- selected_country()
    dd  <- besd_sum |> dplyr::filter(.data$country == cty)
    make_profile_bar(dd, domain_filter = input$domain_filter)
  })

  # Per-domain plots (used when "all domains" is selected)
  domain_plot_map <- list(
    bar_plot_tf = "thinking and feeling",
    bar_plot_sp = "social processes",
    bar_plot_pi = "practical issues"
  )
  for (.pid in names(domain_plot_map)) {
    local({
      pid        <- .pid
      dom_name   <- domain_plot_map[[pid]]
      output[[pid]] <- plotly::renderPlotly({
        shiny::req(input$domain_filter == "all")
        cty <- selected_country()
        dd  <- besd_sum |> dplyr::filter(.data$country == cty)
        make_profile_bar(dd, domain_filter = dom_name)
      })
    })
  }

  # Radar zoom: CSS transform scale on the whole figure
  radar_scale <- shiny::reactiveVal(1.0)

  shiny::observeEvent(input$radar_zoom_in, {
    radar_scale(min(2.0, radar_scale() + 0.15))
    session$sendCustomMessage("radarZoom", radar_scale())
  })

  shiny::observeEvent(input$radar_zoom_out, {
    radar_scale(max(0.5, radar_scale() - 0.15))
    session$sendCustomMessage("radarZoom", radar_scale())
  })

  # Radar (spider) chart — height matches the comparison plot for the shared row
  output$spider_plot_ui <- shiny::renderUI({
    h <- as.integer(max(280L, length(countries) * 28L + 60L) * 0.67)
    plotly::plotlyOutput("spider_plot", height = paste0(h, "px"))
  })

  output$spider_plot <- plotly::renderPlotly({
    cty        <- selected_country()
    comp_valid <- input$compare_country[input$compare_country %in% countries]
    ctys       <- c(cty, comp_valid)

    dom_levels <- c("thinking and feeling", "social processes", "practical issues")

    # Order items by explicit domain sequence, then item_id within domain
    item_info <- topbox_all |>
      dplyr::filter(.data$country == cty) |>
      dplyr::distinct(.data$item_id, .data$domain, .data$question_short) |>
      dplyr::mutate(
        domain_f = factor(.data$domain, levels = dom_levels)
      ) |>
      dplyr::arrange(.data$domain_f, .data$item_id)

    if (nrow(item_info) == 0) {
      return(plotly::plot_ly() |> plotly::layout(title = "No data available"))
    }

    n_items   <- nrow(item_info)
    step      <- 360 / n_items
    theta_deg <- (seq_len(n_items) - 1L) * step   # one angle per item, in degrees

    # Angular axis tick labels
    tick_text <- vapply(
      dplyr::coalesce(item_info$question_short, item_info$item_id),
      function(x) paste(strwrap(x, width = 15L), collapse = "\n"),
      character(1L)
    )

    # ── Score computation: top-2-box for ordinal, topbox for binary ───────────
    ordinal_ids <- unique(besd_sum$item_id[besd_sum$item_type == "ordinal"])
    radar_items     <- item_info$item_id
    ordinal_radar   <- radar_items[radar_items %in% ordinal_ids]
    nonordinal_radar <- radar_items[!radar_items %in% ordinal_ids]

    if (length(ordinal_radar) > 0L) {
      t2_list <- lapply(ordinal_radar, function(iid) {
        top2_levs <- get_top2_levels(iid)
        besd_sum |>
          dplyr::filter(
            .data$country %in% ctys,
            .data$item_id == iid,
            as.character(.data$response) %in% top2_levs
          ) |>
          dplyr::group_by(.data$country, .data$item_id) |>
          dplyr::summarise(pct = sum(.data$pct, na.rm = TRUE), .groups = "drop")
      })
      t2_scores <- dplyr::bind_rows(t2_list)
    } else {
      t2_scores <- data.frame(country  = character(),
                              item_id  = character(),
                              pct      = numeric(),
                              stringsAsFactors = FALSE)
    }

    tb_scores <- topbox_all |>
      dplyr::filter(.data$country %in% ctys,
                    .data$item_id %in% nonordinal_radar) |>
      dplyr::select("country", "item_id", "pct")

    all_scores <- dplyr::bind_rows(t2_scores, tb_scores)

    # ── Domain separator geometry ─────────────────────────────────────────────
    dom_present <- dom_levels[dom_levels %in% item_info$domain]
    dom_counts  <- vapply(dom_present,
                          function(d) sum(item_info$domain == d, na.rm = TRUE),
                          integer(1L))
    cum_ends    <- cumsum(dom_counts)   # 1-indexed end-of-domain positions

    # One separator per domain boundary (including wrap-around after last domain).
    # Item i (1-indexed) sits at (i-1)*step degrees; midpoint between item[k]
    # and item[k+1] is at (k - 0.5)*step.
    sep_angles <- (cum_ends - 1L) * step + step / 2

    # Domain label position = midpoint of each domain's angular arc
    dom_starts  <- c(1L, cum_ends[-length(cum_ends)] + 1L)
    dom_centres <- ((dom_starts - 1L) * step + (cum_ends - 1L) * step) / 2
    dom_short   <- c(
      "thinking and feeling" = "Thinking &\nFeeling",
      "social processes"     = "Social\nProcesses",
      "practical issues"     = "Practical\nIssues"
    )


    # ── Build traces ─────────────────────────────────────────────────────────
    line_colors <- c("#008e9c", "#ed2290", "#f39c12", "#9b59b6",
                     "#27ae60", "#e74c3c", "#2980b9", "#d35400")
    fill_colors <- c("rgba(0,142,156,0.15)",  "rgba(237,34,144,0.15)",
                     "rgba(243,156,18,0.15)", "rgba(155,89,182,0.15)",
                     "rgba(39,174,96,0.15)",  "rgba(231,76,60,0.15)",
                     "rgba(41,128,185,0.15)", "rgba(211,84,0,0.15)")

    fig <- plotly::plot_ly()

    # Country data traces
    for (k in seq_along(ctys)) {
      c_name  <- ctys[k]
      col_idx <- ((k - 1L) %% length(line_colors)) + 1L

      r_vals <- vapply(radar_items, function(iid) {
        v <- all_scores$pct[all_scores$country == c_name &
                              all_scores$item_id == iid]
        if (length(v) == 0L || all(is.na(v))) NA_real_ else v[[1L]]
      }, numeric(1L))

      fig <- fig |> plotly::add_trace(
        type          = "scatterpolar",
        r             = c(r_vals, r_vals[1L]),
        theta         = c(theta_deg, theta_deg[1L]),
        thetaunit     = "degrees",
        mode          = "lines+markers",
        name          = c_name,
        fill          = "toself",
        fillcolor     = fill_colors[col_idx],
        line          = list(color = line_colors[col_idx], width = 2.5),
        marker        = list(color = line_colors[col_idx], size = 5),
        hovertemplate = paste0("<b>", c_name, "</b>: %{r:.1f}%<extra></extra>")
      )
    }

    # Domain separator spokes: thick radial lines, one per domain boundary
    # (including the wrap-around between last and first domain).
    # range = c(0, 100) so r=100 lands exactly on the outer circle.
    for (ang in sep_angles) {
      fig <- fig |> plotly::add_trace(
        type       = "scatterpolar",
        r          = c(0, 100),
        theta      = c(ang, ang),
        thetaunit  = "degrees",
        mode       = "lines",
        line       = list(color = "#444444", width = 2.5),
        showlegend = FALSE,
        hoverinfo  = "none"
      )
    }

    # Domain labels inside the circle as text traces so they rotate with the
    # chart when the user interacts with it.  Placed at r = 78 (inner arc).
    for (k in seq_along(dom_present)) {
      fig <- fig |> plotly::add_trace(
        type       = "scatterpolar",
        r          = 78,
        theta      = dom_centres[k],
        thetaunit  = "degrees",
        mode       = "text",
        text       = paste0("<b>", dom_short[dom_present[k]], "</b>"),
        textfont   = list(size = 12L, color = "#333333",
                          family = "Poppins, sans-serif"),
        showlegend = FALSE,
        hoverinfo  = "none"
      )
    }

    fig |> plotly::layout(
      polar = list(
        radialaxis = list(
          visible    = TRUE,
          range      = c(0, 100),
          ticksuffix = "%",
          tickfont   = list(size = 9, family = "Poppins, sans-serif"),
          gridcolor  = "#e9ecef"
        ),
        angularaxis = list(
          tickvals  = theta_deg,
          ticktext  = tick_text,
          tickfont  = list(size = 9, family = "Poppins, sans-serif"),
          direction = "clockwise",
          rotation  = 90,
          gridcolor = "#e9ecef"
        ),
        bgcolor = "white"
      ),
      showlegend = length(ctys) > 1L,
      legend = list(
        orientation = "h",
        x           = 0,
        xanchor     = "left",
        y           = -0.05
      ),
      font          = list(family = "Poppins, sans-serif"),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin        = list(l = 60, r = 60, t = 30, b = 30)
    ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # ── Global comparison (Country Profile) ───────────────────────────────────

  output$comparison_plot_ui <- shiny::renderUI({
    h <- as.integer(max(280L, length(countries) * 28L + 60L) * 0.67)
    plotly::plotlyOutput("comparison_plot", height = paste0(h, "px"))
  })

  output$comparison_question <- shiny::renderText({
    shiny::req(input$comparison_item)
    q <- besd_sum |>
      dplyr::filter(.data$item_id == input$comparison_item) |>
      dplyr::pull(.data$question) |>
      unique()
    q <- q[!is.na(q) & nzchar(q)]
    if (length(q) > 0) q[1] else input$comparison_item
  })

  output$comparison_plot <- plotly::renderPlotly({
    shiny::req(input$comparison_item)
    make_country_comparison_bar(
      besd_sum      = besd_sum,
      item_id       = input$comparison_item,
      focal_country = selected_country()
    )
  })

  # Demographics table
  output$demo_table <- reactable::renderReactable({
    cty <- selected_country()
    dd <- demo_sum |>
      dplyr::filter(as.character(.data$country) == cty) |>
      dplyr::arrange(.data$item_id, .data$response) |>
      dplyr::transmute(
        variable = as.character(.data$question_short),
        category = as.character(.data$response),
        n        = .data$n,
        pct      = round(.data$pct, 1)
      )

    if (nrow(dd) == 0) {
      return(reactable::reactable(data.frame(Message = paste("No demographic data for", cty))))
    }

    reactable::reactable(
      dd,
      groupBy         = "variable",
      defaultExpanded = FALSE,
      columns = list(
        variable = reactable::colDef(
          name     = "Demographic",
          minWidth = 100
        ),
        category = reactable::colDef(
          name     = "Category",
          minWidth = 140
        ),
        n = reactable::colDef(
          name      = "N",
          width     = 65,
          align     = "right",
          aggregate = "sum",
          format    = reactable::colFormat(separators = TRUE)
        ),
        pct = reactable::colDef(
          name      = "%",
          width     = 65,
          align     = "right",
          aggregate = reactable::JS("function() { return '' }"),
          format    = reactable::colFormat(suffix = "%", digits = 1)
        )
      ),
      compact   = TRUE,
      highlight = TRUE,
      style     = list(fontSize = "0.82rem", fontFamily = "Poppins, sans-serif",
                       fontWeight = "normal"),
      theme = reactable::reactableTheme(
        headerStyle = list(
          fontSize      = "0.74rem",
          fontWeight    = 600,
          color         = "#8492a6",
          textTransform = "uppercase",
          letterSpacing = "0.04em"
        )
      ),
      rowStyle = function(index) list(fontWeight = "normal")
    )
  })

  # ── Download Report ────────────────────────────────────────────────────────

  # Keep the Global Overview country selector in sync with the profile selector
  shiny::observeEvent(input$profile_country, {
    shiny::updateSelectInput(session, "download_country", selected = input$profile_country)
  })
  shiny::observeEvent(input$download_country, {
    shiny::updateSelectInput(session, "profile_country", selected = input$download_country)
    shiny::updateSelectInput(session, "bk_country",      selected = input$download_country)
    selected_country(input$download_country)
  })

  output$download_report <- shiny::downloadHandler(
    filename = function() {
      ext <- if (isTRUE(input$report_format == "pdf")) ".pdf" else ".html"
      paste0("BeSD_Report_", gsub(" ", "_", input$download_country), "_",
             format(Sys.Date(), "%Y%m%d"), ext)
    },
    content = function(file) {
      cty <- input$download_country
      fmt <- if (isTRUE(input$report_format == "pdf")) "pdf_document" else "html_document"

      rmd_template <- file.path(getwd(), "report_template.Rmd")
      if (!file.exists(rmd_template)) {
        rmd_template <- system.file(
          "shiny/explorer/report_template.Rmd", package = "rbesd"
        )
      }

      params <- list(
        country           = cty,
        besd_sum          = besd_sum,
        demo_sum          = demo_sum,
        domain_filter     = input$domain_filter,
        compare_countries = input$compare_country[input$compare_country %in% countries],
        comparison_item   = input$comparison_item,
        include_profile   = "profile"      %in% input$report_sections,
        include_demos     = "demographics" %in% input$report_sections,
        breakdown_sum     = if ("demographics" %in% input$report_sections) breakdown_sum else NULL,
        bk_item           = input$bk_item,
        bk_dem_vars       = input$bk_dem_vars
      )

      rmarkdown::render(
        input         = rmd_template,
        output_format = fmt,
        output_file   = file,
        params        = params,
        envir         = new.env(parent = globalenv()),
        quiet         = TRUE
      )
    }
  )

  # ── Global Comparison ─────────────────────────────────────────────────────

  # Dynamic x-axis label for ranked plot — mirrors the map level dropdown
  ranked_xlab <- shiny::reactive({
    if (isTRUE(input$top2box) && is_ordinal()) {
      top2_levs <- get_top2_levels(input$map_metric)
      return(paste0(top2_levs[1], " + ", top2_levs[2], " (%)"))
    }
    lev <- input$map_level
    if (is.null(lev) || !nzchar(lev)) return("Response (%)")
    paste0(lev, " (%)")
  })

  output$ranked_ci_note <- shiny::renderUI({
    if (!isTRUE(input$top2box) || !is_ordinal()) return(NULL)
    top2_levs <- get_top2_levels(input$map_metric)
    shiny::div(
      class = "text-muted px-3 pb-2",
      style = "font-size: 0.8rem;",
      shiny::icon("circle-info", class = "me-1"),
      paste0(
        'Bars show the combined percentage of \u201c', top2_levs[1],
        '\u201d and \u201c', top2_levs[2],
        '\u201d responses. Credible intervals are derived by error propagation ',
        '(combining the standard errors of each response category) and are capped at 0\u2013100%.'
      )
    )
  })

  map_question_text <- shiny::reactive({
    shiny::req(input$map_metric)
    q <- besd_sum |>
      dplyr::filter(.data$item_id == input$map_metric) |>
      dplyr::pull(.data$question) |>
      unique()
    q <- q[!is.na(q) & nzchar(q)]
    txt <- if (length(q) > 0) q[1] else input$map_metric
    if (isTRUE(input$top2box) && is_ordinal()) {
      top2_levs <- get_top2_levels(input$map_metric)
      txt <- paste0(txt, ' (combining \u201c', top2_levs[1],
                    '\u201d and \u201c', top2_levs[2], '\u201d)')
    }
    txt
  })

  output$map_card_question <- shiny::renderText(map_question_text())
  output$map_question      <- shiny::renderText(map_question_text())

  output$ranked_item_label <- shiny::renderText({
    meta <- item_meta[item_meta$item_id == input$map_metric, ]
    if (nrow(meta) > 0 && !is.na(meta$question_short[1])) {
      meta$question_short[1]
    } else {
      input$map_metric
    }
  })

  output$ranked_plot <- plotly::renderPlotly({
    shiny::req(input$map_metric, input$map_level)

    if (isTRUE(input$top2box) && is_ordinal()) {
      top2_levs <- get_top2_levels(input$map_metric)
      has_ci    <- all(c("lcl", "ucl") %in% names(besd_sum))
      tb_grp    <- besd_sum |>
        dplyr::filter(
          .data$item_id == input$map_metric,
          as.character(.data$response) %in% top2_levs
        ) |>
        dplyr::group_by(.data$country)
      if (has_ci) {
        tb <- tb_grp |>
          dplyr::summarise(
            pct = sum(.data$pct, na.rm = TRUE),
            k   = sum(.data$n_resp, na.rm = TRUE),
            n   = dplyr::first(.data$n),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            # Wilson score CI: z²/(4n²) inside sqrt, NOT z²/(4n)
            p_hat  = .data$k / .data$n,
            denom  = 1 + 1.96^2 / .data$n,
            centre = (.data$p_hat + 1.96^2 / (2 * .data$n)) / .data$denom,
            half   = 1.96 * sqrt(.data$p_hat * (1 - .data$p_hat) / .data$n +
                                   1.96^2 / (4 * .data$n^2)) / .data$denom,
            lcl    = pmax(0,   (.data$centre - .data$half) * 100),
            ucl    = pmin(100, (.data$centre + .data$half) * 100)
          ) |>
          dplyr::select(-"k", -"n", -"p_hat", -"denom", -"centre", -"half")
      } else {
        tb <- tb_grp |>
          dplyr::summarise(pct = sum(.data$pct, na.rm = TRUE), .groups = "drop")
      }
      tb <- tb |>
        dplyr::arrange(.data$pct) |>
        dplyr::mutate(
          country_f = factor(as.character(.data$country),
                             levels = as.character(.data$country))
        )
    } else {
      tb <- besd_sum |>
        dplyr::filter(
          .data$item_id == input$map_metric,
          as.character(.data$response) == input$map_level
        ) |>
        dplyr::arrange(.data$pct) |>
        dplyr::mutate(
          country_f = factor(as.character(.data$country),
                             levels = as.character(.data$country))
        )
    }

    ci_present <- all(c("lcl", "ucl") %in% names(tb)) &&
      any(!is.na(tb$lcl))

    tb <- tb |> dplyr::mutate(
      hover_txt = if (ci_present) {
        paste0("<b>", .data$country, "</b>: ", round(.data$pct, 1), "%",
               "<br>95% CI: ", round(.data$lcl, 1),
               "\u2013", round(.data$ucl, 1), "%")
      } else {
        paste0("<b>", .data$country, "</b>: ", round(.data$pct, 1), "%")
      }
    )

    if (nrow(tb) == 0) {
      return(
        plotly::plot_ly() |>
          plotly::layout(title = "No data for this item")
      )
    }

    # Per-point colours matching the map palette
    vals       <- tb$pct[!is.na(tb$pct)]
    data_range <- if (length(vals) >= 2) range(vals) else c(0, 100)
    span       <- max(data_range[2] - data_range[1], 5)
    pad        <- span * 0.08
    col_domain <- c(max(0, data_range[1] - pad), min(100, data_range[2] + pad))
    col_fn     <- grDevices::colorRampPalette(c("#CC278D", "#926F97", "#4F8D9A"))
    col_pal    <- col_fn(256L)
    tb <- tb |> dplyr::mutate(
      pt_color = {
        idx <- round((pct - col_domain[1]) / (col_domain[2] - col_domain[1]) * 255) + 1L
        col_pal[pmax(1L, pmin(256L, idx))]
      }
    )

    global_mean <- mean(tb$pct, na.rm = TRUE)
    xlab        <- ranked_xlab()

    fig <- plotly::plot_ly(source = "ranked_plot")

    # CI bars — one segment per country so each gets its own literal colour
    if (ci_present) {
      for (i in seq_len(nrow(tb))) {
        fig <- fig |> plotly::add_segments(
          x         = tb$lcl[i], xend = tb$ucl[i],
          y         = as.character(tb$country_f[i]),
          yend      = as.character(tb$country_f[i]),
          line      = list(color = tb$pt_color[i], width = 3),
          hoverinfo = "none",
          showlegend = FALSE
        )
      }
    }

    # Points
    fig <- fig |> plotly::add_markers(
      data          = tb,
      x             = ~pct, y = ~country_f,
      marker        = list(color = ~pt_color, size = 9),
      text          = ~hover_txt,
      hovertemplate = "%{text}<extra></extra>",
      showlegend    = FALSE
    )

    # Global mean line + label
    fig |> plotly::layout(
      shapes = list(list(
        type  = "line",
        layer = "below",
        x0    = global_mean, x1 = global_mean,
        y0    = 0, y1 = 1, yref = "paper",
        line  = list(color = "#aaa", dash = "dash", width = 1.5)
      )),
      annotations = list(list(
        x        = global_mean + 1, y = 0.97,
        xref     = "x", yref = "paper",
        text     = paste0("Mean: ", round(global_mean, 1), "%"),
        showarrow = FALSE,
        xanchor  = "left",
        yanchor  = "top",
        font     = list(color = "#999", size = 10,
                        family = "Poppins, sans-serif")
      )),
      xaxis = list(
        title    = xlab,
        range    = c(0, 100),
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        title         = "",
        categoryorder = "array",
        categoryarray = levels(tb$country_f),
        automargin    = TRUE,
        showgrid      = FALSE,
        tickfont      = list(size = 11, family = "Poppins, sans-serif")
      ),
      font          = list(family = "Poppins, sans-serif"),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      showlegend    = FALSE,
      margin        = list(l = 20, r = 20, t = 10, b = 50)
    ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # ── BeSD by Demographic tab ───────────────────────────────────────────────

  # Render one card per (demographic variable × country) combination
  output$breakdown_panels <- shiny::renderUI({
    vars <- input$bk_dem_vars
    ctys <- if (isTRUE(input$bk_all_countries)) countries else input$bk_country
    if (is.null(vars) || length(vars) == 0) {
      return(shiny::p(class = "text-muted mt-3",
                      "Select at least one demographic variable in the sidebar."))
    }
    if (is.null(ctys) || length(ctys) == 0) {
      return(shiny::p(class = "text-muted mt-3",
                      "Select at least one country in the sidebar."))
    }

    cards <- list()
    for (var in vars) {
      lbl <- unname(breakdown_var_meta[var])
      lbl <- if (length(lbl) == 1 && !is.na(lbl)) lbl else var

      for (cty in ctys) {
        san_cty  <- gsub("[^a-zA-Z0-9]", "_", cty)
        plot_id  <- paste0("bk_plot_",    san_cty, "_", var)
        label_id <- paste0("bk_n_label_", san_cty, "_", var)

        n_levels <- breakdown_sum |>
          dplyr::filter(as.character(.data$country) == cty,
                        .data$subgroup_var == var) |>
          dplyr::pull(.data$subgroup_level) |>
          unique() |>
          length()
        h <- n_levels * 51 + 60

        cards[[length(cards) + 1]] <- bslib::card(
          class = "border-0 shadow-sm mb-3",
          bslib::card_header(
            class = "d-flex align-items-center gap-2 flex-wrap",
            shiny::icon("chart-bar", class = "text-primary"),
            shiny::tags$span(class = "fw-semibold", lbl),
            shiny::tags$span(class = "ms-auto text-muted", cty),
            shiny::div(
              class = "w-100 text-muted",
              style = "font-size: 0.85rem; font-weight: normal;",
              shiny::icon("circle-info"),
              " Item: ",
              shiny::textOutput(label_id, inline = TRUE)
            )
          ),
          plotly::plotlyOutput(plot_id, height = paste0(h, "px"))
        )
      }
    }
    do.call(shiny::tagList, cards)
  })

  # Pre-register outputs for every known country × demographic variable at startup.
  # req() ensures a plot only renders when its country and var are actually selected.
  for (.bk_cty in countries) {
    for (.bk_var in names(breakdown_var_meta)) {
      local({
        local_cty <- .bk_cty
        local_var <- .bk_var
        san_cty   <- gsub("[^a-zA-Z0-9]", "_", local_cty)
        plot_id   <- paste0("bk_plot_",    san_cty, "_", local_var)
        label_id  <- paste0("bk_n_label_", san_cty, "_", local_var)

        output[[plot_id]] <- plotly::renderPlotly({
          shiny::req(input$bk_item, input$bk_dem_vars)
          active_ctys <- if (isTRUE(input$bk_all_countries)) countries else input$bk_country
          shiny::req(local_cty %in% active_ctys, local_var %in% input$bk_dem_vars)
          make_breakdown_bar(
            breakdown_data = breakdown_sum,
            country        = local_cty,
            item_id        = input$bk_item,
            subgroup_var   = local_var
          )
        })

        output[[label_id]] <- shiny::renderText({
          shiny::req(input$bk_item)
          dd <- breakdown_sum |>
            dplyr::filter(
              as.character(.data$country) == local_cty,
              as.character(.data$item_id) == input$bk_item,
              .data$subgroup_var          == local_var
            )
          q <- unique(dd$question)
          q <- q[!is.na(q) & nzchar(as.character(q))]
          if (length(q) > 0) q[1] else input$bk_item
        })
      })
    }
  }
}


shiny::shinyApp(ui, server)
