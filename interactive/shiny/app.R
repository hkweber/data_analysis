# ──────────────────────────────────────────────────────────────────────────────
# File: shiny/app.R   (Shiny UI + server: multi-series, grouped cells, cycles)
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(shiny); library(shinyWidgets)
  library(dplyr); library(ggplot2); library(plotly)
  library(scales); library(tidyr); library(stringr)
})

source(here::here("interactive", "R", "utils.R"))

# ── Per-series config (dir + volumes + Sep→TB) ────────────────────────────────
series_cfg <- list(
  "Series 4" = list(
    dir = tbutils::get_env_dir("TROMBAT_DATA_SERIES4"),
    sep_to_tb = c("Sep_01"="34","Sep_02"="35","Sep_03"="36"),
    volumes = c(
      "34"="1,72 mL","35"="1,91 mL","36"="2,1 mL",
      "49"="2,3 mL","51"="2,5 mL","53"="2,7 mL",
      "58"="2,3 mL","59"="2,5 mL","60"="2,7 mL",
      "61"="3 mL","62"="3,5 mL","63"="4 mL"
    )
  ),
  "Series 5" = list(
    dir = tbutils::get_env_dir("TROMBAT_DATA_SERIES5"),
    sep_to_tb = c("Sep_01"="37","Sep_02"="38","Sep_03"="39"),
    volumes = c(
      "37"="2,21 mL","38"="2,45 mL","39"="2,695 mL",
      "50"="2,95 mL","52"="2,5 mL","54"="3,45 mL"
    )
  )
)

# Small helper used in Q vs Volume
parse_vol_ml <- function(lbl) {
  v <- stringr::str_match(lbl, ",\\s*([0-9]+[\\.,]?[0-9]*)\\s*mL")[, 2]
  as.numeric(sub(",", ".", v, fixed = TRUE))
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("TROMBAT Interactive (local)"),
  sidebarLayout(
    sidebarPanel(
      # Multi-select series
      pickerInput(
        "series", "Series",
        choices = names(series_cfg),
        selected = names(series_cfg)[1],
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selectedTextFormat` = "count > 1",
          size = 8
        )
      ),
      actionButton("refresh", "Refresh data", class = "btn-primary"),
      hr(),
      # Cells + Cycles built dynamically from selected series
      uiOutput("cell_ui"),
      uiOutput("cycle_ui"),
      conditionalPanel(
        condition = "input.tabs === 'Q vs Cycle'",
        radioButtons(
          "qc_mode", "Show:",
          choices = c("Both", "Charge", "Discharge"),
          selected = "Both", inline = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.tabs === 'Q vs Volume'",
        radioButtons(
          "qv_y", "Y axis:",
          choices  = c("Discharge" = "Q_discharge", "Charge" = "Q_charge"),
          selected = "Q_discharge",
          inline   = TRUE
        ),
        radioButtons(
          "qv_layout", "Layout:",
          choices  = c("All in one" = "single", "Facet by cycle" = "facet"),
          selected = "single",
          inline   = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.tabs === 'U–Q'",
        radioButtons(
          "uq_mode", "U–Q mode:",
          choices  = c("Both" = "both", "Charge" = "charge", "Discharge" = "discharge"),
          selected = "both",
          inline   = TRUE
        ),
        radioButtons(
          "uq_layout", "Layout:",
          choices  = c("All in one" = "single", "Facet by cycle" = "facet"),
          selected = "facet",
          inline   = TRUE
        )
      ),
      checkboxInput("show_uq", "Show U–Q tab", value = FALSE),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("CE vs Cycle", plotlyOutput("p_ce", height = "600px")),
        tabPanel("Q vs Cycle", plotlyOutput("p_q", height = "600px")),
        tabPanel("Q vs Volume", plotlyOutput("p_qv", height = "600px")),
        tabPanel("U–Q", conditionalPanel("input.show_uq", plotlyOutput("p_uq", height = "700px")))
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session){
  
  # Load / merge all selected series on refresh or series change
  data_r <- eventReactive(list(input$refresh, input$series), ignoreInit = FALSE, {
    req(length(input$series) >= 1)
    
    # Read each selected series and bind with a 'series' column
    parts <- lapply(input$series, function(sname){
      cfg <- series_cfg[[sname]]
      files <- list.files(cfg$dir, pattern = "\\.csv$", full.names = TRUE)
      validate(need(length(files) > 0, paste("No CSV files in", cfg$dir)))
      
      lookup <- build_label_lookup(files, volumes = cfg$volumes, sep_to_tb = cfg$sep_to_tb)
      uq     <- load_uq_data(files, lookup) %>% mutate(series = sname)
      ce     <- summarise_Q(uq) %>% mutate(series = sname)
      
      list(uq = uq, ce = ce, files = files, lookup = lookup, series = sname)
    })
    
    uq_all <- bind_rows(lapply(parts, `[[`, "uq"))
    ce_all <- bind_rows(lapply(parts, `[[`, "ce"))
    
    # Cells list grouped by series (for picker)
    labs_df <- ce_all %>%
      distinct(series, label) %>%
      arrange(series, label) %>%
      mutate(cell_key = paste(series, label, sep = "||"))
    
    # Build grouped choices: list("Series 4" = c("TB34, 1,72 mL" = "Series 4||TB34, 1,72 mL"), ...)
    choices_by_series <- split(setNames(labs_df$cell_key, labs_df$label), labs_df$series)
    
    # Cycle choices (union across selected series)
    cycles <- sort(unique(na.omit(ce_all$cycle)))
    
    # Colors keyed by label (keeps legend compact). OK across S4+S5 because labels differ.
    cols <- build_palette(sort(unique(na.omit(ce_all$label))))
    
    # Update dependent UI
    output$cell_ui <- renderUI({
      pickerInput(
        "cells", "Cells",
        choices = choices_by_series,
        selected = labs_df$cell_key,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          liveSearch = TRUE,
          `selectedTextFormat` = "count > 1",
          size = 10
        )
      )
    })
    output$cycle_ui <- renderUI({
      pickerInput(
        "cycles", "Cycles",
        choices = cycles,
        selected = cycles,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selectedTextFormat` = "count > 3"
        )
      )
    })
    
    list(uq = uq_all, ce = ce_all, cols = cols, cycles = cycles, labs = labs_df)
  })
  
  # cycle select helper
  cycles_sel <- reactive({
    req(data_r())
    x <- input$cycles
    if (is.null(x) || !length(x)) return(data_r()$cycles)
    x <- suppressWarnings(as.integer(x))
    x <- x[is.finite(x)]
    if (!length(x)) data_r()$cycles else sort(unique(x))
  })
  
  
  # ── CE vs Cycle ─────────────────────────────────────────────────────────────
  output$p_ce <- renderPlotly({
    d <- data_r()$ce %>%
      mutate(cell_key = paste(series, label, sep = "||")) %>%
      filter(cell_key %in% input$cells, cycle %in% input$cycles)
    
    req(nrow(d) > 0)
    
    p <- ggplot(d, aes(factor(cycle), CE, color = label, group = interaction(series, label))) +
      geom_line() + geom_point() +
      scale_color_manual(values = data_r()$cols) +
      scale_y_continuous(labels = percent) +
      labs(x = "Cycle", y = "CE", color = "Cell")
    
    if (n_distinct(d$series) > 1) {
      p <- p + facet_wrap(~ series, nrow = 1)
    }
    
    p <- p + theme_minimal()
    
    ggplotly(p, tooltip = c("series","label","cycle","y")) %>% toWebGL()
  })
  
  # ── Q vs Cycle (Charge + Discharge) ────────────────────────────────────────
  output$p_q <- renderPlotly({
    req(data_r(), length(input$cells))
    
    # Make the same key the picker emits: "Series X||Label"
    qlong <- data_r()$ce %>%
      mutate(cell_key = paste(series, label, sep = "||")) %>%
      filter(cell_key %in% input$cells,
             cycle %in% cycles_sel()) %>%
      pivot_longer(c(Q_charge, Q_discharge),
                   names_to = "type", values_to = "Q") %>%
      mutate(type = factor(type,
                           c("Q_charge","Q_discharge"),
                           c("Charge","Discharge")))
    
    # Apply radio selection: Both / Charge / Discharge
    mode_sel <- if (shiny::isTruthy(input$qc_mode)) input$qc_mode else "Both"
    if (mode_sel != "Both") {
      qlong <- dplyr::filter(qlong, type == mode_sel)
    }
    
    validate(need(nrow(qlong) > 0, "No data for current selection."))
    
    p <- ggplot(qlong, aes(x = factor(cycle), y = Q,
                           color = label, linetype = type,
                           group = interaction(series, label, type))) +
      geom_line() + geom_point() +
      scale_color_manual(values = data_r()$cols) +
      scale_linetype_manual(values = c("Charge" = "solid", "Discharge" = "dashed"),
                            drop = FALSE) +
      labs(x = "Cycle", y = "Q [Ah]", color = "Cell", linetype = "Mode") +
      theme_minimal()
    
    # Optional: facet when comparing multiple series
    if (dplyr::n_distinct(qlong$series) > 1)
      p <- p + facet_wrap(~ series, nrow = 1)
    
    ggplotly(p, tooltip = c("series","label","type","cycle","Q")) %>% toWebGL()
  })
  
#   # --- Q vs Volume (switch Y between Q_discharge / Q_charge) ---
#   output$p_qv <- renderPlotly({
#     req(data_r(), length(input$cells))
#     
#     yvar <- if (shiny::isTruthy(input$qv_y)) input$qv_y else "Q_discharge"
#     
#     d <- data_r()$ce %>%
#       dplyr::mutate(
#         cell_key = paste(series, label, sep = "||"),
#         vol_ml   = parse_vol_ml(label)
#       ) %>%
#       dplyr::filter(
#         cell_key %in% input$cells,
#         cycle %in% cycles_sel(),
#         is.finite(vol_ml),
#         is.finite(.data[[yvar]])
#       )
#     
#     req(nrow(d) > 0)
#     
#     p <- ggplot(d, aes(x = vol_ml, y = !!rlang::sym(yvar), color = label, shape = series)) +
#       geom_point() +
#       geom_smooth(
#         aes(x = vol_ml, y = !!rlang::sym(yvar)),
#         method = "gam", formula = y ~ s(x, k = 5), se = FALSE, color = "grey30",
#         inherit.aes = FALSE
#       ) +
#       scale_color_manual(values = data_r()$cols) +
#       labs(
#         x = "Electrolyte volume [mL]",
#         y = paste0(yvar, " [Ah]"),
#         color = "Cell", shape = "Series"
#       ) +
#       theme_minimal()
#     
#     # Facet if requested
#     if (identical(input$qv_layout, "facet")) {
#       p <- p + facet_wrap(~ cycle)
#     }
#     
#     ggplotly(p, tooltip = c("series", "label", "cycle", "vol_ml", yvar)) %>% toWebGL()
#   })
#   
# 
#   # ── U–Q (built on demand) ──────────────────────────────────────────────────
#   output$p_uq <- renderPlotly({
#     req(input$show_uq)
#     
#     cycs <- sort(unique(as.integer(input$cycles)))
#     req(length(cycs) > 0)
#     
#     # Build per selected series
#     parts <- lapply(input$series, function(sname){
#       cfg <- series_cfg[[sname]]
#       files <- list.files(cfg$dir, pattern = "\\.csv$", full.names = TRUE)
#       lookup <- build_label_lookup(files, volumes = cfg$volumes, sep_to_tb = cfg$sep_to_tb)
#       list(series = sname, files = files, lookup = lookup)
#     })
#     
#     # read charge + discharge for each requested cycle
#     uq <- lapply(parts, function(prt){
#       dplyr::bind_rows(lapply(cycs, function(cy){
#         dplyr::bind_rows(
#           read_uq(prt$files, prt$lookup, mode = "charge",    cycle = cy),
#           read_uq(prt$files, prt$lookup, mode = "discharge", cycle = cy)
#         ) |>
#           dplyr::mutate(series = prt$series, cycle = cy)
#       }))
#     }) |> dplyr::bind_rows()
#     
#     # keep only selected cells (picker key is "Series X||Label")
#     uq <- uq |>
#       dplyr::mutate(cell_key = paste(series, label, sep = "||")) |>
#       dplyr::filter(cell_key %in% input$cells)
#     
#     # respect U–Q mode radio
#     mode_pick <- input$uq_mode %||% "Discharge"
#     if (mode_pick == "Charge")     uq <- dplyr::filter(uq, mode == "charge")
#     if (mode_pick == "Discharge")  uq <- dplyr::filter(uq, mode == "discharge")
#     
#     # choose the correct Q column strictly by mode
#     uq <- uq |>
#       dplyr::mutate(
#         qx = dplyr::if_else(mode == "charge", ah_cyc_charge_0, ah_cyc_discharge_0)
#       ) |>
#       dplyr::filter(is.finite(qx), is.finite(u_v))
#     
#     mode_sel <- (input$uq_mode %||% "Both")
#     if (mode_sel != "Both") {
#       uq <- dplyr::filter(uq, mode == tolower(mode_sel))
#     }
#     
#     validate(need(nrow(uq) > 0, "No U–Q data for current selection."))
#     
#     p <- ggplot(uq, aes(x = qx, y = u_v, color = label,
#                         text = paste("Series:", series,
#                                      "<br>Cycle:", cycle,
#                                      "<br>Mode:", mode))) +
#       geom_point(size = 0.8, alpha = 0.9) +
#       scale_color_manual(values = data_r()$cols) +
#       labs(x = "Q [Ah]", y = "Voltage [V]", color = "Cell") +
#       theme_minimal() +
#       facet_wrap(~ cycle)
#     
#     if (identical(input$uq_layout, "facet")) {
#       p <- p + facet_wrap(~ cycle)
#       # (single layout just overlays all selected cycles in one panel)
#     }
#     
#     ggplotly(p, tooltip = c("label","text","u_v")) %>% toWebGL()
#   })
#   
# }
  output$p_qv <- renderPlotly({
    req(data_r(), length(input$cells), input$qv_layout)
    yvar <- input$qv_y %||% "Q_discharge"
    
    d <- data_r()$ce %>%
      dplyr::mutate(cell_key = paste(series, label, sep = "||"),
                    vol_ml   = parse_vol_ml(label)) %>%
      dplyr::filter(cell_key %in% input$cells,
                    cycle %in% cycles_sel(),
                    is.finite(vol_ml),
                    is.finite(.data[[yvar]]))
    
    req(nrow(d) > 0)
    
    p <- ggplot(d, aes(vol_ml, !!rlang::sym(yvar), color = label, shape = series)) +
      geom_point() +
      geom_smooth(aes(vol_ml, !!rlang::sym(yvar)),
                  method = "gam", formula = y ~ s(x, k = 5),
                  se = FALSE, color = "grey30", inherit.aes = FALSE) +
      scale_color_manual(values = data_r()$cols) +
      labs(x = "Electrolyte volume [mL]", y = paste0(yvar, " [Ah]"),
           color = "Cell", shape = "Series") +
      theme_minimal()
    
    if (identical(input$qv_layout, "facet")) {
      p <- p + facet_wrap(~ cycle)
    }
    
    ggplotly(p, tooltip = c("series","label","cycle","vol_ml", yvar)) %>% toWebGL()
  })
}  
  

shinyApp(ui, server)
