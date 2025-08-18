# ──────────────────────────────────────────────────────────────────────────────
# File: shiny/app.R   (Shiny UI + server for Series 4/5)
# Path: T:/1_TZE_Forschung/2_Energiespeicher_Pettinger/3_Projekte/20064_TromBat/25 Datenauswertung/interactive/shiny/app.R
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(ggplot2); library(plotly)
  library(scales); library(tidyr); library(stringr)
})

source("../R/utils.R")   # 🟨 CHANGED: use utils.R

# 🟨 NEW: per-series config (dir + volumes + Sep→TB)
series_cfg <- list(
  "Series 4" = list(
    dir = "B:/Export/TROMBAT/series 4/modified_data",
    sep_to_tb = c("Sep_01"="34","Sep_02"="35","Sep_03"="36"),
    volumes = c(
      "34"="1,72 mL","35"="1,91 mL","36"="2,1 mL",
      "49"="2,3 mL","51"="2,5 mL","53"="2,7 mL",
      "58"="2,3 mL","59"="2,5 mL","60"="2,7 mL",
      "61"="3 mL","62"="3,5 mL","63"="4 mL"
    )
  ),
  "Series 5" = list(
    dir = "B:/Export/TROMBAT/series 5/modified_data",
    sep_to_tb = c("Sep_01"="37","Sep_02"="38","Sep_03"="39"),
    volumes = c(
      "37"="2,21 mL","38"="2,45 mL","39"="2,695 mL",
      "50"="2,95 mL","52"="2,5 mL","54"="3,45 mL"
    )
  )
)

# 🟨 NEW: tiny helper for volume parsing used in Q vs Volume
parse_vol_ml <- function(lbl) {
  v <- stringr::str_match(lbl, ",\\s*([0-9]+[\\.,]?[0-9]*)\\s*mL")[, 2]
  as.numeric(sub(",", ".", v, fixed = TRUE))
}

ui <- fluidPage(
  titlePanel("TROMBAT Interactive (local)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("series", "Series", choices = names(series_cfg)),
      actionButton("refresh", "Refresh data", class = "btn-primary"),
      hr(),
      uiOutput("cell_ui"),
      sliderInput("cyc", "Cycles", min = 1, max = 3, value = c(1,3), step = 1),
      checkboxInput("show_uq", "Show U–Q tab", value = FALSE),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("CE vs Cycle", plotlyOutput("p_ce", height = "600px")),
        tabPanel("Q vs Cycle", plotlyOutput("p_q",  height = "600px")),
        tabPanel("Q vs Volume", plotlyOutput("p_qv", height = "600px")),
        tabPanel("U–Q", conditionalPanel("input.show_uq", plotlyOutput("p_uq", height = "700px")))
      )
    )
  )
)

server <- function(input, output, session){
  
  data_r <- eventReactive(input$refresh, ignoreInit = FALSE, {
    cfg   <- series_cfg[[ input$series ]]                                 # 🟨 CHANGED
    files <- list.files(cfg$dir, pattern = "\\.csv$", full.names = TRUE)   # 🟨 CHANGED
    validate(need(length(files) > 0, paste("No CSV files in", cfg$dir)))
    
    lookup <- build_label_lookup(files,
                                 volumes  = cfg$volumes,                   # 🟨 CHANGED
                                 sep_to_tb = cfg$sep_to_tb)                # 🟨 CHANGED
    
    uq  <- load_uq_data(files, lookup)                                     # 🟨 CHANGED (read once)
    ce  <- summarise_Q(uq)                                                 # 🟨 CHANGED
    cols <- build_palette(sort(unique(na.omit(ce$label))))                 # 🟨 CHANGED
    
    cycles <- sort(unique(na.omit(ce$cycle)))
    updateSliderInput(session, "cyc",
                      min = min(cycles), max = max(cycles),
                      value = c(min(cycles), max(cycles)))
    
    list(files = files, lookup = lookup, uq = uq, ce = ce, cols = cols, cycles = cycles)
  })
  
  output$cell_ui <- renderUI({
    labs <- sort(unique(na.omit(data_r()$ce$label)))
    checkboxGroupInput("cells", "Cells", choices = labs, selected = labs)
  })
  
  output$p_ce <- renderPlotly({
    d <- data_r()$ce %>%
      filter(label %in% input$cells,
             cycle >= input$cyc[1], cycle <= input$cyc[2])
    p <- ggplot(d, aes(factor(cycle), CE, color = label, group = label)) +
      geom_line() + geom_point() +
      scale_color_manual(values = data_r()$cols) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Cycle", y = "CE", color = "Cell") +
      theme_minimal()
    ggplotly(p, tooltip = c("label","cycle","y")) %>% toWebGL()
  })
  
  output$p_q <- renderPlotly({
    qlong <- data_r()$ce %>%
      filter(label %in% input$cells,
             cycle >= input$cyc[1], cycle <= input$cyc[2]) %>%
      pivot_longer(c(Q_charge, Q_discharge), names_to = "type", values_to = "Q") %>%
      mutate(type = factor(type, c("Q_charge","Q_discharge"), c("Charge","Discharge")))
    p <- ggplot(qlong, aes(factor(cycle), Q, color = label, linetype = type,
                           group = interaction(label, type))) +
      geom_line() + geom_point() +
      scale_color_manual(values = data_r()$cols) +
      scale_linetype_manual(values = c("solid","dashed")) +
      labs(x = "Cycle", y = "Q [Ah]", color = "Cell", linetype = "Mode") +
      theme_minimal()
    ggplotly(p, tooltip = c("label","type","cycle","Q")) %>% toWebGL()
  })
  
  output$p_qv <- renderPlotly({
    d <- data_r()$ce %>%
      filter(label %in% input$cells,
             cycle >= input$cyc[1], cycle <= input$cyc[2]) %>%
      mutate(vol_ml = parse_vol_ml(label)) %>%                        # 🟨 CHANGED
      filter(is.finite(vol_ml))
    p <- ggplot(d, aes(vol_ml, Q_charge, color = label)) +
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, k = 5), se = FALSE, color = "grey30") +
      scale_color_manual(values = data_r()$cols) +
      labs(x = "Electrolyte volume [mL]", y = "Q_charge [Ah]", color = "Cell") +
      theme_minimal()
    ggplotly(p, tooltip = c("label","vol_ml","Q_charge")) %>% toWebGL()
  })
  
  output$p_uq <- renderPlotly({
    req(input$show_uq)
    
    # Build U–Q only when requested (can be heavy)
    cycs <- seq(input$cyc[1], input$cyc[2])
    uq <- lapply(cycs, function(cy){
      rbind(
        read_uq(data_r()$files, data_r()$lookup, mode = "charge",    cycle = cy),
        read_uq(data_r()$files, data_r()$lookup, mode = "discharge", cycle = cy)
      ) |>
        dplyr::mutate(cycle = cy)
    }) |> dplyr::bind_rows()
    
    # keep selected cells
    uq <- uq |> dplyr::filter(label %in% input$cells)
    
    # ----- GUARD goes here -----
    uq <- uq |>
      dplyr::mutate(
        mode = dplyr::case_when(
          !is.na(ah_cyc_charge_0)    ~ "charge",
          !is.na(ah_cyc_discharge_0) ~ "discharge",
          TRUE ~ NA_character_
        ),
        qx = dplyr::case_when(
          mode == "charge"    ~ ah_cyc_charge_0,
          mode == "discharge" ~ ah_cyc_discharge_0,
          TRUE ~ NA_real_
        )
      ) |>
      dplyr::filter(is.finite(qx), is.finite(u_v))
    # ---------------------------
    
    p <- ggplot(uq, aes(x = qx, y = u_v, color = label,
                        text = paste("Cycle:", cycle, "Mode:", mode))) +
      geom_point(size = 0.6) +
      scale_color_manual(values = data_r()$cols) +
      labs(x = "Q [Ah]", y = "Voltage [V]", color = "Cell") +
      theme_minimal() +
      facet_wrap(~ cycle)
    
    ggplotly(p, tooltip = c("label","text","u_v")) %>% toWebGL()
  })
  
}

shinyApp(ui, server)
