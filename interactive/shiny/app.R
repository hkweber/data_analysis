# ──────────────────────────────────────────────────────────────────────────────
# File: shiny/app.R   (Shiny UI + server: multi-series, grouped cells, cycles)
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(shiny); library(shinyWidgets)
  library(dplyr); library(ggplot2); library(plotly)
  library(scales); library(tidyr); library(stringr); library(DT)
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
      actionButton("clear_cache", "Clear cache", class = "btn-warning", style = "margin-left: 6px;"),
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
        ),
        checkboxInput("qv_smooth", "Show smoother", value = FALSE)
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
      conditionalPanel(
        condition = "input.show_uq",
        checkboxInput("uq_precision", "High precision (no downsampling)", value = FALSE)
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
        tabPanel("U–Q", conditionalPanel("input.show_uq", plotlyOutput("p_uq", height = "700px"))),
        tabPanel("Table", DTOutput("tbl_ce", height = "620px"))
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session){
  
  # bump to force reloads when we clear cache
  cache_bump <- reactiveVal(0L)
  
  observeEvent(input$clear_cache, {
    n <- 0L
    if (exists(".tb_cache", inherits = TRUE)) {
      n <- length(ls(envir = .tb_cache))
      try(rm(list = ls(envir = .tb_cache), envir = .tb_cache), silent = TRUE)
    }
    cache_bump(cache_bump() + 1L)   # trigger re-load
    showNotification(
      sprintf("Cleared cache (%d item%s) and reloaded.", n, ifelse(n == 1, "", "s")),
      type = "message"
    )
  })
  
  # --- Load / merge all selected series on refresh or series change -----------
  data_r <- eventReactive(list(input$refresh, input$series, cache_bump()), ignoreInit = FALSE, {
    req(length(input$series) >= 1)
    
    parts <- lapply(input$series, function(sname) {
      cfg <- series_cfg[[sname]]
      load_series_data(
        series_name   = sname,
        cfg           = cfg,
        refresh_token = paste(input$refresh, cache_bump())
      )
    })
    
    uq_all <- dplyr::bind_rows(lapply(parts, `[[`, "uq"))
    ce_all <- dplyr::bind_rows(lapply(parts, `[[`, "ce"))
    
    # cells list grouped by series (for picker)
    labs_df <- ce_all %>%
      dplyr::distinct(series, label) %>%
      dplyr::arrange(series, label) %>%
      dplyr::mutate(cell_key = paste(series, label, sep = "||"))
    
    # union of cycles across selected series
    cycles <- sort(unique(stats::na.omit(ce_all$cycle)))
    
    # palette by label
    cols <- build_palette(sort(unique(stats::na.omit(ce_all$label))))
    
    # optional metadata (won't crash if missing)
    meta <- tryCatch(
      load_cells_meta(),                 # <-- no hard-coded path; uses TROMBAT_CELLS_META
      error = function(e) { message("⚠️ cells_meta not loaded: ", conditionMessage(e)); NULL }
    )
    if (!is.null(meta)) {
      showNotification(sprintf("Loaded cells_meta (%d sheets)", length(meta)), type = "message")
    }
    
    
    list(uq = uq_all, ce = ce_all, cols = cols, cycles = cycles, labs = labs_df, meta = meta)
  })
  
  # --- Dynamic UI (cells & cycles pickers) ------------------------------------
  observeEvent(data_r(), {
    labs_df <- data_r()$labs
    cycles  <- data_r()$cycles
    choices_by_series <- split(setNames(labs_df$cell_key, labs_df$label), labs_df$series)
    
    output$cell_ui <- renderUI({
      shinyWidgets::pickerInput(
        "cells", "Cells",
        choices  = choices_by_series,
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
      shinyWidgets::pickerInput(
        "cycles", "Cycles",
        choices  = cycles,
        selected = cycles,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selectedTextFormat` = "count > 3"
        )
      )
    })
  }, ignoreInit = FALSE)
  
  # --- Safe selection helpers --------------------------------------------------
  cycles_sel <- reactive({
    req(data_r())
    x <- input$cycles
    if (is.null(x) || !length(x)) data_r()$cycles else sort(unique(as.integer(x)))
  })
  cells_sel <- reactive({
    req(data_r())
    x <- input$cells
    if (is.null(x) || !length(x)) data_r()$labs$cell_key else x
  })
  
  # --- CE vs Cycle -------------------------------------------------------------
  output$p_ce <- renderPlotly({
    d <- data_r()$ce %>%
      mutate(cell_key = paste(series, label, sep = "||")) %>%
      filter(cell_key %in% cells_sel(),
             cycle    %in% cycles_sel())
    req(nrow(d) > 0)
    
    p <- ggplot(d, aes(factor(cycle), CE, color = label, group = interaction(series, label))) +
      geom_line() + geom_point() +
      scale_color_manual(values = data_r()$cols) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Cycle", y = "CE", color = "Cell") +
      theme_minimal()
    
    if (dplyr::n_distinct(d$series) > 1) p <- p + facet_wrap(~ series, nrow = 1)
    
    ggplotly(p, tooltip = c("series","label","cycle","y")) %>% toWebGL()
  })
  
  # --- Q vs Cycle --------------------------------------------------------------
  output$p_q <- renderPlotly({
    req(data_r(), length(cells_sel()))
    qlong <- data_r()$ce %>%
      mutate(cell_key = paste(series, label, sep = "||")) %>%
      filter(cell_key %in% cells_sel(),
             cycle %in% cycles_sel()) %>%
      tidyr::pivot_longer(c(Q_charge, Q_discharge), names_to = "type", values_to = "Q") %>%
      mutate(type = factor(type, c("Q_charge","Q_discharge"), c("Charge","Discharge")))
    
    mode_sel <- if (shiny::isTruthy(input$qc_mode)) input$qc_mode else "Both"
    if (mode_sel != "Both") qlong <- dplyr::filter(qlong, type == mode_sel)
    validate(need(nrow(qlong) > 0, "No data for current selection."))
    
    p <- ggplot(qlong, aes(factor(cycle), Q, color = label, linetype = type,
                           group = interaction(series, label, type))) +
      geom_line() + geom_point() +
      scale_color_manual(values = data_r()$cols) +
      scale_linetype_manual(values = c("Charge" = "solid", "Discharge" = "dashed"), drop = FALSE) +
      labs(x = "Cycle", y = "Q [Ah]", color = "Cell", linetype = "Mode") +
      theme_minimal()
    
    if (dplyr::n_distinct(qlong$series) > 1) p <- p + facet_wrap(~ series, nrow = 1)
    
    ggplotly(p, tooltip = c("series","label","type","cycle","Q")) %>% toWebGL()
  })
  
  # --- Q vs Volume -------------------------------------------------------------
  output$p_qv <- renderPlotly({
    req(data_r(), length(cells_sel()), input$qv_layout)
    yvar <- input$qv_y %||% "Q_discharge"
    
    d <- data_r()$ce %>%
      mutate(cell_key = paste(series, label, sep = "||"),
             vol_ml   = parse_vol_ml(label)) %>%
      filter(cell_key %in% cells_sel(),
             cycle %in% cycles_sel(),
             is.finite(vol_ml),
             is.finite(.data[[yvar]]))
    req(nrow(d) > 0)
    
    p <- ggplot(d, aes(vol_ml, !!rlang::sym(yvar), color = label, shape = series)) +
      geom_point() +
      scale_color_manual(values = data_r()$cols) +
      labs(x = "Electrolyte volume [mL]", y = paste0(yvar, " [Ah]"), color = "Cell", shape = "Series") +
      theme_minimal()
    
    if (isTRUE(input$qv_smooth)) {
      p <- p + geom_smooth(aes(vol_ml, !!rlang::sym(yvar)),
                           method = "gam", formula = y ~ s(x, k = 5), se = FALSE,
                           color = "grey30", inherit.aes = FALSE)
    }
    if (identical(input$qv_layout, "facet")) p <- p + facet_wrap(~ cycle)
    
    ggplotly(p, tooltip = c("series", "label", "cycle", "vol_ml", yvar)) %>% toWebGL()
  })
  
  # --- U–Q ---------------------------------------------------------------------
  output$p_uq <- renderPlotly({
    req(input$show_uq, data_r(), length(cells_sel()), input$uq_mode, input$uq_layout)
    
    d <- data_r()$uq %>%
      mutate(cell_key = paste(series, label, sep = "||")) %>%
      filter(cell_key %in% cells_sel(), cycle %in% cycles_sel())
    
    d_charge <- d %>% filter(mode == "charge")    %>% transmute(series, label, cycle, mode = "charge",    qx = ah_cyc_charge_0,    u_v)
    d_dis    <- d %>% filter(mode == "discharge") %>% transmute(series, label, cycle, mode = "discharge", qx = ah_cyc_discharge_0, u_v)
    df <- switch(input$uq_mode %||% "both",
                 charge    = d_charge,
                 discharge = d_dis,
                 both      = dplyr::bind_rows(d_charge, d_dis)) %>%
      filter(is.finite(qx), is.finite(u_v))
    req(nrow(df) > 0)
    
    max_pts <- if (isTRUE(input$uq_precision)) Inf else 30000
    df <- thin_points(df, max_points = max_pts, group_cols = c("series","label","cycle","mode"), xcol = "qx")
    
    p <- ggplot(df, aes(
      x = qx, y = u_v, color = label, shape = series,
      text = paste0(
        "Series: ", series,
        "<br>Cell: ", label,
        "<br>Cycle: ", cycle,
        "<br>Mode: ", mode,
        "<br>Q: ", signif(qx, 4), " Ah",
        "<br>U: ", signif(u_v, 4), " V"
      )
    )) +
      geom_point(size = 0.6, alpha = 0.9) +
      scale_color_manual(values = data_r()$cols) +
      labs(x = "Q [Ah]", y = "Voltage [V]", color = "Cell", shape = "Series") +
      theme_minimal()
    
    if (identical(input$uq_layout, "facet")) p <- p + facet_wrap(~ cycle)
    
    force_scattergl(ggplotly(p, tooltip = "text")) %>% toWebGL()
  })
  
  # --- Table -------------------------------------------------------------------
  output$tbl_ce <- DT::renderDT({
    req(data_r(), length(cells_sel()))
    df <- data_r()$ce %>%
      mutate(cell_key = paste(series, label, sep = "||")) %>%
      filter(cell_key %in% cells_sel(), cycle %in% cycles_sel()) %>%
      arrange(series, label, cycle) %>%
      transmute(
        Series = series,
        Cell   = label,
        Cycle  = cycle,
        Q_charge,
        Q_discharge,
        delta_Q_Ah,
        CE
      )
    
    DT::datatable(
      df,
      extensions = "Buttons",
      rownames = FALSE,
      filter   = "top",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 50,
        scrollX = TRUE
      ),
      class = "compact stripe"
    ) |>
      DT::formatRound(c("Q_charge", "Q_discharge", "delta_Q_Ah"), 3) |>
      DT::formatPercentage("CE", 1)
  })
  
}

  

shinyApp(ui, server)
