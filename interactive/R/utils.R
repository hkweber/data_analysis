
################################################################################
# File: R/utils.R
################################################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tidyr)
  library(janitor); library(ggplot2); library(patchwork)
  library(RColorBrewer); library(scales); library(gt)
})

#---------------------------- Palette helpers ----------------------------------
# Up to 16 colorblind-friendly colors (Dark2 + Set1 fallback)
build_palette <- function(labels) {
  base_dark2 <- RColorBrewer::brewer.pal(8, "Dark2")
  extra_set1 <- RColorBrewer::brewer.pal(8, "Set1")
  cols <- c(base_dark2, extra_set1)[seq_along(labels)]
  stats::setNames(cols, labels)
}

#---------------------------- Label lookup -------------------------------------
# Create (file_id -> pretty label) mapping from the *actual* filenames,
# using TB numbers if present; otherwise mapping Sep_01..03 -> TBxx.
build_label_lookup <- function(files, volumes, sep_to_tb = NULL) {
  ids <- tools::file_path_sans_ext(basename(files))
  tibble(file_id = ids) %>%
    mutate(
      tb_num   = stringr::str_match(file_id, "MonocellTB(\\d+)")[,2],
      sep_id   = stringr::str_match(file_id, "(Sep_0?\\d)")[,2],
      tb_final = dplyr::coalesce(tb_num, if (is.null(sep_to_tb)) NA_character_ else sep_to_tb[sep_id]),
      label    = dplyr::if_else(!is.na(tb_final), paste0("TB", tb_final, ", ", volumes[tb_final]), NA_character_)
    ) %>%
    select(file_id, label)
}

#---------------------------- Data loading -------------------------------------
# Read all CSVs, clean names, annotate with mode/cycle, and join labels
load_uq_data <- function(files, label_lookup) {
  stopifnot(is.character(files), length(files) > 0)
  
  out <- list()                                        # 🟨 avoid NSE/scoping issues
  idx <- 0
  
  for (fp in files) {                                  # 🟨 use explicit loop (no lapply)
    bn <- basename(fp)
    id <- tools::file_path_sans_ext(bn)
    
    mode <- if (grepl("-discharge_", bn, ignore.case = TRUE)) "discharge" else "charge"
    cyc  <- suppressWarnings(as.integer(stringr::str_match(bn, "cycle(\\d+)")[, 2]))
    if (is.na(cyc)) next
    
    # 🟨 robust read with tryCatch; skip unreadable files cleanly
    df <- tryCatch(
      {
        tmp <- readr::read_csv(fp, show_col_types = FALSE)
        janitor::clean_names(tmp)
      },
      error = function(e) {
        message("⚠️ Skipping unreadable file: ", fp, " — ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(df)) next
    
    df$file_name <- id
    df$mode      <- mode
    df$cycle     <- cyc
    
    idx <- idx + 1
    out[[idx]] <- df
  }
  
  if (idx == 0) {
    stop("No valid cycle files could be read from the provided list.")
  }
  
  dplyr::bind_rows(out) %>%
    dplyr::left_join(label_lookup, by = c("file_name" = "file_id"))
}


#---------------------------- Summaries ----------------------------------------
# One row per file: terminal/max capacity and CE per cycle
summarise_Q <- function(uq) {
  per_charge <- uq %>%
    dplyr::filter(mode == "charge") %>%
    dplyr::group_by(label, cycle) %>%
    dplyr::summarise(Q_charge = max(ah_cyc_charge_0, na.rm = TRUE), .groups = "drop")
  
  per_dis <- uq %>%
    dplyr::filter(mode == "discharge") %>%
    dplyr::group_by(label, cycle) %>%
    dplyr::summarise(Q_discharge = max(ah_cyc_discharge_0, na.rm = TRUE), .groups = "drop")
  
  dplyr::full_join(per_charge, per_dis, by = c("label","cycle")) %>%
    dplyr::mutate(
      delta_Q_Ah = Q_charge - Q_discharge,
      CE = dplyr::if_else(Q_charge > 0, Q_discharge / Q_charge, NA_real_)
    ) %>%
    dplyr::arrange(label, cycle)
}


#---------------------------- Plots --------------------------------------------
plot_uq_grid <- function(uq, palette, cycles = sort(unique(uq$cycle))) {
  is_charge <- dplyr::filter(uq, mode == "charge")
  is_dis    <- dplyr::filter(uq, mode == "discharge")
  xr <- range(c(is_charge$ah_cyc_charge_0, is_dis$ah_cyc_discharge_0), na.rm = TRUE)
  yr <- range(uq$u_v, na.rm = TRUE)
  
  one <- function(df, mode, cy) {
    qx <- if (mode == "charge") "ah_cyc_charge_0" else "ah_cyc_discharge_0"
    df %>%
      filter(mode == !!mode, cycle == !!cy) %>%
      mutate(label = factor(label, levels = names(palette))) %>%
      ggplot(aes_string(qx, "u_v", color = "label")) +
      geom_point(size = 0.9, alpha = 0.9) +
      scale_color_manual(values = palette, drop = FALSE) +
      coord_cartesian(xlim = xr, ylim = yr) +
      labs(title = paste("Cycle", cy, stringr::str_to_title(mode)), x = "Q [Ah]", y = "Voltage [V]", color = "Cell") +
      theme_minimal(base_size = 12) + theme(legend.position = "none")
  }
  
  rows <- lapply(cycles, function(cy) one(uq, "charge", cy) | one(uq, "discharge", cy))
  patchwork::wrap_plots(rows, ncol = 1) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(title = sprintf("U–Q (Cycles %s–%s, charge | discharge)", min(cycles), max(cycles)),
                               tag_levels = "A") &
    scale_color_manual(values = palette, drop = FALSE) &
    guides(color = guide_legend(override.aes = list(size = 4))) &
    theme(legend.position = "bottom",
          legend.title = element_text(face = "bold"),
          plot.title  = element_text(face = "bold", hjust = 0.5))
}

plot_ce <- function(ce_tbl, palette, title_prefix = "CE") {
  ce_tbl %>%
    mutate(label = factor(label, levels = names(palette))) %>%
    ggplot(aes(factor(cycle), CE, color = label, group = label)) +
    geom_line(linewidth = 0.6) + geom_point(size = 2) +
    scale_color_manual(values = palette, drop = FALSE) +
    scale_y_continuous(labels = scales::percent, limits = c(0,1), expand = expansion(mult = c(0.02,0.06))) +
    labs(title = paste(title_prefix, "per Cycle"), x = "Cycle", y = "CE", color = "Cell") +
    theme_minimal(base_size = 12) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme(legend.position = "bottom", legend.title = element_text(face = "bold"))
}

plot_q_vs_cycle <- function(ce_tbl, palette, title_prefix = "Capacity per Cycle") {
  ce_tbl %>%
    select(label, cycle, Q_charge, Q_discharge) %>%
    pivot_longer(c(Q_charge, Q_discharge), names_to = "type", values_to = "Q") %>%
    mutate(type = factor(type, c("Q_charge","Q_discharge"), c("Charge","Discharge")),
           label = factor(label, levels = names(palette))) %>%
    ggplot(aes(factor(cycle), Q, color = label, linetype = type, group = interaction(label, type))) +
    geom_line(linewidth = 0.7) + geom_point(size = 2) +
    scale_color_manual(values = palette, drop = FALSE) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    labs(title = title_prefix, x = "Cycle", y = "Q [Ah]", color = "Cell", linetype = "Mode") +
    theme_minimal(base_size = 12) +
    guides(linetype = guide_legend(order = 1, nrow = 1, byrow = TRUE,
                                   override.aes = list(color = "grey20", linewidth = 1.1)),
           color    = guide_legend(order = 2, ncol = pmin(6, length(palette)),
                                   override.aes = list(size = 3.4))) +
    theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_text(face = "bold"))
}

plot_q_vs_volume <- function(ce_tbl, palette, fit = c("loess","gam","poly2","mm","none")) {
  fit <- match.arg(fit)
  parse_vol <- function(lbl) {
    v <- stringr::str_match(lbl, ",\\s*([0-9]+[\\.,]?[0-9]*)\\s*mL")[,2]
    as.numeric(gsub(",", ".", v))
  }
  dat <- ce_tbl %>% mutate(vol_ml = parse_vol(label), label = factor(label, levels = names(palette))) %>%
    filter(is.finite(vol_ml), is.finite(Q_charge))
  
  # optional Michaelis–Menten predictions per cycle
  mm_pred <- NULL
  if (fit == "mm") {
    mm_pred <- dat %>% group_by(cycle) %>% group_modify(function(df, ...) {
      if (nrow(df) < 3) return(tibble())
      a0 <- max(df$Q_charge); b0 <- stats::median(df$vol_ml)
      fit_obj <- try(nls(Q_charge ~ a * vol_ml / (b + vol_ml), data = df, start = list(a = a0, b = b0)), silent = TRUE)
      if (inherits(fit_obj, "try-error")) return(tibble())
      x <- tibble(vol_ml = seq(min(df$vol_ml), max(df$vol_ml), length.out = 200))
      mutate(x, cycle = df$cycle[1], .fitted = as.numeric(predict(fit_obj, newdata = x)))
    }) %>% ungroup()
  }
  
  smoother <- switch(
    fit,
    loess = stat_smooth(method = "loess", span = .9, se = FALSE, color = "grey30"),
    gam   = {
      if (!requireNamespace("mgcv", quietly = TRUE)) message("'mgcv' not installed → falling back to loess")
      if (requireNamespace("mgcv", quietly = TRUE)) stat_smooth(method = "gam", formula = y ~ s(x, k = 5), se = FALSE, color = "grey30")
      else stat_smooth(method = "loess", span = .9, se = FALSE, color = "grey30")
    },
    poly2 = stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, color = "grey30"),
    mm    = geom_line(data = mm_pred, aes(vol_ml, .fitted), inherit.aes = FALSE, color = "grey30", linewidth = 0.9),
    NULL
  )
  
  ggplot(dat, aes(vol_ml, Q_charge, color = label)) +
    geom_point(size = 2) +
    smoother +
    facet_wrap(~ cycle, nrow = 1) +
    scale_color_manual(values = palette, drop = FALSE) +
    labs(title = "Charge Capacity vs Electrolyte Volume by Cycle", x = "Electrolyte volume [mL]", y = "Q_charge [Ah]", color = "Cell") +
    theme_minimal(base_size = 12) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme(legend.position = "bottom", legend.title = element_text(face = "bold"))
}

#---------------------------- One-stop pipeline --------------------------------
run_series <- function(series_cfg, out_dir, fit = "gam") {
  files <- list.files(series_cfg$dir, pattern = "\\.csv$", full.names = TRUE)
  stopifnot(length(files) > 0)
  
  lookup <- build_label_lookup(files, volumes = series_cfg$volumes, sep_to_tb = series_cfg$sep_to_tb)
  uq  <- load_uq_data(files, lookup)
  ce  <- summarise_Q(uq)
  
  labels  <- ce$label %>% unique() %>% sort(na.last = NA)
  palette <- build_palette(labels)
  
  p_uq   <- plot_uq_grid(uq, palette)
  p_ce   <- plot_ce(ce, palette)
  p_qcyc <- plot_q_vs_cycle(ce, palette)
  p_qvol <- plot_q_vs_volume(ce, palette, fit = fit)
  
  list(files = files, lookup = lookup, uq = uq, ce = ce, palette = palette,
       plots = list(uq = p_uq, ce = p_ce, q_cycle = p_qcyc, q_vol = p_qvol), out_dir = out_dir)
}
