# --- Libraries ---
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(tibble)
library(stringr)
library(patchwork)
library(RColorBrewer)
library(grid)      # for unit()
library(tidyr)
library(scales)
library(gt)
library(magick)


# --- Data files ---
setwd(tbutils::get_env_dir("TROMBAT_DATA_SERIES4"))

files <- list.files(pattern = "\\.csv$")
stopifnot(length(files) > 0)

## =========================
## 🟨 NEW: Dynamic labels for Series 4 (incl. TB58–63)
## =========================

# 🟨 exact file IDs actually in the folder (robust against weird spaces)
present_ids_s4 <- tools::file_path_sans_ext(basename(files))

# 🟨 electrolyte volumes from your table (keep comma decimals for legend)
vol_s4 <- c(
  "34" = "1,72 mL",
  "35" = "1,91 mL",
  "36" = "2,1 mL",
  "49" = "2,3 mL",
  "51" = "2,5 mL",
  "53" = "2,7 mL",
  "58" = "2,3 mL",
  "59" = "2,5 mL",
  "60" = "2,7 mL",
  "61" = "3 mL",
  "62" = "3,5 mL",
  "63" = "4 mL"
)

# 🟨 the early polymer files don’t contain TB numbers — map Sep_* → TB34–36
sep_to_tb <- c("Sep_01" = "34", "Sep_02" = "35", "Sep_03" = "36")

# 🟨 build label_lookup from the actual filenames
label_lookup <- tibble::tibble(file_id = present_ids_s4) %>%
  dplyr::mutate(
    tb_num   = stringr::str_match(file_id, "MonocellTB(\\d+)")[, 2],     # TBxx
    sep_id   = stringr::str_match(file_id, "(Sep_0?\\d)")[, 2],          # Sep_01/02/03 for polymer
    tb_final = dplyr::coalesce(tb_num, sep_to_tb[sep_id]),
    label    = dplyr::if_else(
      !is.na(tb_final),
      paste0("TB", tb_final, ", ", vol_s4[tb_final]),
      NA_character_
    )
  ) %>%
  dplyr::select(file_id, label)

# 🟨 quick sanity check — if anything shows up, we didn’t match it
unmatched <- dplyr::filter(label_lookup, is.na(label))
if (nrow(unmatched)) {
  message("⚠️ Unmatched files in Series 4:\n - ",
          paste0(unmatched$file_id, collapse = "\n - "))
} else {
  message("✅ Series 4: all files received labels.")
}

# =========================
# 🟨 NEW: Legend order + 12-color palette
# =========================
# Prefer a stable TB order for color mapping (only keep those that are present)
tb_order <- c("34","35","36","49","51","53","58","59","60","61","62","63")
label_levels <- paste0("TB", tb_order, ", ", vol_s4[tb_order])
label_levels <- label_levels[label_levels %in% label_lookup$label]

# Dark2 has only 8 colors — extend with Set1 to reach up to 12, still CB-friendly
base_dark2 <- RColorBrewer::brewer.pal(8, "Dark2")
extra_set1 <- RColorBrewer::brewer.pal(8, "Set1")[c(1,2,3,4)]  # add 4 distinct hues
palette12  <- c(base_dark2, extra_set1)[seq_along(label_levels)]

# This name→color mapping is what your plots use downstream
dark2_vals <- stats::setNames(palette12, label_levels)

# --- Plot function (unchanged visuals) ---
plot_u_vs_q <- function(mode = "charge", cycle = 1,
                        xlim_range = c(0, 3.2),
                        ylim_range = c(2.5, 3.75),
                        return_data = FALSE) {
  is_charge <- tolower(mode) == "charge"
  pat_cycle <- paste0("cycle", cycle)
  
  filtered_files <- files[
    grepl(mode,  files, ignore.case = TRUE) &
      grepl(pat_cycle, files, ignore.case = TRUE) &
      (!grepl("discharge", files, ignore.case = TRUE) == is_charge)
  ]
  
  if (!length(filtered_files)) return(NULL)
  
  df <- lapply(filtered_files, function(f) {
    read_csv(f, show_col_types = FALSE) %>%
      clean_names() %>%
      mutate(file_name = tools::file_path_sans_ext(f))
  }) %>%
    bind_rows() %>%
    left_join(label_lookup, by = c("file_name" = "file_id")) %>%
    mutate(label = factor(label, levels = label_levels))
  
  if (return_data) return(df)
  
  q_col <- if (is_charge) "ah_cyc_charge_0" else "ah_cyc_discharge_0"
  
  ggplot(df, aes_string(x = q_col, y = "u_v", color = "label")) +
    geom_point(size = 0.9) +
    scale_color_manual(values = dark2_vals, drop = FALSE) +  # keep global colors
    labs(title = paste("Cycle", cycle, str_to_title(mode)),
         x = "Q [Ah]", y = "Voltage [V]", color = "Cell") +
    coord_cartesian(xlim = xlim_range, ylim = ylim_range) +
    theme_minimal(base_size = 12)
}

# ===========================
# 🟨 CHANGED: detect ALL cycles present automatically
# ===========================
cycles_found <- sort(unique(na.omit(
  as.integer(str_match(files, "cycle(\\d+)")[, 2])
)))
stopifnot(length(cycles_found) > 0)

# ===========================
# 🟨 CHANGED: global axis ranges from all detected cycles
# ===========================
all_charge <- bind_rows(lapply(cycles_found, function(c) plot_u_vs_q("charge", c, return_data = TRUE)))
all_discharge <- bind_rows(lapply(cycles_found, function(c) plot_u_vs_q("discharge", c, return_data = TRUE)))

x_range <- range(c(all_charge$ah_cyc_charge_0, all_discharge$ah_cyc_discharge_0), na.rm = TRUE)
y_range <- range(c(all_charge$u_v,             all_discharge$u_v),              na.rm = TRUE)

# ===========================
# 🟨 CHANGED: build rows dynamically for N cycles
# ===========================
row_plots <- lapply(cycles_found, function(cy) {
  pc <- plot_u_vs_q("charge", cy, x_range, y_range) + theme(legend.position = "none")
  pd <- plot_u_vs_q("discharge", cy, x_range, y_range) + theme(legend.position = "none")
  pc | pd
})

final_plot <- wrap_plots(row_plots, ncol = 1) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = paste0("U vs Q for Cycles ", min(cycles_found), "–", max(cycles_found), " (Charge & Discharge)"),
    tag_levels = "A"
  ) &
  scale_color_manual(values = dark2_vals, drop = FALSE) &
  guides(color = guide_legend(override.aes = list(size = 4))) &
  theme(
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.title      = element_text(size = 14, face = "bold"),
    legend.text       = element_text(size = 13),
    legend.key        = element_blank(),
    legend.key.size   = unit(1.2, "lines"),
    legend.spacing.x  = unit(0.8, "cm"),
    plot.title        = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.tag          = element_text(size = 14, face = "bold")
  )

# --- Show figure ---
final_plot

# --- Export paths & common settings ---
out_dir <- tbutils::get_env_dir("TROMBAT_OUTPUT_TEMP_RESULTS")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
w <- 12; h <- 14

ggsave(file.path(out_dir, "U_vs_Q_Cycles_Charge_Discharge.pdf"),
       final_plot, width = w, height = h, units = "in", dpi = 300, device = cairo_pdf)
ggsave(file.path(out_dir, "U_vs_Q_Cycles_Charge_Discharge.png"),
       final_plot, width = w, height = h, units = "in", dpi = 450, bg = "white")
ggsave(file.path(out_dir, "U_vs_Q_Cycles_Charge_Discharge.tiff"),
       final_plot, width = w, height = h, units = "in", dpi = 600, device = "tiff",
       compression = "lzw", bg = "white")

# =====================
# CE per cell & cycle (unchanged; auto-includes new cycles)
# =====================
ce_raw <- lapply(files, function(f) {
  id    <- tools::file_path_sans_ext(f)
  mode  <- ifelse(grepl("-discharge_", f, TRUE), "discharge", "charge")
  cycle <- suppressWarnings(as.integer(str_match(f, "cycle(\\d+)")[, 2]))
  if (is.na(cycle)) return(NULL)
  
  dat <- read_csv(f, col_select = c("Ah-Cyc-Charge-0", "Ah-Cyc-Discharge-0"),
                  show_col_types = FALSE) %>% clean_names()
  qvec <- if (mode == "charge") dat$ah_cyc_charge_0 else dat$ah_cyc_discharge_0
  qval <- if (all(is.na(qvec))) NA_real_ else max(qvec, na.rm = TRUE)
  
  tibble(file_name = id, mode = mode, cycle = cycle, Q = qval)
}) %>%
  bind_rows() %>%
  left_join(label_lookup, by = c("file_name" = "file_id"))

ce_table <- ce_raw %>%
  select(label, cycle, mode, Q) %>%
  pivot_wider(names_from = mode, values_from = Q, names_prefix = "Q_") %>%
  mutate(delta_Q_Ah = Q_charge - Q_discharge,
         CE = if_else(Q_charge > 0, Q_discharge / Q_charge, NA_real_)) %>%
  arrange(label, cycle)

# Pretty console table
ce_table_print <- ce_table %>%
  mutate(
    Q_charge     = round(Q_charge, 4),
    Q_discharge  = round(Q_discharge, 4),
    delta_Q_Ah   = round(delta_Q_Ah, 4),
    CE_percent   = round(100 * CE, 3)
  ) %>%
  select(label, cycle, Q_charge, Q_discharge, delta_Q_Ah, CE_percent)
print(ce_table_print)

# Save CSV
readr::write_csv(ce_table_print, file.path(out_dir, "CE_polymer_separator_table.csv"))

# CE plot
if (!exists("dark2_vals")) {
  label_levels <- sort(unique(na.omit(ce_table$label)))
  base_cols <- RColorBrewer::brewer.pal(8, "Dark2")
  dark2_vals <- stats::setNames(base_cols[seq_along(label_levels)], label_levels)
}
p_ce <- ggplot(ce_table, aes(x = factor(cycle), y = CE, color = label, group = label)) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = dark2_vals, drop = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1),
                     expand = expansion(mult = c(0.02, 0.06))) +
  labs(title = "CE Serie 4: Celgard Separator - Elektrolytmenge",
       x = "Cycle", y = "CE", color = "Cell") +
  theme_minimal(base_size = 12) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 13),
        legend.key   = element_blank(),
        legend.key.size = unit(1.2, "lines"),
        legend.spacing.x = unit(0.8, "cm"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))

ggsave(file.path(out_dir, "CE_polymer_separator.pdf"),  p_ce, width = 8, height = 5, units = "in", dpi = 300, device = cairo_pdf)
ggsave(file.path(out_dir, "CE_polymer_separator.png"),  p_ce, width = 8, height = 5, units = "in", dpi = 450, bg = "white")
ggsave(file.path(out_dir, "CE_polymer_separator.tiff"), p_ce, width = 8, height = 5, units = "in", dpi = 600, device = "tiff",
       compression = "lzw", bg = "white")

# Q_charge & Q_discharge vs cycle (same legend/colors)
qcd_long <- ce_table %>%
  select(label, cycle, Q_charge, Q_discharge) %>%
  pivot_longer(c(Q_charge, Q_discharge), names_to = "type", values_to = "Q") %>%
  filter(!is.na(Q)) %>%
  mutate(type = factor(type, levels = c("Q_charge", "Q_discharge"),
                       labels = c("Charge", "Discharge")),
         label = factor(label, levels = names(dark2_vals)))

# 🟨 CHANGED: choose a sensible wrapping for the color legend (cells)
n_cells      <- length(levels(qcd_long$label))                 # how many cell labels
cols_color   <- min(6, n_cells)                                # up to 6 columns
rows_color   <- ceiling(n_cells / cols_color)

p_q_vs_cycle <- ggplot(qcd_long,
                       aes(x = factor(cycle), y = Q,
                           color = label, linetype = type,
                           group = interaction(label, type))) +
  geom_point() +
  geom_point(size = 2) +
  scale_color_manual(values = dark2_vals, drop = FALSE) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title  = "Charge and Discharge Capacity per Cycle (Polymer separator)",
    x = "Cycle", y = "Q [Ah]",
    color = "Cell", linetype = "Mode"
  ) +
  theme_minimal(base_size = 12) +
  guides(
    # 🟨 CHANGED: put Mode (linetype) on its own 1-row legend line
    linetype = guide_legend(order = 1, nrow = 1, byrow = TRUE,
                            override.aes = list(color = "grey20", linewidth = 1.2)),
    # 🟨 CHANGED: wrap the color legend (cells) across multiple columns
    color    = guide_legend(order = 2, ncol = cols_color, byrow = TRUE,
                            override.aes = list(size = 3.6))
  ) +
  theme(
    legend.position   = "bottom",
    legend.box        = "vertical",                     # 🟨 CHANGED: stack legends
    legend.title      = element_text(size = 12, face = "bold"),  # 🟨 CHANGED: slightly smaller
    legend.text       = element_text(size = 10),                   # 🟨 CHANGED
    legend.key.size   = unit(0.9, "lines"),                        # 🟨 CHANGED
    legend.spacing.x  = unit(0.5, "cm"),
    plot.title        = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.margin       = margin(t = 6, r = 6, b = 10 + rows_color*6, l = 6, unit = "pt") # a little extra bottom room
  )

# 🟨 CHANGED: a touch taller to fit the wrapped legend comfortably
ggsave(file.path(out_dir, "Q_vs_Cycle_polymer_separator.pdf"),  p_q_vs_cycle,
       width = 8, height = 5.8, units = "in", dpi = 300, device = cairo_pdf)
ggsave(file.path(out_dir, "Q_vs_Cycle_polymer_separator.png"),  p_q_vs_cycle,
       width = 8, height = 5.8, units = "in", dpi = 450, bg = "white")
ggsave(file.path(out_dir, "Q_vs_Cycle_polymer_separator.tiff"), p_q_vs_cycle,
       width = 8, height = 5.8, units = "in", dpi = 600, device = "tiff",
       compression = "lzw", bg = "white")

# Printable CE table (PDF/PNG/TIFF)
ce_table_gt <- ce_table %>%
  transmute(label, cycle, Q_charge, Q_discharge, delta_Q_Ah, CE) %>%
  arrange(label, cycle)

ce_gt <- ce_table_gt %>%
  gt(groupname_col = "label", rowname_col = "cycle") %>%
  cols_label(
    cycle        = "Cycle",
    Q_charge     = gt::md("Q<sub>charge</sub> [Ah]"),
    Q_discharge  = gt::md("Q<sub>discharge</sub> [Ah]"),
    delta_Q_Ah   = gt::md("ΔQ [Ah]"),
    CE           = "CE"
  ) %>%
  fmt_number(columns = c(Q_charge, Q_discharge, delta_Q_Ah), decimals = 3) %>%
  fmt_percent(columns = CE, decimals = 1) %>%
  tab_header(title = md("**Coulombic Efficiency per Cycle (Polymer separator)**")) %>%
  opt_row_striping() %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(14),
    column_labels.font.weight = "bold"
  )

gt::gtsave(ce_gt, file.path(out_dir, "CE_polymer_separator_table.pdf"))
gt::gtsave(ce_gt, file.path(out_dir, "CE_polymer_separator_table.png"), vwidth = 2200, vheight = 1200, expand = 0)
magick::image_read(file.path(out_dir, "CE_polymer_separator_table.png")) |>
  magick::image_write(path = file.path(out_dir, "CE_polymer_separator_table.tiff"),
                      format = "tiff", compression = "lzw")

# =====================
# 🟨 NEW: Q_charge vs Electrolyte Volume (Series 4)
# =====================

# parse numeric volume [mL] from labels like "TB35, 1,91 mL"
parse_vol_ml <- function(lbl) {
  v <- stringr::str_match(lbl, ",\\s*([0-9]+[\\.,]?[0-9]*)\\s*mL")[, 2]
  as.numeric(gsub(",", ".", v))
}

qvol_s4 <- ce_table %>%
  dplyr::mutate(
    vol_ml   = parse_vol_ml(label),
    label    = factor(label, levels = names(dark2_vals))
  ) %>%
  dplyr::filter(!is.na(vol_ml) & !is.na(Q_charge))

# wrap the long color legend nicely
n_cells_s4  <- length(levels(qvol_s4$label))
cols_color  <- min(6, n_cells_s4)                        # up to 6 columns
rows_color  <- ceiling(n_cells_s4 / cols_color)

# ---------------------------
# Choose the smoother you want
# ---------------------------
fit_type <- "gam"   # 🟨 CHANGED: pick one of "loess", "gam", "poly2", "mm", "none"
# If you use "gam", make sure mgcv is available:
# library(mgcv)  # optional; ggplot2 will use it if installed

# 🟨 CHANGED: OPTIONAL nonlinear (Michaelis–Menten) predictions (used when fit_type == "mm")
mm_pred <- NULL
if (fit_type == "mm") {
  # Build per-cycle predictions for: Q = a * x / (b + x)
  mm_pred <- qvol_s4 |>
    dplyr::group_by(cycle) |>
    dplyr::group_modify(function(df, key) {
      df <- dplyr::filter(df, is.finite(vol_ml), is.finite(Q_charge))
      if (nrow(df) < 3) return(tibble::tibble())  # not enough points
      
      a0 <- max(df$Q_charge, na.rm = TRUE)          # sensible starts
      b0 <- stats::median(df$vol_ml, na.rm = TRUE)  # approx half-sat
      
      fit <- try(
        nls(Q_charge ~ a * vol_ml / (b + vol_ml),
            data = df, start = list(a = a0, b = b0)),
        silent = TRUE
      )
      if (inherits(fit, "try-error")) return(tibble::tibble())
      
      xseq <- tibble::tibble(vol_ml = seq(min(df$vol_ml), max(df$vol_ml), length.out = 200))
      tibble::tibble(
        cycle  = df$cycle[1],
        vol_ml = xseq$vol_ml,
        .fitted = as.numeric(predict(fit, newdata = xseq))
      )
    }) |>
    dplyr::ungroup()
}

# 🟨 CHANGED: build the smoothing layer based on fit_type
fit_layer <-
  switch(fit_type,
         loess = ggplot2::stat_smooth(method = "loess", span = 0.9, se = FALSE,
                                      color = "grey30", linewidth = 0.8),
         gam   = ggplot2::stat_smooth(method = "gam", formula = y ~ s(x, k = 5),
                                      se = FALSE, color = "grey30", linewidth = 0.8),
         poly2 = ggplot2::stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE),
                                      se = FALSE, color = "grey30", linewidth = 0.8),
         mm    = ggplot2::geom_line(
           data = mm_pred,            # predictions computed above
           ggplot2::aes(x = vol_ml, y = .fitted),
           inherit.aes = FALSE,
           color = "grey30", linewidth = 0.9
         ),
         NULL   # "none"
  )

# 🟨 CHANGED: remove the invalid `nls(...) +` from the plot; just add fit_layer
p_q_vs_vol_s4 <- ggplot(qvol_s4, aes(vol_ml, Q_charge, color = label)) +
  geom_point(size = 2) +
  fit_layer +                                  # <- the chosen smoother goes here
  facet_wrap(~ cycle, nrow = 1) +
  scale_color_manual(values = dark2_vals, drop = FALSE) +
  labs(
    title = "Charge Capacity vs Electrolyte Volume by Cycle (Polymer separator)",
    x = "Electrolyte volume [mL]", y = "Q_charge [Ah]", color = "Cell"
  ) +
  theme_minimal(base_size = 12) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.title      = element_text(size = 14, face = "bold"),
    legend.text       = element_text(size = 13),
    legend.key        = element_blank(),
    legend.key.size   = unit(1.2, "lines"),
    legend.spacing.x  = unit(0.8, "cm"),
    plot.title        = element_text(size = 15, face = "bold", hjust = 0.5)
  )

p_q_vs_vol_s4



# exports (slightly taller to fit wrapped legend)
if (exists("out_dir")) {
  ggsave(file.path(out_dir, "Q_charge_vs_Volume_polymer_separator.pdf"),
         p_q_vs_vol_s4, width = 9, height = 6, units = "in", dpi = 300, device = cairo_pdf)
  ggsave(file.path(out_dir, "Q_charge_vs_Volume_polymer_separator.png"),
         p_q_vs_vol_s4, width = 9, height = 6, units = "in", dpi = 450, bg = "white")
  ggsave(file.path(out_dir, "Q_charge_vs_Volume_polymer_separator.tiff"),
         p_q_vs_vol_s4, width = 9, height = 6, units = "in", dpi = 600,
         device = "tiff", compression = "lzw", bg = "white")
}


