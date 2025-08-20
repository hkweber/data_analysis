# Minimal preamble to make Series 5 block standalone
library(dplyr); library(ggplot2); library(stringr); library(janitor)
library(patchwork); library(RColorBrewer); library(readr); library(grid)



w <- 12; h <- 14
pdf_dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf

out_dir_s5 <- tbutils::get_env_dir("TROMBAT_OUTPUT_TEMP_RESULTS")  # set full path here

if (!dir.exists(out_dir_s5)) dir.create(out_dir_s5, recursive = TRUE)



# =====================
# 🟨 CHANGED: SERIES 5 PIPELINE (reuses the same visuals with Series 5 labels)
# =====================

# --- Point to Series 5 folder and list files ---


series5_dir <- tbutils::get_env_dir("TROMBAT_DATA_SERIES5")
stopifnot(dir.exists(series5_dir))
files_s5_full <- list.files(series5_dir, pattern = "\\.csv$", full.names = TRUE)
files_s5      <- basename(files_s5_full)
stopifnot(length(files_s5) > 0)

# Build label lookup from existing filenames only (robust to spacing/underscores)
present_ids_s5 <- tools::file_path_sans_ext(basename(files_s5_full))  # exact IDs used in join

# Volumes from your screenshots (TB50/52/54) and Papier Sep_01..03 → TB37/38/39
# (use comma decimal to match your legend style)
tb_vol_s5 <- c("50" = "2,95 mL", "52" = "2,5 mL", "54" = "3,45 mL")
papier_label_s5 <- c("Sep_01" = "TB37, 2,21 mL",
                     "Sep_02" = "TB38, 2,45 mL",
                     "Sep_03" = "TB39, 2,695 mL")

label_lookup_s5 <- tibble::tibble(file_id = present_ids_s5) %>%
  dplyr::mutate(
    tb_num = stringr::str_match(file_id, "MonocellTB(\\d+)")[, 2],
    sep_id = stringr::str_match(file_id, "(Sep_0?\\d)")[, 2],
    label  = dplyr::case_when(
      !is.na(tb_num) ~ paste0("TB", tb_num, ", ", tb_vol_s5[tb_num]),
      !is.na(sep_id) ~ papier_label_s5[sep_id],
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(file_id, label)

# Sanity check
unmatched_s5 <- label_lookup_s5 %>% dplyr::filter(is.na(label))
if (nrow(unmatched_s5)) {
  message("⚠️ Series 5: Some files did not match any rule:
",
          paste0(" - ", unmatched_s5$file_id, collapse = "
"))
} else {
  message("✅ Series 5: All files in the folder received a label.")
}

# Fix label order for Series 5 (ensures stable colors)
label_levels_s5 <- c("TB37, 2,21 mL", "TB38, 2,45 mL", "TB39, 2,695 mL",
                     "TB50, 2,95 mL", "TB52, 2,5 mL", "TB54, 3,45 mL")
dark2_vals_s5   <- setNames(RColorBrewer::brewer.pal(8, "Dark2")[1:6], label_levels_s5)

# --- Series 5 plotting helper (files/lookup passed explicitly) ---
plot_u_vs_q_s5 <- function(mode = "charge", cycle = 1,
                           xlim_range = c(0, 3.2), ylim_range = c(2.5, 3.75),
                           return_data = FALSE) {
  is_charge <- tolower(mode) == "charge"
  pat_cycle <- paste0("cycle", cycle)
  filtered <- files_s5[
    grepl(mode,  files_s5, ignore.case = TRUE) &
      grepl(pat_cycle, files_s5, ignore.case = TRUE) &
      (!grepl("discharge", files_s5, ignore.case = TRUE) == is_charge)
  ]
  if (!length(filtered)) {
    message("Series 5: no files for mode=", mode, ", cycle=", cycle)
    return(NULL)
  }
  df <- lapply(file.path(series5_dir, filtered), function(f) {
    readr::read_csv(f, show_col_types = FALSE) %>%
      janitor::clean_names() %>%
      dplyr::mutate(file_name = tools::file_path_sans_ext(basename(f)))
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(label_lookup_s5, by = c("file_name" = "file_id")) %>%
    dplyr::mutate(label = factor(label, levels = label_levels_s5))
  
  if (return_data) return(df)
  
  q_col <- if (is_charge) "ah_cyc_charge_0" else "ah_cyc_discharge_0"
  ggplot2::ggplot(df, ggplot2::aes_string(x = q_col, y = "u_v", color = "label")) +
    ggplot2::geom_point(size = 0.9) +
    ggplot2::labs(
      title = paste("Cycle", cycle, stringr::str_to_title(mode)),
      x = "Q [Ah]", y = "Voltage [V]", color = "Cell"
    ) +
    ggplot2::coord_cartesian(xlim = xlim_range, ylim = ylim_range) +
    ggplot2::theme_minimal(base_size = 12)
}

# --- Axis ranges from all cycles (1–3) ---
all_charge_s5 <- dplyr::bind_rows(
  plot_u_vs_q_s5("charge", 1, return_data = TRUE),
  plot_u_vs_q_s5("charge", 2, return_data = TRUE),
  plot_u_vs_q_s5("charge", 3, return_data = TRUE)
)
all_discharge_s5 <- dplyr::bind_rows(
  plot_u_vs_q_s5("discharge", 1, return_data = TRUE),
  plot_u_vs_q_s5("discharge", 2, return_data = TRUE),
  plot_u_vs_q_s5("discharge", 3, return_data = TRUE)
)

x_range_s5 <- range(c(all_charge_s5$ah_cyc_charge_0, all_discharge_s5$ah_cyc_discharge_0), na.rm = TRUE)
y_range_s5 <- range(c(all_charge_s5$u_v,              all_discharge_s5$u_v),              na.rm = TRUE)

# --- Build the six panels ---
p_charge1_s5    <- plot_u_vs_q_s5("charge",    1, x_range_s5, y_range_s5)
p_charge2_s5    <- plot_u_vs_q_s5("charge",    2, x_range_s5, y_range_s5)
p_charge3_s5    <- plot_u_vs_q_s5("charge",    3, x_range_s5, y_range_s5)
p_discharge1_s5 <- plot_u_vs_q_s5("discharge", 1, x_range_s5, y_range_s5)
p_discharge2_s5 <- plot_u_vs_q_s5("discharge", 2, x_range_s5, y_range_s5)
p_discharge3_s5 <- plot_u_vs_q_s5("discharge", 3, x_range_s5, y_range_s5)

final_plot_s5 <- (
  (p_charge1_s5 | p_discharge1_s5) /
    (p_charge2_s5 | p_discharge2_s5) /
    (p_charge3_s5 | p_discharge3_s5)
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = "U vs Q (Series 5) for Cycles 1–3 (Charge & Discharge)",
    tag_levels = "A"
  ) &
  ggplot2::scale_color_manual(values = dark2_vals_s5, drop = FALSE) &
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4))) &
  ggplot2::theme(
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.title      = ggplot2::element_text(size = 14, face = "bold"),
    legend.text       = ggplot2::element_text(size = 13),
    legend.key        = ggplot2::element_blank(),
    legend.key.size   = grid::unit(1.2, "lines"),
    legend.spacing.x  = grid::unit(0.8, "cm"),
    plot.title        = ggplot2::element_text(size = 15, face = "bold", hjust = 0.5),
    plot.tag          = ggplot2::element_text(size = 14, face = "bold")
  )

# Show
final_plot_s5

# --- Outputs for Series 5 ---

w <- 12  # inches
h <- 14  # inches


# Use same PDF device choice as above (pdf_dev already defined)

# U–Q panels
ggsave(file.path(out_dir_s5, "U_vs_Q_Cycles_Charge_Discharge_series5.pdf"),
       final_plot_s5, width = w, height = h, units = "in", dpi = 300,
       device = pdf_dev)

ggsave(file.path(out_dir_s5, "U_vs_Q_Cycles_Charge_Discharge_series5.png"),
       final_plot_s5, width = w, height = h, units = "in", dpi = 450, bg = "white")

ggsave(file.path(out_dir_s5, "U_vs_Q_Cycles_Charge_Discharge_series5.tiff"),
       final_plot_s5, width = w, height = h, units = "in", dpi = 600,
       device = "tiff", compression = "lzw", bg = "white")

# --- CE for Series 5 ---
ce_raw_s5 <- lapply(files_s5_full, function(f) {
  id <- tools::file_path_sans_ext(basename(f))
  mode  <- ifelse(grepl("-discharge_", f, ignore.case = TRUE), "discharge", "charge")
  cycle <- suppressWarnings(as.integer(stringr::str_match(f, "cycle(\\d+)")[, 2]))
  if (is.na(cycle)) return(NULL)
  
  dat <- readr::read_csv(f, col_select = c("Ah-Cyc-Charge-0", "Ah-Cyc-Discharge-0"), show_col_types = FALSE) %>%
    janitor::clean_names()
  
  qvec <- if (mode == "charge") dat$ah_cyc_charge_0 else dat$ah_cyc_discharge_0
  qval <- if (all(is.na(qvec))) NA_real_ else max(qvec, na.rm = TRUE)
  
  tibble::tibble(file_name = id, mode = mode, cycle = cycle, Q = qval)
}) %>%
  dplyr::bind_rows() %>%
  dplyr::left_join(label_lookup_s5, by = c("file_name" = "file_id"))

ce_table_s5 <- ce_raw_s5 %>%
  dplyr::select(label, cycle, mode, Q) %>%
  tidyr::pivot_wider(names_from = mode, values_from = Q, values_fill = NA, names_prefix = "Q_") %>%
  dplyr::mutate(delta_Q_Ah = Q_charge - Q_discharge,
                CE = dplyr::if_else(Q_charge > 0, Q_discharge / Q_charge, NA_real_)) %>%
  dplyr::arrange(label, cycle)

# Plot CE (Series 5)
if (!exists("dark2_vals_s5")) {
  label_levels_s5 <- sort(unique(na.omit(ce_table_s5$label)))
  base_cols <- RColorBrewer::brewer.pal(8, "Dark2")
  dark2_vals_s5 <- stats::setNames(base_cols[seq_along(label_levels_s5)], label_levels_s5)
}

p_ce_s5 <- ggplot2::ggplot(ce_table_s5, ggplot2::aes(x = factor(cycle), y = CE, color = label, group = label)) +
  ggplot2::geom_line(linewidth = 0.6, alpha = 0.9) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_color_manual(values = dark2_vals_s5, drop = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1),
                              oob = scales::squish, expand = ggplot2::expansion(mult = c(0.02, 0.06))) +
  ggplot2::labs(title = "CE Series 5: Separator/Elektrolytmenge",
                x = "Cycle", y = "CE", color = "Cell") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4))) +
  ggplot2::theme(legend.position = "bottom",
                 legend.direction = "horizontal",
                 legend.title = ggplot2::element_text(size = 14, face = "bold"),
                 legend.text  = ggplot2::element_text(size = 13),
                 legend.key   = ggplot2::element_blank(),
                 legend.key.size = grid::unit(1.2, "lines"),
                 legend.spacing.x = grid::unit(0.8, "cm"),
                 plot.title = ggplot2::element_text(size = 15, face = "bold", hjust = 0.5))

p_ce_s5

# Save CE Series 5
if (exists("out_dir_s5")) {
  ggsave(file.path(out_dir_s5, "CE_series5.pdf"),  p_ce_s5, width = 8, height = 5, units = "in", dpi = 300,
         device = pdf_dev)
  ggsave(file.path(out_dir_s5, "CE_series5.png"),  p_ce_s5, width = 8, height = 5, units = "in", dpi = 450, bg = "white")
  ggsave(file.path(out_dir_s5, "CE_series5.tiff"), p_ce_s5, width = 8, height = 5, units = "in", dpi = 600,
         device = "tiff", compression = "lzw", bg = "white")
}

# Q_charge / Q_discharge vs cycle (Series 5)
qcd_long_s5 <- ce_table_s5 %>%
  dplyr::select(label, cycle, Q_charge, Q_discharge) %>%
  tidyr::pivot_longer(cols = c(Q_charge, Q_discharge), names_to = "type", values_to = "Q") %>%
  dplyr::filter(!is.na(Q)) %>%
  dplyr::mutate(type = factor(type, levels = c("Q_charge", "Q_discharge"), labels = c("Charge", "Discharge")),
                label = factor(label, levels = names(dark2_vals_s5)))

p_q_vs_cycle_s5 <- ggplot2::ggplot(qcd_long_s5,
                                   ggplot2::aes(x = factor(cycle), y = Q, color = label, linetype = type, group = interaction(label, type))) +
  ggplot2::geom_line(linewidth = 0.7, alpha = 0.9) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_color_manual(values = dark2_vals_s5, drop = FALSE) +
  ggplot2::scale_linetype_manual(values = c("solid", "dashed")) +
  ggplot2::labs(title = "Charge and Discharge Capacity per Cycle (Series 5)", x = "Cycle", y = "Q [Ah]",
                color = "Cell", linetype = "Mode") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::guides(color    = ggplot2::guide_legend(override.aes = list(size = 4)),
                  linetype = ggplot2::guide_legend(nrow = 2, byrow = TRUE, override.aes = list(color = "grey20", linewidth = 1.2))) +
  ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal",
                 legend.title = ggplot2::element_text(size = 14, face = "bold"),
                 legend.text  = ggplot2::element_text(size = 13), legend.key = ggplot2::element_blank(),
                 legend.key.size = grid::unit(1.2, "lines"), legend.spacing.x = grid::unit(0.8, "cm"),
                 plot.title = ggplot2::element_text(size = 15, face = "bold", hjust = 0.5))

p_q_vs_cycle_s5

# Save Q vs Cycle (Series 5)
if (exists("out_dir_s5")) {
  ggsave(file.path(out_dir_s5, "Q_vs_Cycle_series5.pdf"),  p_q_vs_cycle_s5, width = 8, height = 5, units = "in", dpi = 300,
         device = pdf_dev)
  ggsave(file.path(out_dir_s5, "Q_vs_Cycle_series5.png"),  p_q_vs_cycle_s5, width = 8, height = 5, units = "in", dpi = 450, bg = "white")
  ggsave(file.path(out_dir_s5, "Q_vs_Cycle_series5.tiff"), p_q_vs_cycle_s5, width = 8, height = 5, units = "in", dpi = 600,
         device = "tiff", compression = "lzw", bg = "white")
}

# =====================
# 🟨 CHANGED: Printable CE table for Series 5 (CSV + PDF/PNG/TIFF)
# =====================

# Build a print-ready CE table (Series 5)
ce_table_s5_print <- ce_table_s5 %>%
  dplyr::transmute(
    label,
    cycle,
    Q_charge    = round(Q_charge, 4),
    Q_discharge = round(Q_discharge, 4),
    delta_Q_Ah  = round(delta_Q_Ah, 4),
    CE_percent  = round(100 * CE, 3)
  ) %>%
  dplyr::arrange(label, cycle)

# 🟨 CHANGED: Save CSV snapshot of the table
if (exists("out_dir_s5") && dir.exists(out_dir_s5)) {
  readr::write_csv(ce_table_s5_print, file.path(out_dir_s5, "CE_series5_table.csv"))
}

# 🟨 CHANGED: Pretty table export via {gt}; PNG → TIFF via {magick}
if (requireNamespace("gt", quietly = TRUE)) {
  ce_gt_s5 <- ce_table_s5 %>%
    dplyr::transmute(
      label, cycle,
      Q_charge, Q_discharge, delta_Q_Ah,
      CE  # keep as fraction for proper % formatting
    ) %>%
    dplyr::arrange(label, cycle) %>%
    gt::gt(groupname_col = "label", rowname_col = "cycle") %>%
    gt::cols_label(
      cycle        = "Cycle",
      Q_charge     = gt::md("Q<sub>charge</sub> [Ah]"),
      Q_discharge  = gt::md("Q<sub>discharge</sub> [Ah]"),
      delta_Q_Ah   = gt::md("ΔQ [Ah]"),
      CE           = "CE"
    ) %>%
    gt::fmt_number(columns = c(Q_charge, Q_discharge, delta_Q_Ah), decimals = 3) %>%
    gt::fmt_percent(columns = CE, decimals = 1) %>%
    gt::tab_header(title = gt::md("**Coulombic Efficiency per Cycle (Series 5)**")) %>%
    gt::opt_row_striping() %>%
    gt::tab_options(
      table.font.size = gt::px(12),
      heading.title.font.size = gt::px(14),
      column_labels.font.weight = "bold"
    )
  
  if (exists("out_dir_s5") && dir.exists(out_dir_s5)) {
    gt::gtsave(ce_gt_s5, file.path(out_dir_s5, "CE_series5_table.pdf"))
    gt::gtsave(ce_gt_s5, file.path(out_dir_s5, "CE_series5_table.png"), vwidth = 2200, vheight = 1200, expand = 0)
    
    if (requireNamespace("magick", quietly = TRUE)) {
      magick::image_read(file.path(out_dir_s5, "CE_series5_table.png")) |>
        magick::image_write(path = file.path(out_dir_s5, "CE_series5_table.tiff"),
                            format = "tiff", compression = "lzw")
    } else {
      message("Install 'magick' to export TIFF from gt tables. PNG and PDF have been saved.")
    }
  }
  
} else if (requireNamespace("gridExtra", quietly = TRUE)) {
  # Fallback image export using gridExtra tableGrob
  tbl_grob_s5 <- gridExtra::tableGrob(ce_table_s5_print, rows = NULL)
  if (exists("out_dir_s5") && dir.exists(out_dir_s5)) {
    ggplot2::ggsave(file.path(out_dir_s5, "CE_series5_table.png"),
                    tbl_grob_s5, width = 10, height = 7, units = "in", dpi = 300, bg = "white")
    ggplot2::ggsave(file.path(out_dir_s5, "CE_series5_table.tiff"),
                    tbl_grob_s5, width = 10, height = 7, units = "in", dpi = 600,
                    device = "tiff", compression = "lzw", bg = "white")
    # PDF fallback by wrapping grob on a blank ggplot canvas
    p_tbl_s5 <- ggplot2::ggplot() + ggplot2::annotation_custom(tbl_grob_s5) + ggplot2::theme_void()
    ggplot2::ggsave(file.path(out_dir_s5, "CE_series5_table.pdf"),
                    p_tbl_s5, width = 10, height = 7, units = "in", dpi = 300, device = pdf_dev)
  }
} else {
  message("For printable tables install either 'gt' (recommended) or 'gridExtra'. CSV export is already saved if out_dir_s5 exists.")
}



