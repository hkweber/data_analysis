
################################################################################
# File: run_series.R
################################################################################

# source helper functions (adjust the path to utils.R if needed)
source("R/utils.R")

# Base output folder
root_out <- tbutils::get_env_dir("TROMBAT_OUTPUT_TEMP_RESULTS")
if (!dir.exists(root_out)) dir.create(root_out, recursive = TRUE)

# Series-specific configuration (folders + volumes + Sep → TB mapping)
series_cfg <- list(
  `4` = list(
    dir = tbutils::get_env_dir("TROMBAT_DATA_SERIES4"),
    sep_to_tb = c("Sep_01"="34","Sep_02"="35","Sep_03"="36"),
    volumes = c(
      "34"="1,72 mL","35"="1,91 mL","36"="2,1 mL",
      "49"="2,3 mL","51"="2,5 mL","53"="2,7 mL",
      "58"="2,3 mL","59"="2,5 mL","60"="2,7 mL",
      "61"="3 mL","62"="3,5 mL","63"="4 mL"
    )
  ),
  `5` = list(
    dir = tbutils::get_env_dir("TROMBAT_DATA_SERIES4"),
    sep_to_tb = c("Sep_01"="37","Sep_02"="38","Sep_03"="39"),
    volumes = c(
      "37"="2,21 mL","38"="2,45 mL","39"="2,695 mL",
      "50"="2,95 mL","52"="2,5 mL","54"="3,45 mL"
    )
  )
)

# Run the pipelines (change fit to "loess"/"gam"/"poly2"/"mm"/"none")
s4 <- run_series(series_cfg[["4"]], out_dir = file.path(root_out, "series4"), fit = "gam")
s5 <- run_series(series_cfg[["5"]], out_dir = file.path(root_out, "series5"), fit = "mm")

# Ensure output folders exist
invisible(lapply(list(s4$out_dir, s5$out_dir), function(p) if (!dir.exists(p)) dir.create(p, TRUE)))

# Helper: pick PDF device that works on your machine
pdf_dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf

# --- Save a few key outputs for Series 4 ---
ggsave(file.path(s4$out_dir, "UQ_series4.pdf"),      s4$plots$uq,      width = 12, height = 14, device = pdf_dev)
ggsave(file.path(s4$out_dir, "CE_series4.png"),      s4$plots$ce,      width = 8,  height = 5,  dpi = 450, bg = "white")
ggsave(file.path(s4$out_dir, "Q_vs_Cycle_s4.tiff"),  s4$plots$q_cycle, width = 8,  height = 5.8, device = "tiff", dpi = 600, compression = "lzw", bg = "white")

# --- Save a few key outputs for Series 5 ---
ggsave(file.path(s5$out_dir, "UQ_series5.pdf"),      s5$plots$uq,      width = 12, height = 14, device = pdf_dev)
ggsave(file.path(s5$out_dir, "CE_series5.png"),      s5$plots$ce,      width = 8,  height = 5,  dpi = 450, bg = "white")
ggsave(file.path(s5$out_dir, "Q_vs_Volume_s5.tiff"), s5$plots$q_vol,   width = 9,  height = 6,  device = "tiff", dpi = 600, compression = "lzw", bg = "white")

# Done. Usage:
#   1) Put `utils.R` under R/ and `run_series.R` at project root (per layout above)
#   2) Open R and setwd(".../25 Datenauswertung")
#   3) source("run_series.R")

