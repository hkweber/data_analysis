# Shift a cycle column by a constant and rename files: "cycleN" -> "cycle{N+offset}"
# - Reads all CSVs in input_dir
# - Writes new files to output_dir (originals untouched)
# - Uses base R I/O to avoid package issues
# - You choose which column to shift: "Cyc-Count" or "abs_cycle"

shift_cycles <- function(
    input_dir,
    use_col = c("Cyc-Count", "abs_cycle"),
    offset = 3,
    output_dir = NULL,
    overwrite = FALSE,
    dry_run = TRUE,
    also_bump_other = FALSE  # if TRUE and the other column exists, bump it too
){
  use_col <- match.arg(use_col)
  other_col <- if (use_col == "Cyc-Count") "abs_cycle" else "Cyc-Count"
  
  if (!dir.exists(input_dir)) stop("Input dir not found: ", input_dir)
  files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) stop("No CSV files in: ", input_dir)
  
  if (is.null(output_dir)) {
    output_dir <- file.path(input_dir, sprintf("shifted_by_%d_%s",
                                               offset, gsub("[^A-Za-z_]", "", use_col)))
  }
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Build new filename, trying to replace the specific 'cycle<old>' token first
  bump_name <- function(path, old_n, new_n){
    base <- basename(path)
    # try exact "cycle<old_n>"
    out <- sub(sprintf("cycle%s", old_n), sprintf("cycle%s", new_n), base, fixed = TRUE)
    if (identical(out, base)) {
      # else replace first "cycle<digits>"
      out2 <- sub("cycle[0-9]+", sprintf("cycle%s", new_n), base, perl = TRUE)
      if (identical(out2, base)) {
        # else append suffix
        out <- paste0(tools::file_path_sans_ext(base),
                      sprintf("_cycle_plus_%d", new_n - old_n), ".",
                      tools::file_ext(base))
      } else {
        out <- out2
      }
    }
    out
  }
  
  results <- lapply(files, function(f){
    df <- try(utils::read.csv(f, check.names = FALSE), silent = TRUE)
    if (inherits(df, "try-error")) {
      return(data.frame(file = f, status = "READ_FAIL",
                        used_col = use_col, src_cycle = NA, new_cycle = NA,
                        new_name = NA, stringsAsFactors = FALSE))
    }
    
    if (!use_col %in% names(df)) {
      return(data.frame(file = f, status = paste0("NO_", use_col, "_COL"),
                        used_col = use_col, src_cycle = NA, new_cycle = NA,
                        new_name = NA, stringsAsFactors = FALSE))
    }
    
    # Determine representative cycle number for this file from the chosen column
    vals <- suppressWarnings(as.numeric(df[[use_col]]))
    vals <- vals[is.finite(vals)]
    if (!length(vals)) {
      return(data.frame(file = f, status = "NO_NUMERIC_VALUES",
                        used_col = use_col, src_cycle = NA, new_cycle = NA,
                        new_name = NA, stringsAsFactors = FALSE))
    }
    # Use the mode (most frequent value) as the file’s cycle tag
    src_cycle <- as.integer(names(which.max(table(vals))))
    new_cycle <- src_cycle + offset
    
    # Bump the chosen column
    v <- suppressWarnings(as.numeric(df[[use_col]]))
    v[is.finite(v)] <- v[is.finite(v)] + offset
    df[[use_col]] <- v
    
    # Optionally bump the other column too (only if it exists)
    if (also_bump_other && other_col %in% names(df)) {
      ov <- suppressWarnings(as.numeric(df[[other_col]]))
      ov[is.finite(ov)] <- ov[is.finite(ov)] + offset
      df[[other_col]] <- ov
    }
    
    # New filename
    new_base <- bump_name(f, old_n = src_cycle, new_n = new_cycle)
    out_path <- file.path(output_dir, new_base)
    
    status <- "OK_DRYRUN"
    if (!dry_run) {
      if (!overwrite && file.exists(out_path)) {
        status <- "SKIP_EXISTS"
      } else {
        utils::write.csv(df, out_path, row.names = FALSE)
        status <- "WROTE"
      }
    }
    
    data.frame(file = f, status = status,
               used_col = use_col, src_cycle = src_cycle, new_cycle = new_cycle,
               new_name = new_base, stringsAsFactors = FALSE)
  })
  
  do.call(rbind, results)
}

### how to use it
src <- "B:/Export/TROMBAT/series 4/modified_data/after_finalization_cycles"

# 1) Dry run, use Cyc-Count, add +3, preview actions
summary <- shift_cycles(src, use_col = "Cyc-Count", offset = 3, dry_run = TRUE)
print(head(summary, 10))

# 2) Actually write files (same settings)
summary <- shift_cycles(src, use_col = "Cyc-Count", offset = 3, dry_run = FALSE)

# 3) Use abs_cycle instead, subtract 1, and bump Cyc-Count as well to keep them in sync
summary <- shift_cycles(src, use_col = "abs_cycle", offset = -1,
                        dry_run = FALSE, also_bump_other = TRUE)

# 4) Custom output folder and allow overwrite, was used to create modified data for S4 (cycles 4-6)
summary <- shift_cycles(src, use_col = "Cyc-Count", offset = 3,
                        output_dir = file.path(src, "renamed_Cyc-Count_plus3"),
                        dry_run = FALSE, overwrite = TRUE)


