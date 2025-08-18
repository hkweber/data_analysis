setwd("T:/1_TZE_Forschung/2_Energiespeicher_Pettinger/3_Projekte/20064_TromBat/25 Datenauswertung/S6")


### helper function to load cells_meta ####


#' Load selected sheets from an Excel workbook into a named list of tibbles
#'
#' @param path Path to the .xlsx file (defaults to "cells_meta2.xlsx")
#' @param include NULL to start from all sheets, or a character vector of names,
#'        a numeric vector of indices, or regex patterns (when use_regex = TRUE).
#' @param exclude NULL or a character/numeric vector like `include`, removing matches.
#' @param use_regex Logical. If TRUE, `include`/`exclude` entries are treated as regex.
#'                  (Case-insensitive.)
#' @param pick Logical. If TRUE and interactive(), opens a checkbox menu to pick sheets.
#' @param skip_empty Logical. If TRUE, drop sheets that read as 0 rows & 0 cols.
#' @param .progress Logical. Print messages while reading.
#' @param .as_tibble Logical. Keep as tibble (TRUE) or coerce to data.frame (FALSE).
#' @param ... Additional arguments passed to readxl::read_excel().
#'
#' @return Named list of data frames/tibbles, one per loaded sheet.
load_excel_sheets <- function(
    path = "cells_meta2.xlsx",
    include = NULL,
    exclude = NULL,
    use_regex = FALSE,
    pick = FALSE,
    skip_empty = TRUE,
    .progress = TRUE,
    .as_tibble = TRUE,
    ...
) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required. Please install it with install.packages('readxl').")
  }
  if (!file.exists(path)) stop("File not found: ", path)
  
  all_sheets <- readxl::excel_sheets(path)
  if (length(all_sheets) == 0) stop("No sheets found in: ", path)
  
  match_sheets <- function(all, sel, regex = FALSE) {
    if (is.null(sel)) return(character(0))
    if (is.numeric(sel)) {
      # guard against OOB
      sel <- sel[sel >= 1 & sel <= length(all)]
      return(all[sel])
    }
    sel <- as.character(sel)
    if (regex) {
      unique(unlist(lapply(sel, function(p) {
        grep(p, all, ignore.case = TRUE, value = TRUE)
      })))
    } else {
      intersect(all, sel)
    }
  }
  
  # Start from include (or all if include is NULL)
  to_load <- if (is.null(include)) all_sheets else match_sheets(all_sheets, include, use_regex)
  
  # Apply exclude
  if (!is.null(exclude)) {
    drop <- match_sheets(all_sheets, exclude, use_regex)
    to_load <- setdiff(to_load, drop)
  }
  
  # Optional interactive picker (checkbox list)
  if (isTRUE(pick) && interactive()) {
    picked <- utils::select.list(choices = to_load, multiple = TRUE, title = "Select sheets to load")
    if (!is.null(picked) && length(picked)) {
      to_load <- picked
    } else {
      if (.progress) message("No sheets selected; nothing will be loaded.")
      return(list())
    }
  }
  
  # Keep workbook order
  to_load <- all_sheets[all_sheets %in% to_load]
  
  if (length(to_load) == 0) {
    if (.progress) message("No sheets matched the selection.")
    return(list())
  }
  
  out <- setNames(vector("list", length(to_load)), to_load)
  
  for (nm in to_load) {
    if (.progress) message("Reading sheet: ", nm)
    x <- readxl::read_excel(path, sheet = nm, ...)
    if (isTRUE(skip_empty) && nrow(x) == 0 && ncol(x) == 0) {
      if (.progress) message("  -> skipped empty sheet: ", nm)
      next
    }
    if (!.as_tibble) x <- as.data.frame(x, stringsAsFactors = FALSE)
    out[[nm]] <- x
  }
  
  # Drop any NULL entries (e.g., skipped empties)
  out <- out[!vapply(out, is.null, logical(1))]
  
  out
}


### usage ###

# 1) Load everything except a few sheets by name
tabs <- load_excel_sheets(
  path = "cells_meta2.xlsx",
  exclude = c("Notes", "Summary")
)

# # 2) Load only sheets whose names start with "RNA" or "QC"
# tabs <- load_excel_sheets(
#   path = "cells_meta2.xlsx",
#   include = c("^RNA", "^QC"),
#   use_regex = TRUE
# )
# 
# # 3) Load sheets by position (e.g., 1st, 3rd, 5th)
# tabs <- load_excel_sheets(
#   path = "cells_meta2.xlsx",
#   include = c(1, 3, 5)
# )
# 
# # 4) Pick sheets interactively from a checkbox list
# tabs <- load_excel_sheets(
#   path = "cells_meta2.xlsx",
#   pick = TRUE
# )


#' Write a named list of data frames/tibbles to ONE tagged-CSV file
#'
#' Format (lines beginning with "#" are section tags):
#'   #<<FILE version="TCV1" sheets="N" generated="YYYY-MM-DDTHH:MM:SSZ">>
#'   #<<SHEET name="SheetName">>
#'   <comma-separated data with header row>  # may span multiple lines
#'   #<<END_SHEET>>
#'   ...
#'   #<<END_FILE>>
#'
#' @param tabs Named list of data frames/tibbles
#' @param file Output path, e.g., "cells_meta_tagged.csv"
#' @param max_rows_per_sheet Limit rows per sheet (Inf = all)
#' @param na NA string in output (default: empty)
#' @param encoding File encoding (default: UTF-8)
#' @return (invisible) normalized path
write_tabs_csv_tagged <- function(
    tabs,
    file,
    max_rows_per_sheet = Inf,
    na = "",
    encoding = "UTF-8"
) {
  stopifnot(is.list(tabs), length(tabs) > 0)
  if (is.null(names(tabs)) || any(names(tabs) == "")) {
    stop("`tabs` must be a *named* list (each sheet needs a name).")
  }
  
  # Convert tricky columns to safe text; keep CSV quoting for commas
  to_char_safe <- function(x) {
    if (inherits(x, "POSIXt")) {
      x <- format(x, "%Y-%m-%dT%H:%M:%S%z")
    } else if (inherits(x, "Date")) {
      x <- format(x, "%Y-%m-%d")
    } else if (is.list(x) && !is.data.frame(x)) {
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("List columns found. Please install 'jsonlite' or drop them.")
      }
      x <- vapply(
        x,
        function(el) jsonlite::toJSON(el, auto_unbox = TRUE, null = "null", na = "null"),
        character(1)
      )
    } else {
      x <- as.character(x)
    }
    # Protect newlines inside cells; commas are handled by CSV quoting
    x <- gsub("\r\n|\r|\n", "\\\\n", x, perl = TRUE)
    x
  }
  
  tabs2 <- lapply(tabs, function(df) {
    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    if (is.finite(max_rows_per_sheet)) df <- utils::head(df, max_rows_per_sheet)
    df[] <- lapply(df, to_char_safe)
    df
  })
  
  con <- file(file, open = "wt", encoding = encoding)
  on.exit(close(con), add = TRUE)
  
  stamp <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  writeLines(sprintf('#<<FILE version="TCV1" sheets="%d" generated="%s">>',
                     length(tabs2), stamp), con)
  
  for (nm in names(tabs2)) {
    writeLines(sprintf('#<<SHEET name="%s">>', nm), con)
    utils::write.table(
      tabs2[[nm]],
      file = con,
      sep = ",",
      row.names = FALSE,
      col.names = TRUE,
      quote = TRUE,          # CSV quoting handles commas and quotes
      na = na,
      qmethod = "double",
      eol = "\n"
    )
    writeLines('#<<END_SHEET>>', con)
  }
  
  writeLines('#<<END_FILE>>', con)
  invisible(normalizePath(file))
}


#' Read a tagged-CSV file back into a named list of data frames (character cols)
#' @param file Path created by write_tabs_csv_tagged()
#' @return Named list of data.frames
read_tabs_csv_tagged <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  
  start_idx <- grep('^#<<SHEET name="', lines)
  end_idx   <- grep('^#<<END_SHEET>>$', lines)
  if (length(start_idx) != length(end_idx)) stop("Malformed file: unmatched sheet tags.")
  
  get_name <- function(line) sub('^#<<SHEET name="(.*)">$','\\1', line)
  sheet_names <- vapply(lines[start_idx], get_name, character(1))
  out <- setNames(vector("list", length(start_idx)), sheet_names)
  
  unescape <- function(df) {
    df[] <- lapply(df, function(x) gsub("\\\\n", "\n", x, fixed = TRUE))
    df
  }
  
  for (i in seq_along(start_idx)) {
    s <- start_idx[i] + 1L
    e <- end_idx[i]   - 1L
    if (e < s) { out[[i]] <- data.frame(); next }
    block <- lines[s:e]
    tc <- textConnection(paste0(block, collapse = "\n"))
    on.exit(close(tc), add = TRUE)
    df <- utils::read.csv(
      tc,
      header = TRUE,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA")
    )
    out[[i]] <- unescape(df)
  }
  
  out
}


#' Quick validation/preview of a tagged-CSV; optional comparison to original
#' @param file Path to tagged .csv
#' @param original Optional original `tabs` list to compare names/dims
#' @param nrows Preview rows per sheet
#' @return (invisible) list loaded from file
check_tabs_csv <- function(file, original = NULL, nrows = 3) {
  loaded <- read_tabs_csv_tagged(file)
  
  cat("\nSheets in file:\n")
  for (nm in names(loaded)) {
    cat(sprintf("  - %s: %d x %d\n", nm, nrow(loaded[[nm]]), ncol(loaded[[nm]])))
  }
  
  if (!is.null(original)) {
    on <- names(original); ln <- names(loaded)
    cat("\nName comparison:\n")
    cat("  In original but missing in file: ",
        paste(setdiff(on, ln), collapse = ", "), "\n", sep = "")
    cat("  In file but missing in original: ",
        paste(setdiff(ln, on), collapse = ", "), "\n", sep = "")
    
    cat("\nDimension comparison:\n")
    for (nm in intersect(on, ln)) {
      o <- original[[nm]]; l <- loaded[[nm]]
      cat(sprintf("  - %s: original %d x %d | file %d x %d | dims_equal=%s\n",
                  nm, nrow(o), ncol(o), nrow(l), ncol(l), all(dim(o) == dim(l))))
    }
  }
  
  cat("\nPreview (first ", nrows, " rows):\n", sep = "")
  for (nm in names(loaded)) {
    cat("\n>> ", nm, " <<\n", sep = "")
    print(utils::head(loaded[[nm]], nrows))
  }
  
  invisible(loaded)
}


# Assuming you already have `tabs` from your Excel (named list of data frames)
# tabs <- load_excel_sheets("cells_meta2.xlsx", exclude = c("Notes", "Summary"))

# 1) Save to ONE CSV file with tagged sections
outfile <- write_tabs_csv_tagged(
  tabs,
  file = "cells_meta_tagged.csv",   # <- send me this file
  max_rows_per_sheet = Inf          # set to 100 for a lightweight preview
)

# 2) Easily check correctness (reads back and previews)
loaded <- check_tabs_csv("cells_meta_tagged.csv", original = tabs, nrows = 2)

# 3) If you (or I) need to re-import the CSV later:
tabs_from_csv <- read_tabs_csv_tagged("cells_meta_tagged.csv")

