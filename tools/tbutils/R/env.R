#' Get a directory path from an environment variable
#'
#' Reads an environment variable and returns a normalized absolute path
#' using forward slashes. Optionally provides a default and checks that the
#' resolved path exists.
#'
#' @param var String. Name of the environment variable, e.g. "TB_SERIES4_DIR".
#' @param default Optional string. Fallback path to use when the variable is
#' not set or empty.
#' @param must_exist Logical. If TRUE, error when the final path does not
#' exist. If FALSE, return the normalized path regardless.
#'
#' @return Character scalar: normalized absolute path (forward slashes).
#' @examples
#' \dontrun{
#' get_env_dir("TB_SERIES4_DIR")
#' get_env_dir("TB_SERIES4_DIR", default = "B:/Export/TROMBAT/series 4/modified_data", must_exist = FALSE)
#' }
#' @export
get_env_dir <- function(var, default = NULL, must_exist = TRUE) {
  val <- Sys.getenv(var, unset = NA_character_)
  if (is.na(val) || identical(val, "")) {
    if (!is.null(default)) {
      val <- default
    } else {
      stop(sprintf("Env var %s is not set.", var), call. = FALSE)
    }
  }
  normalizePath(val, winslash = "/", mustWork = must_exist)
}