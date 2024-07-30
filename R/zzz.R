#--------------------#

#' Initialize logger settings
#'
#' @importFrom logger log_layout log_appender log_threshold log_info
#' @keywords internal


.onLoad <- function(libname, pkgname) {

  if (!requireNamespace("logger", quietly = TRUE)) {
    stop("The logger package is required but not installed.")
  }

  # Initialize logger without using library(logger)
  suppressPackageStartupMessages({
    logger::log_appender(logger::appender_console)
    logger::log_formatter(logger::formatter_glue_or_sprintf)
    logger::log_layout(logger::layout_glue_colors)
  })
}
