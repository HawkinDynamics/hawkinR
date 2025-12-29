#' Logger Settings Controller
#'
#' This function initializes the logger with customization parameters, including the
#' output destination and log thresholds for both stdout and log file.
#'
#' @usage
#' initialize_logger(
#'  log_output = "stdout",
#'  log_threshold_stdout = "INFO",
#'  log_file = "hawkinRlog.log",
#'  log_threshold_file = "INFO"
#'  )
#'
#' @param log_output A character string specifying the log output destination.
#' Options are "stdout", "file", or "both". Default is "stdout".
#'
#' @param log_threshold_stdout A character string specifying the log threshold for stdout.
#' Options are "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL". Default is "INFO".
#'
#' @param log_threshold_file A character string specifying the log threshold for the
#' log file. Options are "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL". Default is
#' "INFO".
#'
#' @param log_file A character string specifying the name of the custom log file.
#'
#' @details
#' The function sets up the logging configuration with the specified output destination
#' and log thresholds. If "stdout" is chosen, logs will be output to the console.
#' If "file" is chosen, logs will be written to a file named "hawkinRlog.log" by default.
#' The `log_file` parameter can be used to use a different file name and location.
#' If "both" is chosen, logs will be output to both the console and the log file.
#'
#' Users can specify different log thresholds for stdout and log file.
#'
#' @examples
#' \dontrun{
#' # Log messages as stdout with a DEBUG threshold
#' initialize_logger(log_output = "stdout", log_threshold_stdout = "DEBUG")
#'
#' # Log to file in 'app' directory with a TRACE threshold
#' initialize_logger(
#'   log_output = "file", log_file = "app/mylog", log_threshold_file = "TRACE"
#' )
#' }
#'
#' @importFrom logger log_layout log_threshold log_appender log_info
#' @importFrom logger appender_file appender_tee
#'
#' @export


# Logger Setting Initialize -----

initialize_logger <-
  function(
    log_output = "stdout",
    log_threshold_stdout = "INFO",
    log_file = "hawkinRlog.log",
    log_threshold_file = "INFO"
    ) {

  # Set the log thresholds
  log_threshold_stdout <-
    base::match.arg(
      log_threshold_stdout, c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
    )
  log_threshold_file <-
    base::match.arg(
      log_threshold_file, c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
    )

  # Define log appenders based on user choice
  log_appender_stdout <- logger::appender_console
  log_appender_file <- logger::appender_file(file = log_file)

  # Set the log appenders and thresholds
  if (log_output == "stdout") {
    # STDOUT Settings
    logger::log_threshold(log_threshold_stdout)
    logger::log_appender(log_appender_stdout)
    logger::log_layout(logger::layout_glue_colors)
    # Log the initialization
    logger::log_info(
      "Logger initialized with {log_output} output at {log_threshold_stdout} threshold"
    )
  } else if (log_output == "file") {
    # File Out Settings
    logger::log_threshold(log_threshold_file)
    logger::log_appender(log_appender_file)
    logger::log_layout(logger::layout_glue)
    # Log the initialization
    logger::log_info(
      "Logger initialized with {log_output} output at {log_threshold_file} threshold"
    )
  } else if (log_output == "both") {

    # Create separate logger configurations for stdout and file
    logger::log_appender(log_appender_stdout, index = 1)
    logger::log_threshold(log_threshold_stdout, index = 1)
    logger::log_layout(logger::layout_glue_colors, index = 1)

    logger::log_appender(log_appender_file, index = 2)
    logger::log_threshold(log_threshold_file, index = 2)
    logger::log_layout(logger::layout_glue, index = 2)

    # Log the initialization
    logger::log_info(
      "Logger initialized with both stdout and file outputs, with stdout threshold:
      {log_threshold_stdout}, and file threshold: {log_threshold_file}"
    )
  }
}

