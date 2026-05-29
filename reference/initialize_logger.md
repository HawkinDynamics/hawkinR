# Logger Settings Controller

This function initializes the logger with customization parameters,
including the output destination and log thresholds for both stdout and
log file.

## Usage

``` r
initialize_logger(
 log_output = "stdout",
 log_threshold_stdout = "INFO",
 log_file = "hawkinRlog.log",
 log_threshold_file = "INFO"
 )
```

## Arguments

- log_output:

  A character string specifying the log output destination. Options are
  "stdout", "file", or "both". Default is "stdout".

- log_threshold_stdout:

  A character string specifying the log threshold for stdout. Options
  are "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL". Default is
  "INFO".

- log_file:

  A character string specifying the name of the custom log file.

- log_threshold_file:

  A character string specifying the log threshold for the log file.
  Options are "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL".
  Default is "INFO".

## Details

The function sets up the logging configuration with the specified output
destination and log thresholds. If "stdout" is chosen, logs will be
output to the console. If "file" is chosen, logs will be written to a
file named "hawkinRlog.log" by default. The `log_file` parameter can be
used to use a different file name and location. If "both" is chosen,
logs will be output to both the console and the log file.

Users can specify different log thresholds for stdout and log file.

## Examples

``` r
if (FALSE) { # \dontrun{
# Log messages as stdout with a DEBUG threshold
initialize_logger(log_output = "stdout", log_threshold_stdout = "DEBUG")

# Log to file in 'app' directory with a TRACE threshold
initialize_logger(
  log_output = "file", log_file = "app/mylog", log_threshold_file = "TRACE"
)
} # }
```
