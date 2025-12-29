#' Get Test Metrics
#'
#' @description
#' Get the metrics and ids for all the metrics in the system.
#'
#' @usage
#' get_metrics(testType = "all")
#'
#' @param testType Supply a value of type string. Must be canonical test Id, test type name, or test
#' name abbreviation.
#'
#' Names | Abbreviations: "Countermovement Jump" | "CMJ", "Squat Jump" | "SJ",
#' "Isometric Test" | "ISO", "Drop Jump" | "DJ", "Free Run" | "FREE", "CMJ Rebound" | "CMJR",
#' "Multi Rebound" | "MR", "Weigh In" | "WI", "Drop Landing" | "DL", "TS Isometric Test" | "TSISO",
#' "TS Free Run" | "TSFREE"
#'
#' @return
#' Response will be a data frame containing the tests metrics that are in the HD system. The parameter
#' `testType` can be used to filter and return only metrics of the specified type.
#'
#' The returned data frame will follow the following schema:
#'
#' | **Column Name**         | **Type** | **Description**                                  |
#' |-------------------------|----------|--------------------------------------------------|
#' | **canonicalTestTypeID** | *chr*    | Canonical Test Id                                |
#' | **testTypeName**        | *chr*    | Given Test Name                                  |
#' | **id**                  | *chr*    | camelCase Test Name                              |
#' | **label**               | *chr*    | Outward facing label or title                    |
#' | **label_unit**          | *chr*    | Outward facing label or title w/ unit of measure |
#' | **header**              | *chr*    | header of data frame output                      |
#' | **units**               | *chr*    | Unit of measure (if any)                         |
#' | **description**         | *chr*    | Description or definition of metric              |
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#' df_testsMetrics <- get_metrics(testType = "CMJ")
#'
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
#' @export

# Get Test Metrics -----
get_metrics <- function(testType = "all") {

  # 1. ----- Set Logger -----
  logger::log_trace("hawkinR -> Run: get_metrics")

  # 2. ----- Load Data -----
  logger::log_trace("hawkinR/get_metrics -> Loading MetricDictionary from package data")
  df <- get("MetricDictionary", envir = asNamespace("hawkinR"))
  logger::log_debug("hawkinR/get_metrics -> Loaded {nrow(df)} total metrics")

  # 3. ----- Build Test Type Map -----
  logger::log_trace("hawkinR/get_metrics -> Building test type lookup map")
  testTypeMap <- c(
    "Countermovement Jump" = "Countermovement Jump",
    "CMJ" = "Countermovement Jump",
    "7nNduHeM5zETPjHxvm7s" = "Countermovement Jump",
    "Squat Jump" = "Squat Jump",
    "SJ" = "Squat Jump",
    "QEG7m7DhYsD6BrcQ8pic" = "Squat Jump",
    "Isometric Test" = "Isometric Test",
    "ISO" = "Isometric Test",
    "2uS5XD5kXmWgIZ5HhQ3A" = "Isometric Test",
    "Drop Jump" = "Drop Jump",
    "DJ" = "Drop Jump",
    "gyBETpRXpdr63Ab2E0V8" = "Drop Jump",
    "Free Run" = "Free Run",
    "FREE" = "Free Run",
    "5pRSUQVSJVnxijpPMck3" = "Free Run",
    "CMJ Rebound" = "CMJ Rebound",
    "CMJR" = "CMJ Rebound",
    "pqgf2TPUOQOQs6r0HQWb" = "CMJ Rebound",
    "Multi Rebound" = "Multi Rebound",
    "MR" = "Multi Rebound",
    "r4fhrkPdYlLxYQxEeM78" = "Multi Rebound",
    "Weigh In" = "Weigh In",
    "WI" = "Weigh In",
    "ubeWMPN1lJFbuQbAM97s" = "Weigh In",
    "Drop Landing" = "Drop Landing",
    "DL" = "Drop Landing",
    "rKgI4y3ItTAzUekTUpvR" = "Drop Landing",
    "TS Isometric Test" = "TS Isometric Test",
    "TSISO" = "TS Isometric Test",
    "umnEZPgi6zaxuw0KhUpM" = "TS Isometric Test",
    "TS Free Run" = "TS Free Run",
    "TSFREE" = "TS Free Run",
    "4KlQgKmBxbOY6uKTLDFL" = "TS Free Run"
  )

  # 4. ----- Filter by Test Type -----
  if (testType != "all") {
    logger::log_trace("hawkinR/get_metrics -> Filtering for testType: '{testType}'")
    if (testType %in% names(testTypeMap)) {
      selectedTestType <- testTypeMap[[testType]]
      logger::log_debug("hawkinR/get_metrics -> Resolved '{testType}' -> '{selectedTestType}'")
      pre_filter_count <- nrow(df)
      df <- df %>% filter(.data$testTypeName == selectedTestType)
      logger::log_trace("hawkinR/get_metrics -> Filtered: {pre_filter_count} -> {nrow(df)} metrics")
    } else {
      logger::log_error("hawkinR/get_metrics -> Invalid testType: '{testType}'")
      stop("Invalid testType. Please provide a valid testType name or abbreviation.")
    }
  } else {
    logger::log_trace("hawkinR/get_metrics -> Returning all metrics (no filter)")
  }

  # 5. ----- Return Data -----
  logger::log_success("hawkinR/get_metrics -> {nrow(df)} metrics returned")
  return(df)
}

