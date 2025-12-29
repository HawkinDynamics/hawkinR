#' Get Force-Time Data
#'
#' @description
#' Get the force-time data for a specific test by id. This includes both left, right and combined force data at 1000hz (per millisecond).
#' Calculated velocity, displacement, and power at each time interval will also be included.
#'
#' @usage
#' get_forcetime(testId, x = NULL)
#'
#' @param testId Give the unique test id of the trial you want to be called.
#'
#' @param x Optional. A `HawkinAuth` object. If NULL, the active connection is used.
#'
#' @return
#' Response will be a data frame containing the following:
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **time_s**      | *int*    | Elapsed time in seconds, starting from end of identified quiet phase |
#' | **force_right** | *int*    | Force recorded from the RIGHT platform coinciding with time point from  `time_s`, measured in Newtons (N) |
#' | **force_Left**  | *int*    | Force recorded from the LEFT platform coinciding with time point from  `time_s`, measured in Newtons (N) |
#' | **force_combined** | *int* | Sum of forces from LEFT and RIGHT, coinciding with time point from  `time_s`, measured in Newtons (N) |
#' | **velocity_m.s** | *int* | Calculated velocity of center of mass at time interval, measured in meters per second (m/s) |
#' | **displacement_m** | *int* | Calculated displacement of center of mass at time interval, measured in meters (m) |
#' | **power_w**     | *int*    | Calculated power of mass at time interval, measured in watts (W) |
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#' df_ft <- get_forcetime(testId = `stringId`)
#'
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 req_url_path_append req_error req_perform resp_status resp_body_json
#' @importFrom stringr str_replace_all
#' @importFrom lubridate as_datetime
#' @importFrom logger log_trace log_debug log_success log_error
#'
#' @export


## Get Force Time Data -----
get_forcetime <- function(testId, x = NULL) {

  # 1. ----- Set Logger -----
  logger::log_trace("hawkinR -> Run: get_forcetime")


  # 2. ----- Parameter Validation -----
  logger::log_trace("hawkinR/get_forcetime -> Validating testId parameter")
  if (!base::is.character(testId)) {
    logger::log_error("hawkinR/get_forcetime -> Incorrect testId. Must be a character string.")
    stop("Incorrect testId. Must be a character string.", call. = FALSE)
  }
  logger::log_debug("hawkinR/get_forcetime -> testId: {testId}")


  # 3. ----- Authentication (new auth manager) -----
  logger::log_trace("hawkinR/get_forcetime -> Resolving connection")
  if (is.null(x)) x <- get_active_conn()

  token_remaining <- round(as.numeric(difftime(x@expires_at, Sys.time(), units = "secs")))
  logger::log_debug("hawkinR/get_forcetime -> Token expires in {token_remaining} seconds")
  if (token_remaining < 300) {
    logger::log_info("hawkinR/get_forcetime -> Token expiring soon. Refreshing...")
    x <- authenticate(x)
    set_active_conn(x)
  }

  # 4. ----- Build Request -----
  logger::log_trace("hawkinR/get_forcetime -> Building request")
  request <- httr2::request(paste0(x@base_url, "/", x@config@org_id)) |>
    httr2::req_url_path_append("forcetime") |>
    httr2::req_url_path_append(testId)

  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug("hawkinR/get_forcetime -> {reqPath$method}: {reqPath$headers$host}{reqPath$path}")


  # 5. ----- Execute Call -----
  logger::log_trace("hawkinR/get_forcetime -> Executing API request")
  resp <-  request |>
    httr2::req_auth_bearer_token(x@access_token) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  # Response Status
  status <- httr2::resp_status(resp = resp)
  logger::log_debug("hawkinR/get_forcetime -> Response status: {status}")

  # 6. ----- Error Handling -----
  error_message <- NULL

  if (status == 401) {
    error_message <- "Error 401: Refresh Token is invalid or expired."
  } else if (status == 404) {
    logger::log_error("hawkinR/get_forcetime -> Error 404: Test not found for testId: {testId}")
    stop("Error 404: Requested Resource Not Found", call. = FALSE)
  } else if (status == 500) {
    error_message <- "Error 500: Something went wrong. Please contact support@hawkindynamics.com"
  }

  if (!base::is.null(error_message)) {
    logger::log_error("hawkinR/get_forcetime -> {error_message}")
    stop(error_message, call. = FALSE)
  }

  # 7. ----- Parse Response -----
  if (status == 200) {
    logger::log_trace("hawkinR/get_forcetime -> Parsing JSON response")
    x <- httr2::resp_body_json(
      resp = resp,
      check_type = TRUE,
      simplifyVector = TRUE
    )

    if (length(x) < 1) {
      logger::log_error("hawkinR/get_forcetime -> No test data returned")
      stop("No test data returned", call. = FALSE)
    }

    # 8. ----- Extract Metadata -----
    logger::log_trace("hawkinR/get_forcetime -> Extracting test metadata")
    testTypeID <- x[[1]][[1]]
    testName <- stringr::str_replace_all(x[[1]][[2]], "-", " - ")
    testCanonical <- x[[1]][[3]]

    athleteID <- x[[3]][[1]]
    athleteName <- x[[3]][[2]]
    athleteStatus <- if (x[[3]][[5]]) "active" else "inactive"

    timestamp <- x[[4]]
    dateTime <- lubridate::as_datetime(timestamp, tz = base::Sys.timezone())

    logger::log_debug("hawkinR/get_forcetime -> Test type: {testName} ({testCanonical})")
    logger::log_debug("hawkinR/get_forcetime -> Athlete: {athleteName} [{athleteID}]")

    # 9. ----- Build Force-Time Series -----
    logger::log_trace("hawkinR/get_forcetime -> Building force-time data frame")
    time_s <- x[[5]]
    right_force_N <- x[[6]]
    left_force_N <- x[[7]]
    combined_force_N <- x[[8]]
    velocity_m_s <- x[[9]]
    displacement_m <- x[[10]]
    power_W <- x[[11]]

    # Data Frame Output based on test type
    ft <- if (testCanonical %in% c(
      "r4fhrkPdYlLxYQxEeM78", # Multi Rebound
      "2uS5XD5kXmWgIZ5HhQ3A", # Isometric
      "5pRSUQVSJVnxijpPMck3", # Free Run
      "ubeWMPN1lJFbuQbAM97s" # Weigh In
    )) {
      logger::log_trace("hawkinR/get_forcetime -> Using force-only columns (Multi Rebound/Isometric/Free Run/Weigh In)")
      base::data.frame(time_s, right_force_N, left_force_N, combined_force_N)
    } else if (testCanonical %in% c(
      "4KlQgKmBxbOY6uKTLDFL", # TruStrength
      "umnEZPgi6zaxuw0KhUpM"
    )) {
      logger::log_trace("hawkinR/get_forcetime -> Using combined-only columns (TruStrength)")
      base::data.frame(time_s, combined_force_N)
    } else {
      logger::log_trace("hawkinR/get_forcetime -> Using full kinetic columns")
      base::data.frame(
        time_s,
        right_force_N,
        left_force_N,
        combined_force_N,
        velocity_m_s,
        displacement_m,
        power_W
      )
    }

    # 10. ----- Check For TriAxial data -----
    triaxial_cols <- 0
    if (length(x[[12]]) > 0) { ft$x_left_force_N <- x[[12]]; triaxial_cols <- triaxial_cols + 1 }
    if (length(x[[13]]) > 0) { ft$x_right_force_N <- x[[13]]; triaxial_cols <- triaxial_cols + 1 }
    if (length(x[[14]]) > 0) { ft$y_left_force_N <- x[[14]]; triaxial_cols <- triaxial_cols + 1 }
    if (length(x[[15]]) > 0) { ft$y_right_force_N <- x[[15]]; triaxial_cols <- triaxial_cols + 1 }
    if (length(x[[16]]) > 0) { ft$x_left_moments <- x[[16]]; triaxial_cols <- triaxial_cols + 1 }
    if (length(x[[17]]) > 0) { ft$x_right_moments <- x[[17]]; triaxial_cols <- triaxial_cols + 1 }
    if (length(x[[18]]) > 0) { ft$y_left_moments <- x[[18]]; triaxial_cols <- triaxial_cols + 1 }
    if (length(x[[19]]) > 0) { ft$y_right_moments <- x[[19]]; triaxial_cols <- triaxial_cols + 1 }

    if (triaxial_cols > 0) {
      logger::log_trace("hawkinR/get_forcetime -> Added {triaxial_cols} triaxial data columns")
    }

    # 11. ----- Success Log + Return -----
    logger::log_trace("hawkinR/get_forcetime -> Data frame complete: {nrow(ft)} samples, {ncol(ft)} columns")
    logger::log_success("hawkinR/get_forcetime -> Test [{testId}]: {testName} by {athleteName} ({athleteStatus}) at {dateTime}")
    return(ft)
  }
}
