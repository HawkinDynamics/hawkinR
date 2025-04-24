#' Get Force-Time Data
#'
#' @description
#' Get the force-time data for a specific test by id. This includes both left, right and combined force data at 1000hz (per millisecond).
#' Calculated velocity, displacement, and power at each time interval will also be included.
#'
#' @usage
#' get_forcetime(testId)
#'
#' @param testId Give the unique test id of the trial you want to be called.
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
#' @importFrom httr2 request req_url_path_append req_auth_bearer_token req_error req_perform resp_status resp_body_json
#' @importFrom rlang .data
#' @importFrom stringr str_replace_all
#' @importFrom lubridate as_datetime
#' @importFrom logger log_info log_formatter formatter_pander
#'
#' @export


## Get Force Time Data -----
get_forcetime <- function(testId) {

  # 1. ----- Set Logger -----
  # Log Trace
  logger::log_trace(base::paste0("hawkinR -> Run: get_forcetime"))

  # 2. ----- Parameter Validation -----

  # Retrieve access token and expiration from environment variables
  aToken <- base::Sys.getenv("accessToken")

  #-----#

  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration"))

  #-----#

  # Check for Access Token and Expiration
  if (base::is.null(aToken) ||
      token_exp <= base::as.numeric(base::Sys.time())) {
    logger::log_error("hawkinR/get_forcetime -> Access token not available or expired. Call get_access() to obtain it.")
    stop("Access token not available or expired. Call get_access() to obtain it.")
  } else {
    # Log Debug
    logger::log_debug(base::paste0("hawkinR/get_forcetime -> Temporary access token expires: ", base::as.POSIXct(token_exp)))
  }

  #-----#

  # Validate Test Id Parameter
  if (!base::is.character(testId)) {
    logger::log_error(base::paste0("hawkinR/get_forcetime -> Incorrect testId. Must be a character string."))
    base::stop("Incorrect testId. Must be a character string.")
  }

  # 2. ----- Build URL Request -----

  # Create URL Path
  urlPath <- base::paste0("forcetime/", testId)

  # Build Request
  request <- httr2::request(base::Sys.getenv("urlRegion")) %>%
    # Add URL Path
    httr2::req_url_path_append(urlPath) %>%
    # Supply Bearer Authentication
    httr2::req_auth_bearer_token(token = aToken)

  # Log Debug
  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug(base::paste0(
    "hawkinR/get_forcetime -> ",reqPath$method, ": ", reqPath$headers$host, reqPath$path))

  # Execute Call
  resp <- request %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  # Response Status
  status <- httr2::resp_status(resp = resp)

  # 4. ----- Create Response Outputs -----

  # Error Handler
  error_message <- NULL

  if (status == 401) {
    error_message <- 'Error 401: Refresh Token is invalid or expired.'
  } else if (status == 404) {
    base::stop("Error 404: Requested Resource Not Found")
  } else if (status == 500) {
    error_message <- 'Error 500: Something went wrong. Please contact support@hawkindynamics.com'
  }

  if (!base::is.null(error_message)) {
    logger::log_error(base::paste0("hawkinR/get_forcetime -> ",error_message))
    stop(error_message)
  }

  # Response Table
  if(status == 200){
    # Response GOOD - Run rest of script
    x <- httr2::resp_body_json(
      resp = resp,
      check_type = TRUE,
      simplifyVector = TRUE
    )

    # Check For Returned Test Results
    if(length(x) < 1) {
      logger::log_error(base::paste0("hawkinR/get_forcetime -> No test data returned"))
      stop("No test data returned")
    }

    # 5. ----- Sort Test Type Data -----

    # Test ID
    testTypeID <- x[[1]][[1]]

    # Test Type Name with Tags
    testName <- stringr::str_replace_all(x[[1]][[2]], "-", " - ")

    # Test Type Canonical ID
    testCanonical <- x[[1]][[3]]

    # 6. ----- Sort Athlete Data -----

    # Athlete ID
    athleteID <- x[[3]][[1]]

    # Athlete Name
    athleteName <- x[[3]][[2]]

    # Athlete Active Status
    athleteStatus <- if(x[[3]][[5]]) "active" else "inactive"

    # 7. ----- Sort Trial Info -----

    # Time stamp
    timestamp <- x[[4]]

    # Date Time
    dateTime <- lubridate::as_datetime(x[[4]], tz = base::Sys.timezone())

    # 8. ----- Create Test Data Frame -----

    # Time
    time_s <- x[[5]]

    # Right Force
    right_force_N <- x[[6]]

    # Left Force
    left_force_N <- x[[7]]

    # Combined Force
    combined_force_N <- x[[8]]

    # Velocity
    velocity_m_s <- x[[9]]

    # Displacement
    displacement_m <- x[[10]]

    # Power
    power_W <- x[[11]]

    # Data Frame Output
    ft <- if(testCanonical %in% c(
      "r4fhrkPdYlLxYQxEeM78", # Multi Rebound
      "2uS5XD5kXmWgIZ5HhQ3A", # Isometric
      "5pRSUQVSJVnxijpPMck3", # Free Run
      "ubeWMPN1lJFbuQbAM97s"  # Weigh In
    )) {
      base::data.frame(
        time_s,
        right_force_N,
        left_force_N,
        combined_force_N
      )
    } else if(testCanonical %in% c("4KlQgKmBxbOY6uKTLDFL", # TruStrength
                                   "umnEZPgi6zaxuw0KhUpM")) {
      base::data.frame(
        time_s,
        combined_force_N
      )
    } else {
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

    # 9. ----- Check For TriAxial data -----

    # X Left Force
    if (length(x[[12]]) > 0) {ft$x_left_force_N <- x[12]}

    # X Right Force
    if (length(x[[13]]) > 0) {ft$x_right_force_N <- x[13]}

    # Y Left Force
    if (length(x[[14]]) > 0) {ft$y_left_force_N <- x[14]}

    #  Y Right Force
    if (length(x[[15]]) > 0) {ft$y_right_force_N <- x[15]}

    # X Left Moments
    if (length(x[[16]]) > 0) {ft$x_left_moments <- x[16]}

    # X Right Moments
    if (length(x[[17]]) > 0) {ft$x_right_moments <- x[17]}

    # Y Left Moments
    if (length(x[[18]]) > 0) {ft$y_left_moments <- x[18]}

    # Y Right Moments
    if (length(x[[19]]) > 0) {ft$y_right_moments <- x[19]}

    # 10. ----- Returns -----

    # Output Message
    mssg <- base::paste0( "Test [",
                          testId,
                          "] is a `",
                          testName,
                          "` by athlete ",
                          athleteID,
                          " [",athleteName,
                          " (", athleteStatus,
                          ")] at ",
                          timestamp,
                          " [",
                          dateTime,
                          " ",
                          base::Sys.timezone(),
                          "]."
    )

    # Print to Log
    logger::log_success(base::paste0("hawkinR/get_forcetime -> ", mssg))

    base::return(ft)
  }
}

