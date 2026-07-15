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

  # Retrieve and validate access token (NA-safe; stops if missing/expired)
  aToken <- validate_access_token("hawkinR/get_forcetime")

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
    testTypeID <- x$testType$id

    # Test Type Name with Tags
    # Combine the test type name with any tag names (if present), joined by "-"
    tagNames <- x$testType$tags$name
    testName <- base::paste(c(x$testType$name, tagNames), collapse = "-")

    # Test Type Canonical ID
    testCanonical <- x$testType$canonicalId

    # 6. ----- Sort Athlete Data -----

    # Athlete ID
    athleteID <- x$athlete$id

    # Athlete Name
    athleteName <- x$athlete$name

    # Athlete Active Status
    athleteStatus <- if(x$athlete$active) "active" else "inactive"

    # 7. ----- Sort Trial Info -----

    # Time stamp
    timestamp <- x$timestamp

    # Date Time
    dateTime <- lubridate::as_datetime(x$timestamp, tz = base::Sys.timezone())

    # 8. ----- Create Test Data Frame -----

    # Time
    time_s <- x$`Time(s)`

    # Right Force
    right_force_N <- x$`RightForce(N)`

    # Left Force
    left_force_N <- x$`LeftForce(N)`

    # Combined Force
    combined_force_N <- x$`CombinedForce(N)`

    # Velocity
    velocity_m_s <- x$`Velocity(m/s)`

    # Displacement
    displacement_m <- x$`Displacement(m)`

    # Power
    power_W <- x$`Power(W)`
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
    if (length(x$`XLeftForce(N)`) > 0) {ft$x_left_force_N <- x$`XLeftForce(N)`}

    # X Right Force
    if (length(x$`XRightForce(N)`) > 0) {ft$x_right_force_N <- x$`XRightForce(N)`}

    # Y Left Force
    if (length(x$`YLeftForce(N)`) > 0) {ft$y_left_force_N <- x$`YLeftForce(N)`}

    #  Y Right Force
    if (length(x$`YRightForce(N)`) > 0) {ft$y_right_force_N <- x$`YRightForce(N)`}

    # X Left Moments
    if (length(x$`XLeftMoments(Nm)`) > 0) {ft$x_left_moments <- x$`XLeftMoments(Nm)`}

    # X Right Moments
    if (length(x$`XRightMoments(Nm)`) > 0) {ft$x_right_moments <- x$`XRightMoments(Nm)`}

    # Y Left Moments
    if (length(x$`YLeftMoments(Nm)`) > 0) {ft$y_left_moments <- x$`YLeftMoments(Nm)`}

    # Y Right Moments
    if (length(x$`YRightMoments(Nm)`) > 0) {ft$y_right_moments <- x$`YRightMoments(Nm)`}

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

