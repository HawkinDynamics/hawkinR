#' @include auth_system.R
NULL

# 1. Class Definition -----------------------------------------------------

#' Hawkin Force-Time Data Class
#'
#' @description
#' An S7 class structured to store the raw time-series data of a test
#' alongside its metadata.
#'
#' @param test_id character. The unique ID of the test trial.
#' @param test_sampling_rate integer. The sampling rate of the test data (e.g., 1000 Hz).
#' @param testType_id character. The unique ID of the test type.
#' @param testType_name character. The human-readable name of the test type.
#' @param testType_canoncical character. The canonical ID associated with the test type.
#' @param testType_tags list. A list of tags associated with the test.
#' @param athlete_id character. The unique ID of the athlete.
#' @param athlete_name character. The name of the athlete (First Last).
#' @param athlete_teams character. A comma-separated string of team IDs.
#' @param athlete_groups character. A comma-separated string of group IDs.
#' @param athlete_active logical. Indicates if the athlete is currently active.
#' @param athlete_external list. A list of external properties associated with the athlete.
#' @param timestamp integer. The Unix timestamp of the test trial.
#' @param test_date POSIXct. The date and time the test occurred.
#' @param data data.frame. A data frame containing the raw sensor time-series data.
#' @param data_rsi any. Calculated RSI values or relevant raw metrics.
#'
#' @usage NULL
#'
#' @export
HawkinForceTime <- S7::new_class(
  "HawkinForceTime",
  properties = list(
    test_id               = S7::new_property(S7::class_character),
    test_sampling_rate    = S7::new_property(S7::class_integer),
    test_date             = S7::new_property(S7::class_POSIXct),
    testType_id           = S7::new_property(S7::class_character),
    testType_name         = S7::new_property(S7::class_character),
    testType_canoncical   = S7::new_property(S7::class_character),
    testType_tags         = S7::new_property(S7::class_list),
    athlete_id            = S7::new_property(S7::class_character),
    athlete_name          = S7::new_property(S7::class_character),
    athlete_teams         = S7::new_property(S7::class_character),
    athlete_groups        = S7::new_property(S7::class_character),
    athlete_active        = S7::new_property(S7::class_logical),
    athlete_external      = S7::new_property(S7::class_list),
    timestamp             = S7::new_property(S7::class_integer),
    data                  = S7::new_property(S7::class_data.frame),
    data_rsi              = S7::new_property(S7::class_any)
  )
)

# 2. Function Definition --------------------------------------------------

#' Get Force-Time Data
#'
#' @description
#' Retrieves the raw high-frequency sensor data (1000Hz) for a specific test ID.
#' Returns a `HawkinForceTime` object containing both the raw data frame and
#' relevant metadata.
#'
#' @usage
#' get_forcetime(testId, x = NULL)
#'
#' @param testId Give the unique test id of the trial you want to be called.
#'
#' @param x Optional. A `HawkinAuth` object. If NULL, the active connection is used.
#'
#' @return
#' A `HawkinForceTime` object.
#' \itemize{
#'   \item Use `obj@data` to access the raw data frame (Time, Force, Velocity, etc.).
#'   \item Use `obj@athlete_name` or `obj@test_type_name` to access metadata.
#' }
#'
#' @examples
#' \dontrun{
#' # Fetch data for a single test
#' ft_data <- get_forcetime(testId = "test_id_string")
#'
#' # Access the raw numbers
#' df <- ft_data@data
#'
#' # Access metadata
#' print(ft_data@athlete_name)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 request req_url_path_append req_auth_bearer_token req_error req_perform resp_status resp_body_json req_dry_run
#' @importFrom stringr str_replace_all
#' @importFrom lubridate as_datetime
#' @importFrom logger log_trace log_debug log_success log_error
#' @importFrom S7 new_class new_property class_character class_data.frame class_list class_POSIXct class_logical class_integer
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
    # We assume a fixed list structure based on the API definition
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
    # Indices based on known API response structure:
    # [[1]]: Test Type Info
    # [[3]]: Athlete Info
    # [[4]]: Timestamp

    meta_test    <- x$testType
    meta_athlete <- x$athlete
    timestamp <- x$timestamp

    # Logging Metadata
    testName <- stringr::str_replace_all(meta_test$name, "-", " - ")
    testCanonical <- meta_test$canonicalId
    athleteName <- meta_athlete$name
    athleteID <- meta_athlete$id
    logger::log_debug("hawkinR/get_forcetime -> Test type: {testName} ({testCanonical})")
    logger::log_debug("hawkinR/get_forcetime -> Athlete: {athleteName} [{athleteID}]")

    # 9. ----- Build Force-Time Series -----
    logger::log_trace("hawkinR/get_forcetime -> Building force-time data frame")

    # Map API list index -> Column Name
    data_map <- list(
      "5"  = "time_s",
      "6"  = "right_force_N",
      "7"  = "left_force_N",
      "8"  = "combined_force_N",
      "9"  = "velocity_m_s",
      "10" = "displacement_m",
      "11" = "power_W",
      # Tri-axial / Plate specific data
      "12" = "x_left_force_N",
      "13" = "x_right_force_N",
      "14" = "y_left_force_N",
      "15" = "y_right_force_N",
      "16" = "x_left_moments",
      "17" = "x_right_moments",
      "18" = "y_left_moments",
      "19" = "y_right_moments"
    )

    # Initialize a list to collect valid columns
    collected_cols <- list()

    # Extract and validate sensor data
    logger::log_trace("hawkinR/get_forcetime -> Building force-time data frame")

    for (idx_char in names(data_map)) {
      idx <- as.integer(idx_char)
      col_name <- data_map[[idx_char]]

      # Check if index exists and contains data
      if (idx <= length(x)) {
        vec <- x[[idx]]
        # Verify it's a valid numeric vector with length > 0
        if (!is.null(vec) && length(vec) > 0 && is.numeric(vec)) {
          collected_cols[[col_name]] <- vec
        }
      }
    }

    # Convert list to Data Frame
    # We assume all returned vectors are of equal length (sample count)
    ft_df <- tryCatch(
      as.data.frame(collected_cols),
      error = function(e) {
        logger::log_error("hawkinR/get_forcetime -> Failed to bind columns. Data lengths may vary.")
        stop("Data parsing error: Mismatched vector lengths from API.", call. = FALSE)
      }
    )

    # 8. ----- Construct S7 Object -----

    # Parse timestamp safely
    date_obj <- lubridate::as_datetime(timestamp, tz = Sys.timezone())

    # Parse tags safely
    tags_obj <- if (length(meta_test$tags) > 0) {
        as.list(meta_test$tags[[2]])
      } else {
        list()
      }

    # Parse RSI safely
    rsi_obj <- if (!inherits(x$rsi, "numeric")) {
      as.numeric(x$rsi)
    } else {
      NULL
    }

    # Store sampling rate
    sampling_rate <- if (testCanonical %in% c("4KlQgKmBxbOY6uKTLDFL", "umnEZPgi6zaxuw0KhUpM")) {
      1200L
    } else {
      1000L
    }

    out_object <- HawkinForceTime(
      test_id               = testId,
      test_sampling_rate    = as.integer(sampling_rate),
      timestamp             = as.integer(timestamp),
      test_date             = date_obj,
      testType_id           = as.character(meta_test[[3]]),
      testType_name         = as.character(testName),
      testType_canoncical   = as.character(testCanonical),
      testType_tags         = tags_obj,
      athlete_id            = as.character(athleteID),
      athlete_name          = as.character(athleteName),
      athlete_teams         = as.character(meta_athlete$teams),
      athlete_groups        = as.character(meta_athlete$groups),
      athlete_active        = as.logical(meta_athlete$active),
      athlete_external      = as.list(meta_athlete$external),
      data                  = ft_df,
      data_rsi              = rsi_obj
    )

    # 9. ----- Success Log -----
    logger::log_trace("hawkinR/get_forcetime -> Data frame complete: {nrow(ft_df)} samples, {ncol(ft_df)} columns")

    logger::log_success("hawkinR/get_forcetime -> Fetched test '{testId}': {out_object@testType_name} by {out_object@athlete_name} at {date_obj}")

    return(out_object)
  }
}
