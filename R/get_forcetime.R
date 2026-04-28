#' @include auth_system.R
NULL

# 1. Class Definition -----------------------------------------------------
# (HawkinForceTime class definition remains identical)
HawkinForceTime <- S7::new_class("HawkinForceTime",
                                 properties = list(
                                   test_id               = S7::class_character,
                                   test_sampling_rate    = S7::class_integer,
                                   testType_id           = S7::class_character,
                                   testType_name         = S7::class_character,
                                   testType_canonical   = S7::class_character,
                                   testType_tags         = S7::class_list,
                                   athlete_id            = S7::class_character,
                                   athlete_name          = S7::class_character,
                                   athlete_teams         = S7::class_character,
                                   athlete_groups        = S7::class_character,
                                   athlete_active        = S7::class_logical,
                                   athlete_external      = S7::class_list,
                                   timestamp             = S7::class_integer,
                                   test_date             = S7::class_POSIXct,
                                   data                  = S7::class_data.frame,
                                   data_rsi              = S7::class_any
                                 )
)

# 2. Function Definition --------------------------------------------------

#' Get Force-Time Data
#'
#' Retrieves the raw force-time data for a specific test trial ID.
#'
#' @param testId character. The unique identifier for the test trial.
#' @param ... Optional arguments.
#' \itemize{
#'   \item `profile`: A `HawkinAuth` object. If not provided, the active connection is used.
#' }
#'
#' @return A `HawkinForceTime` object.
#' @importFrom httr2 request req_auth_bearer_token req_perform resp_status resp_body_json
#' @importFrom logger log_info log_error log_trace log_warn
#' @importFrom lubridate as_datetime with_tz
#' @export
get_forcetime <- function(testId, ...) {

  # 1. ----- Resolve Connection -----
  logger::log_trace("hawkinR/get_forcetime -> Resolving connection")
  extra_args <- list(...)

  if (!is.null(extra_args$profile)) {
    if (is.character(extra_args$profile)) {
      # User passed a name string, so we connect
      conn <- hd_connect(profile = extra_args$profile)
    } else {
      # User passed the object directly
      conn <- extra_args$profile
    }
  } else {
    conn <- get_active_conn()
  }

  # Validate
  if (!is.object(conn) || is.null(conn@access_token)) {
    stop("A valid HawkinAuth connection is required. Run hd_connect() first.", call. = FALSE)
  }

  # Token Lifecycle Management
  token_remaining <- round(as.numeric(difftime(conn@expires_at, Sys.time(), units = "secs")))
  logger::log_debug("hawkinR/get_forcetime -> Token expires in {token_remaining} seconds")
  if (token_remaining < 300) {
    logger::log_info("hawkinR/get_forcetime -> Token expiring soon. Refreshing...")
    conn <- authenticate(conn)
    set_active_conn(conn)
  }

  # 2. ----- Build Request -----
  logger::log_trace("hawkinR/get_forcetime -> Building request")
  request <- httr2::request(paste0(conn@base_url, "/", conn@config@org_id)) |>
    httr2::req_url_path_append("forcetime") |>
    httr2::req_url_path_append(testId)

  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug("hawkinR/get_forcetime -> {reqPath$method}: {reqPath$headers$host}{reqPath$path}")


  # 5. ----- Execute Call -----
  logger::log_trace("hawkinR/get_forcetime -> Executing API request")
  resp <-  request |>
    httr2::req_auth_bearer_token(conn@access_token) |>
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
    error_message <- "Error 500: Something went wrong. Please contact dev-team@hawkindynamics.com"
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

    # Parse RSI safely — value may be numeric, string "NA", or NULL
    rsi_obj <- tryCatch({
      val <- x$rsi
      if (is.null(val) || length(val) == 0) return(NULL)
      num <- suppressWarnings(as.numeric(val))
      if (is.na(num)) NULL else num
    }, error = function(e) NULL)

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
      testType_canonical   = as.character(testCanonical),
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
  } else {
    logger::log_error("hawkinR/get_forcetime -> Unexpected HTTP status: {status}")
    stop(paste0("Unexpected HTTP status: ", status), call. = FALSE)
  }
}
