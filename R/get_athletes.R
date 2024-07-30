#' Get Athletes
#'
#' @description
#' Get the athletes for an account. Inactive players will only be included if
#' `includeInactive` parameter is set to TRUE.
#'
#' @usage
#' get_athletes(includeInactive = FALSE)
#'
#' @param includeInactive FALSE by default to exclude inactive players in database. Set to TRUE if you want
#' inactive players included in the return.
#'
#' @return
#' Response will be a data frame containing the athletes that match this query.
#' Each athlete includes the following variables:
#'
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **id**          | *chr*    | athlete's unique ID |
#' | **name**        | *chr*    | athlete's given name (First Last) |
#' | **active**      | *bool*   | athlete is active (TRUE) |
#' | **teams**       | *chr*    | team ids separated by "," |
#' | **groups**      | *chr*    | group ids separated by "," |
#' | **external**    | *chr*    | external properties will have a column of their name with the appropriate values for the athlete of `NA` if it does not apply |
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called. If you only wish to call active players,
#' # you don't need to provide any parameters.
#'
#' df_athletes <- get_athletes()
#'
#' # If you want to include all athletes, including inactive athletes, include the optional
#' # `includeInactive` parameter.
#'
#' df_wInactive <- get_athletes( includeInactive = TRUE)
#'
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 request req_url_path_append req_url_query req_auth_bearer_token req_error req_perform resp_status resp_body_json
#' @importFrom rlang .data
#' @importFrom logger log_info
#'
#' @export


# Get Athletes -----
get_athletes <- function(includeInactive = FALSE) {

  # 1. ----- Set Logger -----
  # Log Trace
  logger::log_trace(base::paste0("hawkinR -> Run: get_athltes"))

  # 2. ----- Parameter Validation -----

  # Retrieve access token and expiration from environment variables
  aToken <- base::Sys.getenv("accessToken")

  #-----#

  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration"))

  #-----#

  # Check for Access Token and Expiration
  if (base::is.null(aToken) ||
      token_exp <= base::as.numeric(base::Sys.time())) {
    logger::log_error("hawkinR/get_athletes -> Access token not available or expired. Call get_access() to obtain it.")
    stop("Access token not available or expired. Call get_access() to obtain it.")
  } else {
    # Log Debug
    logger::log_debug(base::paste0("hawkinR/get_athletes -> Temporary access token expires: ", base::as.POSIXct(token_exp)))
  }

  # 3. ----- Build URL Request -----

  # Include inactive athletes
  inactives <- if (includeInactive) {
    "true"
  } else {
    "false"
  }

  # Build Request
  request <- httr2::request(base::Sys.getenv("urlRegion")) %>%
    # Add URL Path
    httr2::req_url_path_append("/athletes") %>%
    # Add Inactive Query
    httr2::req_url_query(includeInactive = inactives) %>%
    # Supply Bearer Authentication
    httr2::req_auth_bearer_token(token = aToken)

  # Log Debug
  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug(base::paste0(
    "hawkinR/create_athletes -> ",
    reqPath$method,
    ": ",
    reqPath$headers$host,
    reqPath$path
  ))

  # Execute Call
  resp <- request %>%
    httr2::req_error(
      is_error = function(resp)
        FALSE
    ) %>%
    httr2::req_perform()

  # Response Status
  status <- httr2::resp_status(resp = resp)

  # 4. ----- Create Response Outputs -----

  # Error Handler
  error_message <- NULL

  if (status == 401) {
    error_message <- 'Error 401: Refresh Token is invalid or expired.'
  } else if (status == 500) {
    error_message <-
      'Error 500: Something went wrong. Please contact support@hawkindynamics.com'
  }

  if (!base::is.null(error_message)) {
    logger::log_error(base::paste0(
      "hawkinR/get_athletes -> ", error_message
    ))
    stop(error_message)
  }

  # Response Table
  if (status == 200) {
    # Response GOOD - Run rest of script
    # Convert JSON Response
    x <- httr2::resp_body_json(resp = resp,
                               check_type = TRUE,
                               simplifyVector = TRUE)

    # 5. ----- Sort Response Athlete Data -----

    # Print confirmation response
    logger::log_success(base::paste0("hawkinR/get_athletes -> ", x[[2]], " athletes returned"))

    # Create data frame from returns data
    df <- base::as.data.frame(x[[1]])

    # Handle External Properties
    if (base::ncol(df) > 5) {
      # Select base athlete attributes
      a <- df[, 1:5]
      # Select any external value pairs
      b <- df[, 6:base::ncol(df)]

      # Return new data frame with externals expanded
      return(base::cbind(a, b))
    } else {
      # Return data frame
      return(df)
    }
  }
}


