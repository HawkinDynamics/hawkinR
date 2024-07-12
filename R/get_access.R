#' Get Access Token
#'
#' @description Use the Refresh Token generated to get a valid Access Token. Only the organization
#' administrator account has the ability to generate API tokens.
#'
#' @usage
#' get_access(`refreshToken`, `region` = "Americas")
#'
#' @param refreshToken Use the Refresh Token generated from
#' 'https://cloud.hawkindynamics.com/integrations'.
#'
#' @param region The region to define the URL to be used. Options: "Americas" (default),
#' "Europe", "Asia/Pacific".
#'
#' @details
#' Use this function to initiate access to your data in the cloud. All other hawkinR functions will
#' depend on the values returned from this function.
#'
#' When correct inputs are passed through the `region` and `refreshToken` parameters, the returned
#' access token, expiration time, and regional URL will be stored in the system for use by other
#' functions during this session.
#'
#' The accessToken is set to expire every 60 minutes. If the token has expired, and you attempt to use
#' a dependent function, you will be prompted to run this function again to receive a new access token.
#'
#' @return A data frame with necessary information for accessing API (access token, token expiration,
#' URL region). The contents of the data frame are stored in the system environment.
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called with the region defaulting to "Americas".
#' # Replace 'refresh token' with an actual authentication token.
#'
#' get_access('refreshToken')
#'
#' # If you are in a different region and use one of the other URLs, declare your region by using the
#' #`region` parameter.
#'
#' get_access('refreshToken', region = "Europe")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 request req_auth_bearer_token req_error req_perform resp_status resp_body_json
#' @importFrom logger log_info
#' @importFrom lubridate as_datetime with_tz
#' @importFrom jsonlite fromJSON
#'
#' @export


# Get Access Token-----
get_access <- function(refreshToken, region = "Americas") {

  # 1. ----- Set Logger -----
  # Log Trace
  logger::log_trace(paste0("hawkinR -> Run: get_access"))

  # Save the current setting
  old_show_error_messages <- base::getOption("show.error.messages")
  base::on.exit(options(show.error.messages = old_show_error_messages),
                add = TRUE)

  # Disable error messages
  base::options(show.error.messages = FALSE)

  # 2. ----- Parameter Validation -----

  if (base::is.character(refreshToken)) {
    rToken <- refreshToken
  } else {
    stop(logger::log_error(
      paste(
        "hawkinR/get_access -> refreshToken invalid. token must be of class <chr>"
      )
    ))
  }

  # 3. ----- Set the API URL based on the region -----
  urlToken <- base::switch(
    region,
    "Americas" = "https://cloud.hawkindynamics.com/api/token",
    "Europe" = "https://eu.cloud.hawkindynamics.com/api/token",
    "Asia/Pacific" = "https://apac.cloud.hawkindynamics.com/api/token",
    "Dev" = "https://cloud.dev.hawkindynamics.com/api/token",
    stop("Invalid region specified.")
  )

  urlCloud <- base::switch(
    region,
    "Americas" = "https://cloud.hawkindynamics.com/api/dev",
    "Europe" = "https://eu.cloud.hawkindynamics.com/api/dev",
    "Asia/Pacific" = "https://apac.cloud.hawkindynamics.com/api/dev",
    "dev" = "https://cloud.dev.hawkindynamics.com/api/dev"
  )

  # Log Debug
  logger::log_debug(base::paste0("hawkinR/get_access -> Cloud URL: ", urlCloud))

  #-----#

  # Build Request
  request <- httr2::request(urlToken) %>%
    # Supply Bearer Authentication
    httr2::req_auth_bearer_token(token = rToken)

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
  } else if (status == 403) {
    error_message <- 'Error 403: Refresh Token is missing'
  } else if (status == 500) {
    error_message <-
      'Error 500: Something went wrong. Please contact support@hawkindynamics.com'
  }

  if (!base::is.null(error_message)) {
    stop(logger::log_error(paste0(
      "hawkinR/get_access -> ", error_message
    )))
  }

  # Response Table
  if (status == 200) {
    # Response GOOD - Run rest of script
    x <- httr2::resp_body_json(resp = resp,
                               check_type = TRUE,
                               simplifyVector = TRUE)

    # Set environment variables from Response
    Sys.setenv(
      "accessToken" = x$access_token[1],
      "accessToken_expiration" = x$expires_at[1],
      "urlRegion" = urlCloud
    )

    # Log Debug
    logger::log_debug(paste0("hawkinR/get_access -> Temp Access Token: ", x$access_token[1]))
    logger::log_debug(paste0(
      "hawkinR/get_access -> Access Token Expiration: ",
      x$expires_at[1]
    ))
  }

  #-----#

  outMsg <- paste(
    "Your access token was recieved and stored for use by other hawkinR functions. Your token will expire at",
    lubridate::with_tz(
      lubridate::as_datetime(base::as.numeric(Sys.getenv(
        "accessToken_expiration"
      )),
      tz = "UTC"),
      tzone = base::Sys.timezone()
    )
  )

  return(logger::log_success(paste0("hawkinR/get_access -> ", outMsg)))
}


