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
#' @export
get_access <- function(refreshToken, region = "Americas") {

  # API Token URL
  urlToken <- if(region == "Americas") {
    "https://cloud.hawkindynamics.com/api/token"
  } else if(region == "Europe") {
    "https://eu.cloud.hawkindynamics.com/api/token"
  } else if(region == "Asia/Pacific") {
    "https://apac.cloud.hawkindynamics.com/api/token"
  } else {
    "https://cloud.dev.hawkindynamics.com/api/token"
  }

  #-----#

  # API Cloud URL
  urlCloud <- if(region == "Americas") {
    "https://cloud.hawkindynamics.com/api/dev/"
  } else if(region == "Europe") {
    "https://eu.cloud.hawkindynamics.com/api/dev/"
  } else if(region == "Asia/Pacific") {
    "https://apac.cloud.hawkindynamics.com/api/dev/"
  } else {
    "https://cloud.dev.hawkindynamics.com/api/dev/"
  }

  #-----#

  # Call Variables
  payload <- ""
  encode <- "raw"

  #-----#

  # GET Request
  response <- httr::VERB("GET",
                         urlToken,
                         body = payload,
                         httr::add_headers(Authorization = paste0("Bearer ", refreshToken)),
                         httr::content_type("application/octet-stream"), encode = encode
  )

  #-----#

  # Response Table
  tokResp <- if(response$status_code == 401) {
    # Invalid Token Response
    "Refresh Token is invalid or expired."
  } else  if(response$status_code == 403){
    # Missing Refresh Token Response
    "Refresh Token is missing"
  } else  if(response$status_code == 500){
    # Contact Support Response
    "Something went wrong. Please contact support@hawkindynamics.com"
  } else  if(response$status_code == 200){
    # Response GOOD - Run rest of script
    x <- data.frame(
      jsonlite::fromJSON(
        httr::content(response, "text")
      )
    )

    # Set environment variables from Response
    Sys.setenv(
      "accessToken" = x$access_token[1],
      "accessToken_expiration" = x$expires_at[1],
      "urlRegion" = urlCloud
    )
  }

  #-----#

  return(
    if(response$status_code == 200) {
      print(
        paste(
          "Success! Your access token was recieved and stored for use by other hawkinR functions. Your token will expire at",
          lubridate::with_tz(
            lubridate::as_datetime(
              base::as.numeric(Sys.getenv("accessToken_expiration")),
              tz = "UTC"
            ),
            tzone = base::Sys.timezone()
          )
        )
      )
    } else {
      print(tokResp)
    }
  )

}
