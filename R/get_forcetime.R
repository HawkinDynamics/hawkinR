#' Get Force-Time Data
#'
#' @description
#' Use this function to retrieve the raw recorded force-time data from a trial. This includes both left, right and combined force data at 1000hz (per millisecond).
#' Calculated velocity, displacement, and power at each time interval will also be included.
#'
#' @usage
#' get_forcetime(testID)
#'
#' @param testID Give the unique test id of the trial you want to be called.
#'
#' @return
#' Response will be a data frame containing the following:
#'
#' **time_s** (int): elapsed time in seconds, starting from end of identified quiet phase
#'
#' **force_right** (int): Force recorded from the RIGHT platform coinciding with time point from  `time_s`, measured in Newtons (N)
#'
#' **force_Left** (int): Force recorded from the LEFT platform coinciding with time point from  `time_s`, measured in Newtons (N)
#'
#' **force_combined** (int): Sum of forces from LEFT and RIGHT, coinciding with time point from  `time_s`, measured in Newtons (N)
#'
#' **velocity_m.s** (int): Calculated velocity of center of mass at time interval, measured in meters per second (m/s)
#'
#' **displacement_m** (int): Calculated displacement of center of mass at time interval, measured in meters (m)
#'
#' **power_w** (int): Calculated power of mass at time interval, measured in watts (W)
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#' df_forcetime <- get_forcetime( testID = `stringId` )
#'
#' }
#'
#' @importFrom rlang .data
#' @export
get_forcetime <- function(testID) {

  # Retrieve access token and expiration from environment variables
  aToken <- base::Sys.getenv("accessToken")
  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration" ))

  #-----#

  # Check for Access Token and Expiration
  if(base::is.null(aToken) || token_exp <= base::as.numeric(base::Sys.time())) {
    stop("Access token not available or expired. Call get_access() to obtain it.")
  }

  #-----#

  # API Cloud URL
  urlCloud <- base::Sys.getenv("urlRegion")

  # Create URL for request
  URL <-base::paste0(urlCloud,"/forcetime/",testID)

  #-----#

  # Call Variables
  payload <- ""
  encode <- "raw"

  #-----#

  # GET Request
  response <- httr::VERB("GET",
                         URL,
                         body = payload,
                         httr::add_headers(Authorization = base::paste0("Bearer ", aToken)),
                         httr::content_type("application/octet-stream"),
                         encode = encode
  )

  #-----#

  # Response Table
  Resp <- if(response$status_code == 401) {
    # Invalid Token Response
    "Invalid Access Token."
  } else  if(response$status_code == 404){
    "Requested Resource Not Found"
  } else  if(response$status_code == 500){
    # Contact Support Response
    "Something went wrong. Please contact support@hawkindynamics.com"
  } else  if(response$status_code == 200){
    # Response GOOD - Run rest of script
    x <- jsonlite::fromJSON(
        httr::content(response, "text")
      )

    x
  }

  #-----#

  # Return Response
  return(
    if(response$status_code == 200) {
      tbl <- base::data.frame(
        "time_s" = Resp$`Time(s)`,
        "force_right" = Resp$`RightForce(N)`,
        "force_left" = Resp$`LeftForce(N)`,
        "force_combined" = Resp$`CombinedForce(N)`,
        "velocity_m.s" = Resp$`Velocity(m/s)`,
        "displacement_m" = Resp$`Displacement(m)`,
        "power_w" = Resp$`Power(W)`
      )

      tbl
    } else {
      base::print(Resp)
    }
  )

}
