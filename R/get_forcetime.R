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
#'
#' **time_s**   *int*   Elapsed time in seconds, starting from end of identified quiet phase
#'
#' **force_right**   *int*   Force recorded from the RIGHT platform coinciding with time point from  `time_s`, measured in Newtons (N)
#'
#' **force_Left**   *int*   Force recorded from the LEFT platform coinciding with time point from  `time_s`, measured in Newtons (N)
#'
#' **force_combined**   *int*   Sum of forces from LEFT and RIGHT, coinciding with time point from  `time_s`, measured in Newtons (N)
#'
#' **velocity_m.s**   *int*   Calculated velocity of center of mass at time interval, measured in meters per second (m/s)
#'
#' **displacement_m**   *int*   Calculated displacement of center of mass at time interval, measured in meters (m)
#'
#' **power_w**   *int*   Calculated power of mass at time interval, measured in watts (W)
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#' df_ft <- get_forcetime( testId = `stringId` )
#'
#' }
#'
#' @importFrom rlang .data
#' @export


## Get Force Time Data -----
get_forcetime <- function(testId) {

  # Retrieve access token and expiration from environment variables
  aToken <- base::Sys.getenv("accessToken")
  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration" ))

  #-----#

  # Check for Access Token and Expiration
  if(base::is.null(aToken) || token_exp <= base::as.numeric(base::Sys.time())) {
    base::stop("Access token not available or expired. Call get_access() to obtain it.")
  }

  #-----#

  # API Cloud URL
  urlCloud <- base::Sys.getenv("urlRegion")

  tId <- if( base::is.character(testId)) {
    testId
  } else {
    base::stop("Incorrect testId. Must be a character string.")
  }

  # Create URL for request
  URL <-base::paste0(urlCloud,"/forcetime/",tId)

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
    base::stop("Invalid Access Token.")
  } else  if(response$status_code == 404){
    base::stop("Requested Resource Not Found")
  } else  if(response$status_code == 500){
    # Contact Support Response
    base::stop("Something went wrong. Please contact support@hawkindynamics.com")
  } else  if(response$status_code == 200){
    # Response GOOD - Run rest of script
    x <- jsonlite::fromJSON(
      httr::content(response, "text")
    )

    # create columns for df

    ## Time
    t <-  x$`Time(s)`

    ## Force Right
    rF <- if(length(x$`RightForce(N)`) == 0) {
      seq(from = 0, to=0, length.out = length(x$`Time(s)`))
    } else {x$`RightForce(N)`}

    ## Force Left
    lF <- if(length(x$`LeftForce(N)`) == 0) {
      seq(from = 0, to=0, length.out = length(x$`Time(s)`))
    } else {x$`LeftForce(N)`}

    ## Combined Force
    cF <- if(length(x$`CombinedForce(N)`) == 0) {
      seq(from = 0, to=0, length.out = length(x$`Time(s)`))
    } else {x$`CombinedForce(N)`}

    # Velocity
    v <- if(length(x$`Velocity(m/s)`) == 0) {
      seq(from = 0, to=0, length.out = length(x$`Time(s)`))
    } else {x$`Velocity(m/s)`}

    # Displacement
    d <- if(length(x$`Displacement(m)`) == 0) {
      seq(from = 0, to=0, length.out = length(x$`Time(s)`))
    } else {x$`Displacement(m)`}

    # Power
    p <- if(length(x$`Power(W)`) == 0) {
      seq(from = 0, to=0, length.out = length(x$`Time(s)`))
    } else {x$`Power(W)`}

    # create data frame
    tbl <- base::data.frame(
      "time_s" = t,
      "force_right" = rF,
      "force_left" = lF,
      "force_combined" = cF,
      "velocity_m.s" = v,
      "displacement_m" = d,
      "power_w" = p
    )

    tbl
  }

  #-----#

  # Return Response
  return(Resp)

}

