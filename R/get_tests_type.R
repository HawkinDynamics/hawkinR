#' Get Test Trials By Test Type
#'
#' @description
#' Use this function to retrieve only tests of the specified type.
#'
#' @usage
#' get_tests_type(typeId, from, to)
#'
#' @return
#' Response will be a data frame containing the trials of the specified test type within the time range (if specified)
#'
#' id: <chr> test's unique ID
#'
#' name: <chr> test's given name
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#'
#' ## Call for all CMJ tests
#' df_cmj <- get_tests_type(typeId = "7nNduHeM5zETPjHxvm7s")
#'
#'
#' ## Call for Free Run tests within a  specific time frame
#' df_free <- get_tests_type(typeId = "5pRSUQVSJVnxijpPMck3", from = 1689958617, to = 1691207356)
#'
#'
#' ## Call for Squat Jump tests since a specific date
#' df_sjSince <- get_tests_type("QEG7m7DhYsD6BrcQ8pic", from = 1689958617)
#'
#' }
#'
#' @importFrom rlang .data
#' @export


## Get Tests Data by Test Type -----
get_tests_type <- function(typeId, from = NULL, to = NULL) {

  # Retrieve Access Token and Expiration from Environment Variables
  aToken <- base::Sys.getenv("accessToken")
  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration"))

  #-----#

  # Check for Access Token and Expiration
  if(base::is.null(aToken) || token_exp <= base::as.numeric(base::Sys.time())) {
    stop("Access token not available or expired. Call accToken() to refresh.")
  }

  # Check for Proper EPOCH Times
  if(!is.null(from) && !is.numeric(from)) {
    stop("Error: `from` expecting numeric EPOCH/Unix timestamp.")
  } else if(!is.null(to) && !is.numeric(to)) {
    stop("Error: `to` expecting numeric EPOCH/Unix timestamp.")
  }

  #-----#

  # API Cloud URL
  urlCloud <- base::Sys.getenv("urlRegion")


  # From DateTime
  fromDT <- if(base::is.null(from)) {
    ""
  } else {
    base::paste0("&from=",from)
  }

  # To DateTime
  toDT <- if(base::is.null(to)) {
    ""
  } else {
    base::paste0("&to=",to)
  }

  # Create URL for request!!!!!!!
  URL <- base::paste0(urlCloud,"?testTypeId=", typeId, fromDT, toDT)

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
                         httr::content_type("application/octet-stream"), encode = encode
  )

  #-----#

  # Response
  Resp <- if(response$status_code == 401) {
    "Invalid Access token."
  } else if(response$status_code == 500) {
    "Someting went wrong. Please contact support@hawkindynamics.com"
  } else if(response$status_code == 200) {
    x <- data.frame(
      jsonlite::fromJSON(
        httr::content(response, "text")
      )
    )

    x
  }

  #-----#


  # Return Response
  return(
    if(response$status_code == 200) {

      # Clean Resp Headers
      base::names(Resp) <- base::sub("^data\\.", "", base::names(Resp))

      # UnNest testType and Athlete data
      Resp <- Resp %>% tidyr::unnest(c(testType, athlete), names_sep = ".")

      Resp
    } else {
      base::print(Resp)
    }
  )

}
