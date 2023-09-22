#' Get All Tests or Sync Tests
#'
#' @description
#' Use this function to retrieve all tests or sync tests from your database. You can specify a time frame from, or to, which the tests should come (or be synced)
#'
#' @usage
#' get_tests(from, to, sync = FALSE)
#'
#' @return
#' Response will be a data frame containing the trials within the time range (if specified)
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
#' ## Call for all tests
#' dfAllTests <- get_tests()
#'
#'
#' ## Call for all tests within a specific time frame
#' dfFromTo <- get_tests(from = 1689958617, to = 1691207356)
#'
#'
#' ## Call for all new tests since a specific date, or any tests that have been updated/changed since that date
#' dfSync <- get_tests(from = 1689958617, sync=TRUE)
#'
#' }
#'
#' @importFrom rlang .data
#' @export

get_tests <- function(from = NULL, to = NULL, sync = FALSE) {

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
  } else if(base::isTRUE(sync)){
    base::paste0("?syncFrom=",from)
  } else {
    base::paste0("?from=",from)
  }


  # To DateTime
  toDT <- if(base::is.null(to)) {
    ""
  } else if(base::is.null(from)) {
    base::paste0("?to=",to)
  } else if(base::is.null(from) && base::isTRUE(sync)) {
    base::paste0("?syncTo=",to)
  } else if(base::isTRUE(sync)) {
    base::paste0("&syncTo=",to)
  } else {
    base::paste0("&to=",to)
  }


  # Create URL for request!!!!!!!
  URL <- base::paste0(urlCloud, fromDT, toDT)

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

