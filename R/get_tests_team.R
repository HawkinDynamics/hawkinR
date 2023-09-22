#' Get Test Trials By Teams
#'
#' @description
#' Use this function to retrieve only tests of the specified teams.
#'
#' @usage
#' get_tests_team(teamId, from, to)
#'
#' @return
#' Response will be a data frame containing the trials of the specified teams within the time range (if specified)
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
#' ## Call for all tests by Group 1
#' dfteam1 <- get_tests_team(teamId = "team1")
#'
#'
#' ## Call for all tests from Groups 1 & 2
#' dfteam_1_2 <- get_tests_team(teamId = paste0("team1","team2"))
#'
#'
#' ## Call for all Group 1 tests since a specific date
#' df_team1_Since <- get_tests_team("team1", from = 1689958617)
#'
#' }
#'
#' @importFrom rlang .data
#' @export

get_tests_team <- function(teamId, from = NULL, to = NULL) {

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
  URL <- base::paste0(urlCloud,"?=teamId=", teamId, fromDT, toDT)

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

