#' Get Test Trials By Athlete
#'
#' @description
#' Get only tests of the specified athlete for an account.
#'
#' @usage
#' get_tests_ath(athleteId, from, to)
#'
#' @param athleteId Supply an athlete’s id to receive tests for a specific athlete
#'
#' @param from Optionally supply a time (Unix timestamp) you want the tests from. If you do not
#' supply this value you will receive every test. This parameter is best suited for bulk exports of
#' historical data
#'
#' @param to Optionally supply a time (Unix timestamp) you want the tests to. If you do not
#' supply this value you will receive every test from the beginning of time or the optionally
#' supplied `from` parameter. This parameter is best suited for bulk exports of historical data.
#'
#' @return
#' Response will be a data frame containing the trials from the specified team and within the time range (if specified).
#'
#' **id**   *str*   Test trial unique ID
#'
#' **timestamp**   *int*   UNIX timestamp of trial
#'
#' **segment**   *chr*   Description of the test type and trial number of the session (testType:trialNo)
#'
#' **testType.id**   *chr*   Id of the test type of the trial
#'
#' **testType.name**   *chr*   Name of the test type of the trial
#'
#' **testType.canonicalId**   *chr*   Canonical Id of the test type of the trial
#'
#' **athlete.id**   *chr*   Unique Id of the athlete
#'
#' **athlete.name**   *chr*   Athlete given name
#'
#' **athlete.active**   *logi*   The athlete is active
#'
#' **athlete.teams**   *list*   List containing Ids of each team the athlete is on
#'
#' **athlete.groups**   *list*   List containing Ids of each group the athlete is in
#'
#' All metrics from each test type are included as the remaining variables.
#' If a trial does not have data for a variable it is returned NA.
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#'
#' ## Call for all tests from a specified athlete
#' df_cmj <- get_tests_ath(athleteId = "athleteId")
#'
#'
#' ## Call for all tests within a specific time frame
#' dfFromTo <- get_tests_ath(athleteId = "athleteId", from = 1689958617, to = 1691207356)
#'
#'
#' ## Call for all tests since a specific date
#' dfSince <- get_tests_ath("athleteId", from = 1689958617)
#'
#' }
#'
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @export

## Get Tests Data by Athlete Id -----
get_tests_ath <- function(athleteId, from = NULL, to = NULL) {

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
  } else if(!is.numeric(from)) {
    base::stop("date must be in numeric unix format")
  } else{
    base::paste0("&from=",from)
  }

  # To DateTime
  toDT <- if(base::is.null(to)) {
    ""
  } else if(!is.numeric(to)) {
    base::stop("date must be in numeric unix format")
  } else{
    base::paste0("&to=",to)
  }

  # Athlete Id
  aId <- if(base::is.character(athleteId)) {
    athleteId
  } else {
    base::stop("Error: athleteId should be character string of an athlete ID. Example: 'athleteId'")
  }

  # Create URL for request!!!!!!!
  URL <- base::paste0(urlCloud,"?athleteId=", aId, fromDT, toDT)

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
    base::stop("Invalid Access token.")
  } else if(response$status_code == 500) {
    base::stop("Someting went wrong. Please contact support@hawkindynamics.com")
  } else if(response$status_code == 200) {
    # Convert JSON
    resp <- jsonlite::fromJSON(
      httr::content(response, "text")
    )

    # Evaluate Response
    x <- if(resp$count[1] > 0) {
      # Convert to data frame
      df <- base::data.frame(resp)

      # Clean Resp Headers
      base::names(df) <- base::sub("^data\\.", "", base::names(df))

      # UnNest testType and Athlete data
      df <- df %>% tidyr::unnest(c(.data$testType, .data$athlete), names_sep = ".")

      # Clean colnames with janitor
      df <- janitor::clean_names(df)

      df
    } else {
      base::stop("No trials returned. Check athleteId or from/to entries")
    }
  }

  #-----#


  # Return Response
  return(Resp)

}

