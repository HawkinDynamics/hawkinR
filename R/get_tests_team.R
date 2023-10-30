#' Get Test Trials By Teams
#'
#' @description
#' Get only tests of the specified team for an account.
#'
#' @usage
#' get_tests_team(teamId, from, to)
#'
#' @param teamId Supply a team’s or a string of a comma separated list of group id’s to receive tests from
#' specific groups. Recommended to use method `paste0()`. A maximum of 10 groups can be fetched at once.
#'
#' @param from Optionally supply a time (Unix timestamp) you want the tests from. If you do not
#' supply this value you will receive every test. This parameter is best suited for bulk exports of
#' historical data.
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
#' **athlete.teams**   *List*   list containing Ids of each team the athlete is on
#'
#' **athlete.groups**   *List*   list containing Ids of each group the athlete is in
#'
#' All metrics from each test type are included as the remaining variables.
#' If a trial does not have data for a variable it is returned NA.
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
#' @importFrom tidyr unnest
#' @export

## Get Tests Data by Team Id -----
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

  # Team Id
  tId <- if(base::is.character(teamId)) {
    teamId
  } else {
    stop("Error: teamId should be character string of a team ID , or a comma seperated list of
         team IDs in a single character string. Example: 'team1Id,team2Id,team3Id'")
  }

  # Create URL for request!!!!!!!
  URL <- base::paste0(urlCloud,"?teamId=", tId, fromDT, toDT)

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
    x <- tryCatch({
      base::data.frame(
        jsonlite::fromJSON(
          httr::content(response, "text")
        )
      )
    }, error = function(e) {
      # Handle the error here
      base::stop("No tests returned. If you feel this is incorrect, check the teamId and date range.")
    })

    # Check if an error occurred
    if (base::inherits(x, "try-error")) {
      # Handle the error case here
    } else {
      # The code ran successfully, and 'result' contains the data frame
    }

    # Clean Resp Headers
    base::names(x) <- base::sub("^data\\.", "", base::names(x))

    # UnNest testType and Athlete data
    x <- x %>% tidyr::unnest(c(.data$testType, .data$athlete), names_sep = ".")

    # Split the ID string into individual IDs
    teamIds <- base::unlist(base::strsplit(teamId, ","))

    # Check if any of the IDs in teamIds are present in any of the lists in the 'athlete.teams' column
    filtered_df <- x %>%
      dplyr::filter(base::any(base::sapply(.data$athlete.teams, function(ids) base::any(ids %in% teamIds))))

    # Clean column names with janitor
    filtered_df <- janitor::clean_names(filtered_df)

    # Use an if statement to handle the cases
    x <- if (base::nrow(filtered_df) > 0) {
      # Data matching the ID(s) was found
      filtered_df
    } else {
      base::stop("No data returned. Check teamId")
    }

    x
  }

  #-----#


  # Return Response
  return(Resp)

}
