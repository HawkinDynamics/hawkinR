#' Get Test Trials By Test Type
#'
#' @description
#' Get only tests of the specified type for an account.
#'
#' @usage
#' get_tests_type(typeId, from, to)
#'
#' @param typeId Supply a test type id to only retrieve tests of that type.
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
#' Response will be a data frame containing the trials of the specified type and within the time range (if specified).
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
#' Only metrics of the given test type are included as the remaining variables.
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
#' @importFrom tidyr unnest
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
    base::stop("Error: `from` expecting numeric EPOCH/Unix timestamp.")
  } else if(!is.null(to) && !is.numeric(to)) {
    base::stop("Error: `to` expecting numeric EPOCH/Unix timestamp.")
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

  # Valid test type IDs
  valid_typeIds <- c(
    "7nNduHeM5zETPjHxvm7s",
    "QEG7m7DhYsD6BrcQ8pic",
    "2uS5XD5kXmWgIZ5HhQ3A",
    "gyBETpRXpdr63Ab2E0V8",
    "5pRSUQVSJVnxijpPMck3",
    "pqgf2TPUOQOQs6r0HQWb",
    "r4fhrkPdYlLxYQxEeM78",
    "ubeWMPN1lJFbuQbAM97s",
    "rKgI4y3ItTAzUekTUpvR"
  )

  # check typeId
  tId <- if (typeId %in% valid_typeIds) {
    typeId
  } else {
    base::stop("typeId incorrect. Check your entry")
  }

  # Create URL for request!!!!!!!
  URL <- base::paste0(urlCloud,"?testTypeId=", tId, fromDT, toDT)

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
      base::stop("No tests returned. If you feel this is incorrect, check the typeId and date range.")
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

    # Clean colnames with janitor
    x <- janitor::clean_names(x)

    x
  }

  #-----#


  # Return Response
  return(Resp)

}
