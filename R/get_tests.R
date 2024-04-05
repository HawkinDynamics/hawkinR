#' Get All Tests or Sync Tests
#'
#' @description
#' Get the tests for an account. You can specify a time frame from, or to, which the tests should come (or be synced).
#'
#' @usage
#' get_tests(from, to, sync = FALSE, active = TRUE)
#'
#' @param from Optionally supply a time (Unix timestamp) you want the tests from. If you do not
#' supply this value you will receive every test. This parameter is best suited for bulk exports of
#' historical data
#'
#' @param to Optionally supply a time (Unix timestamp) you want the tests to. If you do not
#' supply this value you will receive every test from the beginning of time or the optionally
#' supplied `from` parameter. This parameter is best suited for bulk exports of historical data.
#'
#' @param sync  The result set will include updated and newly created tests. This parameter is best
#' suited to keep your database in sync with the Hawkin database. If you do not supply this value
#' you will receive every test.
#'
#' @param active There was a change to the default API configuration to reflect the majority of
#' users API configuration. Inactive tests or tests where `active:false` are returned in these
#' configuration. Be default, `active` is set to TRUE. To return all tests, including disabled
#' trials, set `active` to FALSE.
#'
#' @return
#' Response will be a data frame containing the trials within the time range (if specified).
#'
#' **id**   *str*   Test trial unique ID
#'
#' **active**   *logi*   The trial is active and not disabled
#'
#' **timestamp**   *int*   UNIX timestamp of trial
#'
#' **segment**   *chr*   Description of the test type and trial number of the session (testType:trialNo)
#'
#' **test_type_id**   *chr*   Id of the test type of the trial
#'
#' **test_type_name**   *chr*   Name of the test type of the trial
#'
#' **test_type_canonicalId**   *chr*   Canonical Id of the test type of the trial
#'
#' **test_type_tag_ids**   *chr*   String of Ids associated with tags used during the test trial
#'
#' **test_type_tag_names**   *chr*   String of names of tags used during the test trial
#'
#' **test_type_tag_desc**   *chr*   String of descriptions of tags used during the test trial
#'
#' **athlete_id**   *chr*   Unique Id of the athlete
#'
#' **athlete_name**   *chr*   Athlete given name
#'
#' **athlete_active**   *logi*   The athlete is active
#'
#' **athlete_teams**   *list*   List containing Ids of each team the athlete is on
#'
#' **athlete_groups**   *list*   List containing Ids of each group the athlete is in
#'
#' All metrics from each test type are included as the remaining variables.
#' If a trial does not have data for a variable it is returned NA.
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
#' ## Call for all new tests since a specific date, or any tests that have been
#' updated/changed since that date
#' dfSync <- get_tests(from = 1689958617, sync=TRUE)
#'
#' }
#'
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @export

## Get All Tests or Sync Tests -----
get_tests <- function(from = NULL, to = NULL, sync = FALSE, active = TRUE) {

  # Retrieve Access Token and Expiration from Environment Variables
  aToken <- base::Sys.getenv("accessToken")
  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration"))

  #-----#

  # Check for Access Token and Expiration
  if(base::is.null(aToken) || token_exp <= base::as.numeric(base::Sys.time())) {
    stop("Access token not available or expired. Call accToken() to refresh.")
  }

  # Check for Proper EPOCH Times
  epochArgCheck(arg.from = from, arg.to = to)

  #-----#

  # API Cloud URL
  urlCloud <- base::Sys.getenv("urlRegion")

  # Handle Date time args
  dt <- if(is.null(to) && isFALSE(sync)) {     # from
    paste0('?from=',from)
  } else if(is.null(from) && isFALSE(sync)) {     # to
    paste0('?to=',to)
  } else if(is.null(to) && isTRUE(sync)) {     # sync from
    paste0('?syncFrom=',from)
  } else if(is.null(from) && isTRUE(sync)) {     # sync to
    paste0('?syncTo=',to)
  } else if(isFALSE(sync)) {     # from to
    paste0('?from=', from, '&to=', to)
  } else if(isTRUE(sync)) {     # sync from to
    paste0('?syncFrom=', from, '&syncTo=', to)
  }

  # Create URL for request
  URL <- base::paste0(urlCloud, dt)

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
    "Error 401: Invalid Access token."
  } else if(response$status_code == 500) {
    "Error 500: Someting went wrong. Please contact support@hawkindynamics.com"
  } else if(response$status_code == 200) {
    x <- tryCatch({
      base::data.frame(
        jsonlite::fromJSON(
          httr::content(response, "text")
        )
      )
    }, error = function(e) {
      # Handle the error here
      base::stop("No tests returned. If you feel this is incorrect, check the date range.")
    })

    #-----#
    ### Create data frame ###
    #-----#

    # Clean Resp Headers
    base::names(x) <- base::sub("^data\\.", "", base::names(x))

    ##-- External IDs --##

    # Create athlete df
    a <- AthletePrep(arg.df = x$athlete)

    ##-- Test Types --##

    # Create testType df
    t <- TagPrep(x$testType)

    ##-- finish data frame --##

    # select trial metadata metrics from DF
    x1 <- dplyr::select(.data = x, base::c('id', 'timestamp', 'segment'))

    # select all metrics from DF
    x2 <- dplyr::select(.data = x, -base::c('id', 'timestamp', 'segment', 'testType', 'athlete'))

    # create new complete DF
    x <- base::cbind(x1, t, a, x2)

    # Clean colnames with janitor
    x <- janitor::clean_names(x)

    # filter inactive tests
    x <- if( base::isTRUE(active) ) {
      filt <- dplyr::filter(.data = x, active == TRUE)

      filt <- dplyr::relocate(.data = filt, 'active', .before = 'timestamp')

      filt
    } else if( base::isFALSE(active) ){
      x <- dplyr::relocate(.data = x, 'active', .before = 'timestamp')

      x
    }

    # return final DF
    x
  }

  #-----#

  # Return Response
  return(Resp)

}
