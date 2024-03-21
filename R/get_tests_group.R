#' Get Test Trials By Groups
#'
#' @description
#' Get only tests of the specified group for an account.
#'
#' @usage
#' get_tests_group(groupId, from, to, sync = FALSE, active = TRUE)
#'
#' @param groupId Supply a group’s or a string of a comma separated list of group id’s to receive tests from
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
#' Response will be a data frame containing the trials from the specified team and within the time
#' range (if specified).
#'
#' **id**   *str*   Test trial unique ID
#'
#' **active**   *logi*   The trial is active and not disabled
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
#' **test_type_tag_ids**   *chr*   String of Ids associated with tags used during the test trial
#'
#' **test_type_tag_names**   *chr*   String of names of tags used during the test trial
#'
#' **test_type_tag_desc**   *chr*   String of descriptions of tags used during the test trial
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
#' ## Call for all tests by Group 1
#' dfGroup1 <- get_tests_group(groupId = "group1")
#'
#'
#' ## Call for all tests from Groups 1 & 2
#' dfGroups_1_2 <- get_tests_group(groupId = paste0("group1","group2"))
#'
#'
#' ## Call for all Group 1 tests since a specific date
#' df_Group1_Since <- get_tests_group("group1", from = 1689958617)
#'
#' }
#'
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @export

## Get Tests Data by Group Id -----
get_tests_group <- function(groupId, from = NULL, to = NULL, sync = FALSE, active = TRUE) {

  # Retrieve Access Token and Expiration from Environment Variables
  aToken <- base::Sys.getenv("accessToken")
  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration"))

  #-----#

  # Check for Access Token and Expiration
  if(base::is.null(aToken) || token_exp <= base::as.numeric(base::Sys.time())) {
    stop("Access token not available or expired. Call accToken() to refresh.")
  }

  # Group Id
  gId <- if(!is.character(groupId)) {
    stop("Error: groupId should be character string of a group ID , or a comma seperated list of
         group IDs in a single character string. Example: 'group1Id,group2Id,group3Id'")
  }

  # Check for Proper EPOCH Times
  epochArgCheck(arg.from = from, arg.to = to)

  #-----#

  # API Cloud URL
  urlCloud <- base::Sys.getenv("urlRegion")

  # From DateTime
  fromDT <- DateTimeParam(param= 'from', value = from, sync = sync)

  # To DateTime
  toDT <- DateTimeParam(param= 'to', value = from, sync = sync)

  # Create URL for request!!!!!!!
  URL <- base::paste0(urlCloud,"?groupId=", gId, fromDT, toDT)

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
    base::stop("Error 401: Invalid Access token.")
  } else if(response$status_code == 500) {
    base::stop("Error 500: Someting went wrong. Please contact support@hawkindynamics.com")
  } else if(response$status_code == 200) {
    x <- tryCatch({
      base::data.frame(
        jsonlite::fromJSON(
          httr::content(response, "text")
        )
      )
    }, error = function(e) {
      # Handle the error here
      base::stop("No tests returned. If you feel this is incorrect, check the groupId and date range.")
    })


    #-----#
    ### Create data frame ###
    #-----#

    ##--- Filter cases ---###

    # Clean Resp Headers
    base::names(x) <- base::sub("^data\\.", "", base::names(x))

    # Split the ID string into individual IDs
    groupIds <- base::unlist(base::strsplit(groupId, ","))

    # Check if any of the IDs in groupIds are present in any of the lists in the 'athlete.teams' column
    filtered_df <- x %>%
      dplyr::filter(base::any(base::sapply(.data$athlete$groups, function(ids) base::any(ids %in% groupIds))))

    ###
    # Use an if statement to handle the cases
    x <- if (base::nrow(filtered_df) > 0) {
      # Data matching the ID(s) was found
      x <- filtered_df

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

    } else {
      base::stop("No data returned. Check groupId")
    }

    x
  }

  #-----#

  # Return Response
  return(Resp)

}
