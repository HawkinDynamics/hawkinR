#' Get All Tests or Sync Tests
#'
#' @description
#' Get the tests for an account. You can specify a time frame from, or to, which the tests
#' should
#' come (or be synced).
#'
#' @usage
#' get_tests(
#'  from = NULL,
#'  to = NULL,
#'  sync = FALSE,
#'  athleteId = NULL,
#'  typeId = NULL,
#'  teamId = NULL,
#'  groupId = NULL,
#'  includeInactive = FALSE
#'  )
#'
#' @param from Optionally supply a time (Unix time stamp) you want the tests from. If you
#' do not supply this value you will receive every test. This parameter is best suited
#' for bulk exports of historical data
#'
#' @param to Optionally supply a time (Unix time stamp) you want the tests to. If you do
#' not supply this value you will receive every test from the beginning of time or the
#' optionally supplied `from` parameter. This parameter is best suited for bulk exports
#' of historical data.
#'
#' @param sync The result set will include updated and newly created tests. This parameter
#' is best suited to keep your database in sync with the Hawkin database. If you do not
#' supply this value you will receive every test.
#'
#' @param includeInactive There was a change to the default API configuration to reflect
#' the majority of users API configuration. Inactive tests or tests where `active = false`
#' are returned in these configuration. Be default, `includeInactive` is set to `FALSE`.
#' To return all tests, including disabled trials, set `includeInactive` to `TRUE`.
#'
#' @param athleteId Supply an athlete’s id to receive tests for a specific athlete
#'
#' @param typeId Supply a value of type string. Must be canonical test Id, test type name,
#' or test name abbreviation.
#'
#' Names | Abbreviations: "Countermovement Jump" | "CMJ", "Squat Jump" | "SJ",
#' "Isometric Test" | "ISO", "Drop Jump" | "DJ", "Free Run" | "FREE",
#' "CMJ Rebound" | "CMJR", "Multi Rebound" | "MR", "Weigh In" | "WI",
#' "Drop Landing" | "DL", "TS Free Run" | "TSFR", "TS Isometric Test" | "TSISO"
#'
#' @param groupId Supply a group’s ID, a list of group IDs, or a string of a comma
#' separated group id’s to receive tests from the specified groups. A maximum of 10 groups
#' can be fetched at once.
#'
#' @param teamId Supply a team’s ID, a list of team IDs, or a string of a comma separated
#' team id’s to receive tests from the specified teams. A maximum of 10 teams can be
#' fetched at once.
#'
#' @return
#' Response will be a data frame containing the trials within the time range
#' (if specified).
#'
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **id**          | *str*    | Test trial unique ID |
#' | **active**      | *logi*   | The trial is active and not disabled |
#' | **timestamp**   | *int*    | UNIX time stamp of trial |
#' | **segment**     | *chr*    | Description of the test type and trial
#' number of the session (testType:trialNo) |
#' | **test_type_id** | *chr*   | Id of the test type of the trial |
#' | **test_type_name** |  *chr* | Name of the test type of the trial |
#' | **test_type_canonicalId** | *chr* | Canonical Id of the test type of the trial |
#' | **test_type_tag_ids** | *chr* | String of Ids associated with tags used during
#' the test trial |
#' | **test_type_tag_names** | *chr* | String of names of tags used during the
#' test trial |
#' | **test_type_tag_desc** | *chr* | String of descriptions of tags used during
#' the test trial |
#' | **athlete_id** | *chr* | Unique Id of the athlete |
#' | **athlete_name** | *chr* | Athlete given name |
#' | **athlete_active** | *logi* | The athlete is active |
#' | **athlete_teams** | *list* | List containing Ids of each team the athlete is on |
#' | **athlete_groups** | *list* | List containing Ids of each group the athlete is in |
#'
#' All metrics from each test type are included as the remaining variables
#' unless `typeId` is provided, then only the metrics of that test type will be returned.
#' If a trial does not have data for a variable it is returned NA.
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
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
#' @importFrom magrittr %>%
#' @importFrom httr2 request req_url_query req_auth_bearer_token req_error req_perform
#' @importFrom httr2 resp_status resp_body_json
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom lubridate as_datetime with_tz
#' @importFrom janitor clean_names
#' @importFrom logger log_info
#'
#' @export


## Get All Tests or Sync Tests -----
get_tests <- function(from = NULL,
                      to = NULL,
                      sync = FALSE,
                      athleteId = NULL,
                      typeId = NULL,
                      teamId = NULL,
                      groupId = NULL,
                      includeInactive = FALSE) {

  # 1. ----- Set Logger -----
  # Log Trace
  logger::log_trace(base::paste0("hawkinR -> Run: get_tests"))

  # 2. ----- Parameter Validation -----

  # Retrieve access token and expiration from environment variables
  aToken <- base::Sys.getenv("accessToken")

  #-----#

  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration"))

  #-----#

  # Check for Access Token and Expiration
  if (base::is.null(aToken) ||
      token_exp <= base::as.numeric(base::Sys.time())) {
    logger::log_error("hawkinR/get_tests -> Access token not available or expired. Call get_access() to obtain it.")
    stop("Access token not available or expired. Call get_access() to obtain it.")
  } else {
    # Log Debug
    logger::log_debug(
      base::paste0(
        "hawkinR/get_tests -> Temporary access token expires: ",
        base::as.POSIXct(token_exp)
      )
    )
  }

  #-----#

  # Validate Parameters
  ParamValidation(
    arg_from = from,
    arg_to = to,
    arg_athleteId = athleteId,
    arg_testTypeId = typeId,
    arg_teamId = teamId,
    arg_groupId = groupId
  )

  # 3. ----- Build URL Request -----

  # Initialize query list
  query <- list()

  # Add parameters to query if they are not NULL

  # All or Sync
  if (base::isTRUE(sync)) {
    # Sync From
    if (!base::is.null(from)) {
      query$syncFrom <- from
    }
    # Sync To
    if (!base::is.null(to)) {
      query$syncTo <- to
    }
  } else if (isFALSE(sync)) {
    # From
    if (!base::is.null(from)) {
      query$from <- from
    }
    # To
    if (!base::is.null(to)) {
      query$to <- to
    }
  }

  # Athlete ID
  if (!base::is.null(athleteId)) {
    query$athleteId <- athleteId
  }

  # Test Type ID
  if (!base::is.null(typeId)) {
    query$testTypeId <- TestIdCheck(arg_id = typeId)
  }

  # Team ID
  if (!base::is.null(teamId)) {
    query$teamId <- teamId
  }

  # Group ID
  if (!base::is.null(groupId)) {
    query$groupId <- groupId
  }

  #-----#

  # Build Request
  request <- httr2::request(base::Sys.getenv("urlRegion")) %>%
    # Add URL Path
    httr2::req_url_query(!!!query) %>%
    # Supply Bearer Authentication
    httr2::req_auth_bearer_token(token = aToken)

  # Log Debug
  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug(
    base::paste0(
      "hawkinR/get_tests -> ", reqPath$method, ": ", reqPath$headers$host, reqPath$path
    )
  )

  # Execute Call
  resp <- request %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  # Response Status
  status <- httr2::resp_status(resp = resp)

  # 4. ----- Create Response Outputs -----

  # Error Handler
  error_message <- NULL

  if (status == 401) {
    error_message <- 'Error 401: Refresh Token is invalid or expired.'
  } else if (status == 500) {
    error_message <-
      'Error 500: Something went wrong. Please contact support@hawkindynamics.com'
  }

  if (!base::is.null(error_message)) {
    stop(logger::log_error(base::paste0(
      "hawkinR/get_tests -> ", error_message
    )))
  }

  # Response Table
  if (status == 200) {
    # Response GOOD - Run rest of script
    # Convert JSON Response
    x <- httr2::resp_body_json(resp = resp,
                               check_type = TRUE,
                               simplifyVector = TRUE)

    # 5. ----- Sort Test Data -----
    ## Test returned count -----
    count <- x$count

    ## Create Test Data frame -----
    if (count > 0) {
      ### Get data frame from .data -----
      df <- x$data

      # Trial Info
      trialInfo <- df[1:3]

      # Trial Metrics
      trialMetrics <- df[6:ncol(df)]

      # Clean Trial Metric Names
      trialMetrics <- janitor::clean_names(trialMetrics)

      ### Test Type Data -----
      # Index Test Type Data
      TestTypeData <- df[[4]]

      # Reformat Test Type Data
      TestTypeData <- TestTypePrep(TestTypeData)

      ### Athlete Data -----
      # Index Athlete Data
      AthleteData <- df[[5]]

      # Reformat Athlete Data
      AthleteData <- AthletePrep(AthleteData)

      ### Test Meta Data -----
      # Last Synced
      lastSync <- x$lastSyncTime
      lastSyncDt <- lubridate::with_tz(lubridate::as_datetime(lastSync, tz = "UTC"),
                                       tzone = base::Sys.timezone())

      # Last Tested
      lastTest <- x$lastTestTime
      lastTestDt <- lubridate::with_tz(lubridate::as_datetime(lastTest, tz = "UTC"),
                                       tzone = base::Sys.timezone())

      # Build Output Test Data frame
      outputDF <-
        base::cbind(trialInfo, TestTypeData, AthleteData, trialMetrics)

      # Add Test Meta Data to output data frame
      outputDF <-
        dplyr::mutate(outputDF, last_test_time = lastTest, last_sync_time = lastSync)

      # Validate Active Tests
      if (base::isFALSE(includeInactive)) {
        # Output DF
        outputDF <- dplyr::filter(outputDF, outputDF$active == TRUE)
      }
    } else {
      # No Matching Groups
      stop(logger::log_warn(
        base::paste0(
          "hawkinR/get_tests -> Error: No tests returned meeting those query parameters"
        )
      ))
    }

    # 6. ----- Returns -----

    if (count > 0) {
      if (base::isFALSE(includeInactive)) {
        logger::log_success(
          base::paste0(
            "hawkinR/get_tests -> ",
            nrow(outputDF),
            " active tests returned. Last tested: ",
            lastTestDt,
            " | Last Synced: ",
            lastSyncDt
          )
        )
        return(outputDF)
      } else {
        logger::log_success(
          base::paste0(
            "hawkinR/get_tests -> ",
            count,
            " tests returned. Last tested: ",
            lastTestDt,
            " | Last Synced: ",
            lastSyncDt
          )
        )
        return(outputDF)
      }
    }
  }
}
