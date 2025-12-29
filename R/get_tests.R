#' Get All Tests or Sync Tests
#'
#' @description
#' Retrieves test data from the Hawkin Dynamics API. This function replaces all previous
#' `get_tests_*` functions. It can filter by athlete, team, group, date range, or sync timestamp.
#'
#' @usage
#' get_tests(
#'  x = NULL,
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
#' @param x Optional. A `HawkinAuth` object. If NULL, the active connection is used.
#'
#' @param from Optionally supply a time frame **start** value. Accepts either:
#' - A Unix timestamp as an `integer` (e.g., `1689958617`), or
#' - A date as a `character` string in `"YYYY-MM-DD"` format (e.g., `"2023-08-01"`).
#'
#' If not supplied, all available tests from the earliest record will be returned.
#' Use this parameter for bulk exports or to define a starting point for data retrieval.
#'
#' @param to Optionally supply a time frame **end** value. Accepts either:
#' - A Unix timestamp as an `integer` (e.g., `1691207356`), or
#' - A date as a `character` string in `"YYYY-MM-DD"` format (e.g., `"2023-08-10"`).
#'
#' If not supplied, all available tests up to the latest record will be returned,
#' or up to the `from` parameter if specified. Use this parameter to limit the
#' range of historical data retrieved.
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
#' A data frame containing test trials and their metrics. Each row represents a single test trial.
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
#' # Call for all tests
#'
#' dfAllTests <- get_tests()
#'
#'
#' # Call for all tests within a specific time frame
#'
#' dfFromTo <- get_tests(from = 1689958617, to = 1691207356)
#'
#'
#' # Call for all new tests since a specific date, or any tests that have been
#' updated/changed since that date
#'
#' dfSync <- get_tests(from = 1689958617, sync=TRUE)
#'
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 request req_url_query req_auth_bearer_token req_headers req_error req_perform
#' @importFrom httr2 resp_status resp_body_json
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom lubridate as_datetime with_tz
#' @importFrom janitor clean_names
#' @importFrom logger log_info log_trace log_debug log_success log_warn
#'
#' @export


## Get All Tests or Sync Tests -----
get_tests <- function(x = NULL,
                      from = NULL,
                      to = NULL,
                      sync = FALSE,
                      athleteId = NULL,
                      typeId = NULL,
                      teamId = NULL,
                      groupId = NULL,
                      includeInactive = FALSE) {

  # 1. ----- Set Logger -----
  logger::log_trace("hawkinR -> Run: get_tests")

  # 2. ----- Resolve Connection -----
  logger::log_trace("hawkinR/get_tests -> Resolving connection")
  if (missing(x) || is.null(x)) {
    x <- get_active_conn()
  }

  # Validate connection object
  if (!is.object(x) || is.null(x@access_token)) {
    stop("A valid HawkinAuth connection is required. Run hd_connect() first.", call. = FALSE)
  }

  # 3. ----- Token Lifecycle Management -----
  token_remaining <- round(as.numeric(difftime(x@expires_at, Sys.time(), units = "secs")))
  logger::log_debug("hawkinR/get_tests -> Token expires in {token_remaining} seconds")
  if (token_remaining < 300) {
    logger::log_info("hawkinR/get_tests -> Token expiring soon. Refreshing...")
    x <- authenticate(x)
    set_active_conn(x)
  }

  # 4. ----- Validate Parameters -----
  logger::log_trace("hawkinR/get_tests -> Validating query parameters")
  params <- list()

  ParamValidation(arg_athleteId = athleteId,
                  arg_testTypeId = typeId,
                  arg_teamId = teamId,
                  arg_groupId = groupId)

  logger::log_trace("hawkinR/get_tests -> Params validated: sync={sync}, inactive={includeInactive}")

  # Process Dates (Using internal helper or simple checks)
  process_date <- function(d) {
    if (is.null(d)) return(NULL)
    if (is.character(d) && grepl("^\\d{4}-\\d{2}-\\d{2}$", d)) {
      return(as.numeric(as.POSIXct(d)))
    }
    return(d)
  }

  # Date Parameters
  if (isTRUE(sync)) {
    if (!is.null(from)) {
      params$syncFrom <- process_date(from)
    }

    if (!is.null(to)) {
      params$syncTo   <- process_date(to)
    }
  } else {
    if (!is.null(from)) {
      params$from <- process_date(from)
    }

    if (!is.null(to)) {
      params$to   <- process_date(to)
    }
  }

  # athleteId Parameter
  if (!is.null(athleteId)) {
    params$athleteId <- athleteId
  }

  # typeId Parameters
  if (!is.null(typeId)) {
    params$testTypeId    <- TestIdCheck(typeId)
  }

  # teamId Parameters
  if (!is.null(teamId)) {
    params$teamId    <- teamId
  }
  # groupId Parameters
  if (!is.null(groupId)) {
    params$groupId   <- groupId
  }

  logger::log_debug("hawkinR/get_tests -> Query params: {jsonlite::toJSON(params, auto_unbox=T)}")

  # 5. ----- Build Request -----
  logger::log_trace("hawkinR/get_tests -> Building request")
  request <- httr2::request(paste0(x@base_url, "/", x@config@org_id))

  # Add query parameters if they exist
  if (length(params) > 0) {
    request <- request |> httr2::req_url_query(!!!params)
  }

  # Capture the dry run
  req_preview <- httr2::req_dry_run(request, quiet = TRUE)

  # CORRECTLY reconstruct the query string from the list
  query_list <- req_preview$query
  if (length(query_list) > 0) {
    # This turns list(a=1, b=2) into "a=1&b=2"
    query_str <- paste(names(query_list), query_list, sep = "=", collapse = "&")
    full_url  <- paste0(req_preview$headers$host, req_preview$path, "?", query_str)
  } else {
    full_url  <- paste0(req_preview$headers$host, req_preview$path)
  }

  logger::log_debug("hawkinR/get_tests -> {req_preview$method}: {full_url}")

  # 6. ----- Execute Call -----
  logger::log_trace("hawkinR/get_tests -> Executing API request")
  resp <- request |>
    httr2::req_auth_bearer_token(x@access_token) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  logger::log_debug("hawkinR/get_tests -> Response status: {status}")

  # 7. ----- Error Handling -----
  if (status == 401) {
    logger::log_error("hawkinR/get_tests -> Error 401: Token invalid")
    stop("Unauthorized (401). Token invalid.", call. = FALSE)
  }
  if (status == 500) {
    logger::log_error("hawkinR/get_tests -> Error 500: Server error")
    stop("Server Error (500). Contact Support.", call. = FALSE)
  }
  if (status == 404) {
    logger::log_error("hawkinR/get_tests -> Error 404: Org ID '{x@config@org_id}' not found")
    stop(sprintf("Not Found (404). Check your Org ID: '%s'", x@config@org_id), call. = FALSE)
  }
  if (status >= 400) {
    logger::log_error("hawkinR/get_tests -> API Error: {status}")
    stop(paste("API Error:", status), call. = FALSE)
  }

  # 8. ----- Parse Response -----
  logger::log_trace("hawkinR/get_tests -> Parsing JSON response")
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE, check_type = TRUE)

  count <- body$count
  logger::log_debug("hawkinR/get_tests -> API returned {count} raw tests")

  if (count > 0) {
    # Get dataframe from .data
    df <- body$data

    # 9. ----- Process Trial Data -----
    logger::log_trace("hawkinR/get_tests -> Slicing trial info and metrics")
    trialInfo    <- df[1:3]
    trialMetrics <- df[6:ncol(df)]

    # Clean Metrics Names
    logger::log_trace("hawkinR/get_tests -> Cleaning metric column names")
    trialMetrics <- janitor::clean_names(trialMetrics)

    # 10. ----- Process Test Type & Athlete Data -----
    logger::log_trace("hawkinR/get_tests -> Processing test type data")
    TestTypeData <- TestTypePrep(df[[4]])

    logger::log_trace("hawkinR/get_tests -> Processing athlete data")
    AthleteData  <- AthletePrep(df[[5]])

    # 11. ----- Handle Timestamps -----
    logger::log_trace("hawkinR/get_tests -> Extracting timestamp metadata")
    lastSync   <- body$lastSyncTime
    lastSyncDt <- lubridate::with_tz(
      lubridate::as_datetime(lastSync, tz = "UTC"),
      tzone = Sys.timezone()
    )

    lastTest   <- body$lastTestTime
    lastTestDt <- lubridate::with_tz(
      lubridate::as_datetime(lastTest, tz = "UTC"),
      tzone = Sys.timezone()
    )
    logger::log_debug("hawkinR/get_tests -> Last sync: {lastSyncDt}, Last test: {lastTestDt}")

    # 12. ----- Build Final Output -----
    logger::log_trace("hawkinR/get_tests -> Combining data frames")
    outputDF <- cbind(trialInfo, TestTypeData, AthleteData, trialMetrics)
    outputDF <- dplyr::mutate(outputDF, last_test_time = lastTest, last_sync_time = lastSync)

    # Filter Active Tests
    if (isFALSE(includeInactive)) {
      pre_filter_count <- nrow(outputDF)
      outputDF <- dplyr::filter(outputDF, .data$active == TRUE)
      logger::log_trace("hawkinR/get_tests -> Filtered inactive: {pre_filter_count} -> {nrow(outputDF)} tests")
    }

    logger::log_trace("hawkinR/get_tests -> Final data frame: {nrow(outputDF)} rows, {ncol(outputDF)} columns")
    logger::log_success("hawkinR/get_tests -> {nrow(outputDF)} tests returned. Last tested: {lastTestDt}")
    return(outputDF)

  } else {
    logger::log_warn("hawkinR/get_tests -> No tests returned meeting query parameters")
    return(data.frame())
  }
}
