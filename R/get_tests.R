#' @include auth_system.R
NULL

#' Get All Tests or Sync Tests
#'
#' @description
#' Retrieves test data from the Hawkin Dynamics API. This function replaces all previous
#' `get_tests_*` functions. It can filter by athlete, team, group, date range, or sync timestamp.
#'
#' @details
#' **Smart Chunking:**
#' This function splits requests into chunks (default 60 days) to prevent API timeouts
#' and manage load. You can adjust the `chunk_size` argument if you encounter timeouts.
#'
#' * If `from` is provided, it serves as the start of the first chunk.
#' * If `from` is NOT provided, the function looks for a default `start_date` in the connection
#'   profile. If none exists, it will pause and ask you to enter one.
#' * If `to` is not provided, it defaults to the current date.
#'
#' **Schema Handling:**
#' Because metrics may be added to the system over time, older test results might have fewer
#' columns than newer ones. This function uses `dplyr::bind_rows` to combine chunks, filling
#' missing columns with `NA` where necessary.
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
#'  includeInactive = FALSE,
#'  ...
#'  )
#'
#' @param from Optionally supply a time frame **start** value. Accepts either:
#' - A Unix timestamp as an `integer` (e.g., `1689958617`), or
#' - A date as a `character` string in `"YYYY-MM-DD"` format (e.g., `"2023-08-01"`).
#'
#' If not supplied, you will be prompted to enter a start date (or use the profile default).
#'
#' @param to Optionally supply a time frame **end** value. Accepts either:
#' - A Unix timestamp as an `integer` (e.g., `1691207356`), or
#' - A date as a `character` string in `"YYYY-MM-DD"` format (e.g., `"2023-08-10"`).
#'
#' If not supplied, defaults to the current date.
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
#' @param ... Optional arguments.
#' \itemize{
#'   \item `profile`: A `HawkinAuth` object or a character string of the profile name.
#'   If not provided, the active connection is used.
#'   \item `chunk_size`: Integer. The number of days to fetch per API request. Defaults to 60.
#'   Reduce this number (e.g., to 7 or 14) if you experience timeouts.
#' }
#'
#' @return
#' A data frame containing test trials and their metrics. Each row represents a single test trial.
#' (if specified).
#'
#' @examples
#' \dontrun{
#' # Call for all tests (active connection)
#' dfAllTests <- get_tests()
#'
#' # Adjust chunk size for slower connections
#' dfSmallerChunks <- get_tests(chunk_size = 7)
#'
#' # Call for all tests within a specific time frame
#' dfFromTo <- get_tests(from = "2023-08-01", to = "2023-08-10")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 request req_url_query req_auth_bearer_token req_headers req_error req_perform req_url_path_append
#' @importFrom httr2 resp_status resp_body_json
#' @importFrom rlang .data
#' @importFrom dplyr filter bind_rows
#' @importFrom lubridate as_datetime with_tz ymd days
#' @importFrom janitor clean_names
#' @importFrom logger log_info log_trace log_debug log_success log_warn
#' @importFrom progress progress_bar
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
                      includeInactive = FALSE,
                      ...) {

  # 1. ----- Resolve Arguments & Connection -----
  extra_args <- list(...)

  # Resolve chunk_size (Default to 60 if not provided)
  chunk_size <- if (!is.null(extra_args$chunk_size)) extra_args$chunk_size else 60

  # Resolve Profile/Connection
  if (!is.null(extra_args$profile)) {
    if (is.character(extra_args$profile)) {
      conn <- hd_connect(profile = extra_args$profile)
    } else {
      conn <- extra_args$profile
    }
  } else {
    conn <- get_active_conn()
  }

  logger::log_trace("hawkinR -> Run: get_tests (chunk_size: {chunk_size})")

  # Validate connection object
  if (!is.object(conn) || is.null(conn@access_token)) {
    stop("A valid HawkinAuth connection is required. Run hd_connect() first.", call. = FALSE)
  }

  # 2. ----- Time Window Logic (Chunking) -----
  # We ALWAYS calculate chunking to ensure stability.

  # A. Determine Start Date
  start_dt <- NULL

  if (!is.null(from)) {
    # If provided, try to parse it
    start_dt <- tryCatch(lubridate::ymd(from), error = function(e) NA)
    # If parsing failed, maybe it's a numeric timestamp? Convert to date for chunking logic
    if (is.na(start_dt) && is.numeric(from)) start_dt <- as.Date(as.POSIXct(from, origin = "1970-01-01"))
  }

  if (is.null(start_dt) || is.na(start_dt)) {
    # Fallback 1: Config
    config_start <- conn@config@start_date
    if (!is.null(config_start)) {
      start_dt <- tryCatch(lubridate::ymd(config_start), error = function(e) NA)
    }

    # Fallback 2: Prompt User
    if (is.null(start_dt) || is.na(start_dt)) {
      if (interactive()) {
        message("\n[NOTE] 'from' date is required to fetch tests.")
        start_input <- readline(prompt = "Enter Start Date (YYYY-MM-DD): ")
        if (start_input == "") stop("Operation cancelled by user.", call. = FALSE)
        start_dt <- tryCatch(lubridate::ymd(start_input), error = function(e) NA)
      } else {
        stop("get_tests() requires a 'from' date. In non-interactive mode, set 'start_date' in hd_connect().", call. = FALSE)
      }
    }
  }

  if (is.na(start_dt)) stop("Invalid Start Date format.", call. = FALSE)

  # B. Determine End Date
  end_dt <- Sys.Date()
  if (!is.null(to)) {
    parsed_to <- tryCatch(lubridate::ymd(to), error = function(e) NA)
    if (!is.na(parsed_to)) end_dt <- parsed_to
    # Handle numeric timestamp input for 'to'
    if (is.na(parsed_to) && is.numeric(to)) end_dt <- as.Date(as.POSIXct(to, origin = "1970-01-01"))
  }

  # C. Generate Chunks
  # We create a sequence using the customizable chunk_size
  by_str <- paste(chunk_size, "days")
  chunk_starts <- seq(start_dt, end_dt, by = by_str)
  time_windows <- list()

  for (d in seq_along(chunk_starts)) {
    w_start <- chunk_starts[d]
    w_end   <- min(w_start + lubridate::days(chunk_size), end_dt)

    # If fetching just 1 day or last chunk is small, ensure start <= end
    if (w_start > w_end) w_end <- w_start

    time_windows[[d]] <- list(start = w_start, end = w_end)
  }

  logger::log_info("hawkinR/get_tests -> Chunking request into {length(time_windows)} periods of {chunk_size} days...")

  # 3. ----- Main Processing Loop -----
  all_results <- list()

  # Initialize progress bar
  pb <- progress::progress_bar$new(
    format = "  Fetching Data [:bar] :percent | :current/:total chunks",
    total = length(time_windows), clear = FALSE, width = 60
  )

  for (i in seq_along(time_windows)) {
    pb$tick()

    # Extract current window dates
    current_from <- time_windows[[i]]$start
    current_to   <- time_windows[[i]]$end

    # 3a. Token Check
    token_remaining <- round(as.numeric(difftime(conn@expires_at, Sys.time(), units = "secs")))
    if (token_remaining < 60) {
      logger::log_info("hawkinR/get_tests -> Token expiring soon. Refreshing...")
      conn <- authenticate(conn)
      set_active_conn(conn)
    }

    # 3b. Validate & Build Params
    params <- list()

    # Helper: Convert ANY date input to Numeric Epoch String
    to_epoch <- function(d) {
      if (is.null(d)) return(NULL)
      if (inherits(d, c("Date", "POSIXt"))) {
        # Force to UTC midnight for consistency
        return(as.character(as.numeric(as.POSIXct(format(d), tz = "UTC"))))
      }
      return(as.character(d))
    }

    if (isTRUE(sync)) {
      params$syncFrom <- to_epoch(current_from)
      params$syncTo   <- to_epoch(current_to)
    } else {
      params$from <- to_epoch(current_from)
      params$to   <- to_epoch(current_to)
    }

    if (!is.null(athleteId)) params$athleteId <- athleteId
    if (!is.null(typeId))    params$testTypeId <- TestIdCheck(typeId)
    if (!is.null(teamId))    params$teamId <- teamId
    if (!is.null(groupId))   params$groupId <- groupId
    if (isTRUE(includeInactive)) params$includeInactive <- "true"

    # 3c. Build Request (base_url/org_id)
    req_url <- paste0(conn@base_url, "/", conn@config@org_id)

    request <- httr2::request(req_url)

    if (length(params) > 0) {
      request <- request |> httr2::req_url_query(!!!params)
    }

    # 3d. Execute Request
    tryCatch({
      resp <- request |>
        httr2::req_auth_bearer_token(conn@access_token) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()

      status <- httr2::resp_status(resp)

      if (status == 200) {
        body <- httr2::resp_body_json(resp, simplifyVector = TRUE, check_type = TRUE)

        if (body$count > 0) {
          # Process Data
          df <- body$data
          trialInfo    <- df[1:3]
          trialMetrics <- janitor::clean_names(df[6:ncol(df)])
          TestTypeData <- TestTypePrep(df[[4]])
          AthleteData  <- AthletePrep(df[[5]])

          # Combine
          chunk_res <- cbind(trialInfo, TestTypeData, AthleteData, trialMetrics)

          # Add timestamps
          chunk_res$last_test_time <- body$lastTestTime
          chunk_res$last_sync_time <- body$lastSyncTime

          # Filter Inactive if needed
          if (isFALSE(includeInactive)) {
            chunk_res <- dplyr::filter(chunk_res, .data$active == TRUE)
          }

          all_results[[i]] <- chunk_res
        }
      } else {
        logger::log_warn("hawkinR/get_tests -> API Error {status} for chunk {i}")
      }
    }, error = function(e) {
      logger::log_error("hawkinR/get_tests -> Request failed for chunk {i}: {e$message}")
    })
  }

  # 4. ----- Finalize Output -----
  if (length(all_results) == 0) {
    logger::log_warn("hawkinR/get_tests -> No tests returned.")
    return(data.frame())
  }

  # Sanitize chunks to prevent logical/list conflicts
  all_results <- sanitize_chunks(all_results)

  # Robust Bind: Use bind_rows to handle schema mismatch (e.g. new metrics)
  final_df <- dplyr::bind_rows(all_results)

  # Deduplicate
  final_df <- unique(final_df)

  logger::log_success("hawkinR/get_tests -> {nrow(final_df)} total tests returned.")
  return(final_df)
}
