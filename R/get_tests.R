#' @include auth_system.R
NULL

#' Get All Tests or Sync Tests
#'
#' @description
#' Retrieves test data from the Hawkin Dynamics API. This function replaces all previous
#' `get_tests_*` functions. It can filter by athlete, team, group, date range, or sync timestamp.
#'
#' @details
#' **Pagination:**
#' This function uses cursor-based pagination (API v1.13+) to reliably fetch all matching
#' tests regardless of dataset size. The API returns pages of up to 1,000 tests each.
#' The function loops automatically until all pages are retrieved.
#'
#' The result is all-or-nothing. Transient failures (HTTP 429, 502, 503, 504) are
#' retried with backoff. If any page still fails, the function stops with an error
#' naming the page rather than returning the pages fetched so far.
#'
#' * If `from` is provided, it is used as a time-range filter.
#' * If `from` is NOT provided, no lower bound is applied and all available
#'   history is returned. Pagination handles arbitrarily large result sets.
#' * If `to` is NOT provided, no upper bound is applied (results run through the
#'   most recent test).
#'
#' **Schema Handling:**
#' Because metrics may be added to the system over time, older test results might have fewer
#' columns than newer ones. This function uses `dplyr::bind_rows` to combine pages, filling
#' missing columns with `NA` where necessary.
#'
#' **Athlete columns:**
#' The athlete block is prefixed with `athlete_` and includes the core fields
#' (`athlete_id`, `athlete_name`, `athlete_active`, `athlete_teams`, `athlete_groups`),
#' the API v1.14 profile fields when present (`athlete_image`, `athlete_position`,
#' `athlete_dob`, `athlete_sport`, `athlete_height`, `athlete_lastTestedOn`), and one
#' `athlete_<key>` column per external (custom) property. Athletes missing a given
#' external key receive `NA`.
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
#'  includeEid = FALSE,
#'  useNulls = TRUE,
#'  rounding = FALSE,
#'  nestMetrics = FALSE,
#'  ...
#'  )
#'
#' @param from Optionally supply a time frame **start** value. Accepts either:
#' - A Unix timestamp as an `integer` (e.g., `1689958617`), or
#' - A date as a `character` string in `"YYYY-MM-DD"` format (e.g., `"2023-08-01"`).
#'
#' Optional. If not supplied, no lower bound is applied and all available history is returned.
#'
#' @param to Optionally supply a time frame **end** value. Accepts either:
#' - A Unix timestamp as an `integer` (e.g., `1691207356`), or
#' - A date as a `character` string in `"YYYY-MM-DD"` format (e.g., `"2023-08-10"`).
#'
#' Optional. If not supplied, no upper bound is applied (results run through the most recent test).
#'
#' @param sync The result set will include updated and newly created tests. This parameter
#' is best suited to keep your database in sync with the Hawkin database. If you do not
#' supply this value you will receive every test.
#'
#' @param includeInactive Logical. Default `FALSE`, which sends `includeInactive=false` to
#' the API so that only active tests are returned server-side. Set to `TRUE` to include
#' inactive (disabled) trials in the response.
#'
#' @param includeEid Logical. Default `FALSE`. When `TRUE`, the API includes an
#' `eid` (equipment ID) field on each test record identifying the hardware that
#' produced the trial. The column will be `NA` for any tests where the equipment
#' ID is not recorded.
#'
#' @param useNulls Logical. Default `TRUE`: non-calculable metrics come back as `NA`.
#' Set to `FALSE` to receive the string `"N/A"` instead (API v1.16). Any metric
#' column containing `"N/A"` becomes character. Intended for customers migrating
#' from a legacy named endpoint; leave the default otherwise.
#'
#' @param rounding Logical. Default `FALSE`: raw metric values. Set to `TRUE` to have
#' the API round each metric to its standard display precision (API v1.16). Has no
#' effect when `nestMetrics = TRUE`. Intended for customers migrating from a legacy
#' named endpoint; leave the default otherwise.
#'
#' @param nestMetrics Logical. Default `FALSE`: one row per test with a column per
#' metric. Set to `TRUE` to receive a long table instead: one row per test and
#' metric, with `metric_id`, `metric_label`, `metric_units` and `metric_value`
#' columns beside the trial, athlete and test-type columns (API v1.16). Only
#' metrics with a numeric value are included; a test with none keeps a single row
#' with `NA` metric fields. Intended for customers migrating from a legacy named
#' endpoint; leave the default otherwise.
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
#'   \item `chunk_size`: \strong{Deprecated.} Previously controlled time-window chunking.
#'   Now ignored; the API uses cursor-based pagination with a fixed page size of 1,000.
#' }
#'
#' @return
#' A data frame containing test trials and their metrics. Each row represents a single test trial,
#' or a single test-and-metric pair when `nestMetrics = TRUE`.
#'
#' @examples
#' \dontrun{
#' # Call for all tests (active connection)
#' dfAllTests <- get_tests()
#'
#' # Call for all tests within a specific time frame
#' dfFromTo <- get_tests(from = "2023-08-01", to = "2023-08-10")
#'
#' # Include inactive tests
#' dfAll <- get_tests(from = "2023-08-01", includeInactive = TRUE)
#'
#' # Reproduce a legacy named endpoint's payload: rounded values, "N/A" strings
#' dfLegacy <- get_tests(from = "2023-08-01", rounding = TRUE, useNulls = FALSE)
#'
#' # Long table: one row per test and metric
#' dfLong <- get_tests(from = "2023-08-01", nestMetrics = TRUE)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 request req_url_query req_auth_bearer_token req_headers req_error req_perform req_url_path_append
#' @importFrom httr2 req_retry resp_status resp_body_json
#' @importFrom rlang .data
#' @importFrom dplyr filter bind_rows
#' @importFrom lubridate as_datetime with_tz ymd days
#' @importFrom janitor clean_names
#' @importFrom logger log_info log_trace log_debug log_success log_warn log_error
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
                      includeEid = FALSE,
                      useNulls = TRUE,
                      rounding = FALSE,
                      nestMetrics = FALSE,
                      ...) {

  # 1. ----- Resolve Arguments & Connection -----
  extra_args <- list(...)

  # Deprecated: chunk_size (now using cursor-based pagination)
  if (!is.null(extra_args$chunk_size)) {
    logger::log_warn("hawkinR/get_tests -> 'chunk_size' is deprecated and ignored. The API now uses cursor-based pagination.")
  }

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

  logger::log_trace("hawkinR -> Run: get_tests")

  # Validate connection object
  if (!is.object(conn) || is.null(conn@access_token)) {
    stop("A valid HawkinAuth connection is required. Run hd_connect() first.", call. = FALSE)
  }

  # 2. ----- Resolve Date Parameters -----

  # Helper: Convert ANY date input to Numeric Epoch String
  to_epoch <- function(d) {
    if (is.null(d)) return(NULL)
    if (inherits(d, c("Date", "POSIXt"))) {
      return(as.character(as.numeric(as.POSIXct(format(d), tz = "UTC"))))
    }
    return(as.character(d))
  }

  # A. Determine Start Date (optional). Cursor-based pagination means a 'from'
  # date is no longer required — omitting it returns all available history.
  from_epoch <- NULL

  if (!is.null(from)) {
    start_dt <- tryCatch(suppressWarnings(lubridate::ymd(from)), error = function(e) NA)
    if (is.na(start_dt) && is.numeric(from)) start_dt <- as.Date(as.POSIXct(from, origin = "1970-01-01"))
    if (is.na(start_dt)) {
      stop("Invalid 'from' value. Use a 'YYYY-MM-DD' string or a Unix timestamp.", call. = FALSE)
    }
    from_epoch <- to_epoch(start_dt)
  }

  # B. Determine End Date (optional). Omitting it returns results through the
  # most recent test rather than imposing an upper bound.
  to_epoch_val <- NULL
  if (!is.null(to)) {
    parsed_to <- tryCatch(suppressWarnings(lubridate::ymd(to)), error = function(e) NA)
    if (!is.na(parsed_to)) {
      to_epoch_val <- to_epoch(parsed_to)
    } else if (is.numeric(to)) {
      to_epoch_val <- to_epoch(as.Date(as.POSIXct(to, origin = "1970-01-01")))
    } else {
      stop("Invalid 'to' value. Use a 'YYYY-MM-DD' string or a Unix timestamp.", call. = FALSE)
    }
  }

  # 3. ----- Build Query Parameters -----
  params <- list(paginate = "true")

  if (isTRUE(sync)) {
    if (!is.null(from_epoch))   params$syncFrom <- from_epoch
    if (!is.null(to_epoch_val)) params$syncTo   <- to_epoch_val
  } else {
    if (!is.null(from_epoch))   params$from <- from_epoch
    if (!is.null(to_epoch_val)) params$to   <- to_epoch_val
  }

  if (!is.null(athleteId)) params$athleteId <- athleteId
  if (!is.null(typeId))    params$testTypeId <- TestIdCheck(typeId)
  # Per API spec: teamId / groupId accept a single ID OR a comma-separated
  # list (max 10). Vectors must be collapsed — httr2::req_url_query rejects
  # non-scalar list values.
  if (!is.null(teamId))    params$teamId  <- paste(teamId,  collapse = ",")
  if (!is.null(groupId))   params$groupId <- paste(groupId, collapse = ",")

  # Server-side includeInactive filtering (API v1.13+)
  if (isFALSE(includeInactive)) params$includeInactive <- "false"

  # Optional: ask the API to include the equipment ID on each record
  if (isTRUE(includeEid)) params$includeEid <- "true"

  # Response-shape overrides (API v1.16). Only the non-default value is sent so
  # existing callers produce byte-identical requests.
  if (isFALSE(useNulls))   params$useNulls    <- "false"
  if (isTRUE(rounding))    params$rounding    <- "true"
  if (isTRUE(nestMetrics)) params$nestMetrics <- "true"

  logger::log_info("hawkinR/get_tests -> Fetching tests with pagination...")

  # 4. ----- Pagination Loop -----
  all_results <- list()
  page_count <- 0
  cursor <- NULL

  # Every page failure is fatal: returning the pages fetched so far would
  # silently truncate the export. Past page 1, say why those pages are dropped.
  stop_page <- function(error_message) {
    if (page_count > 1) {
      error_message <- paste0(
        error_message, ". No results returned because the export would be incomplete."
      )
    }
    logger::log_error("hawkinR/get_tests -> {error_message}")
    stop(error_message, call. = FALSE)
  }

  repeat {
    page_count <- page_count + 1

    # Token refresh check (300s threshold)
    token_remaining <- token_seconds_remaining(conn)
    if (token_remaining < 300) {
      logger::log_info("hawkinR/get_tests -> Token expiring soon. Refreshing...")
      conn <- authenticate(conn)
      set_active_conn(conn)
    }

    # Add cursor for subsequent pages
    current_params <- params
    if (!is.null(cursor)) current_params$cursor <- cursor

    # Build Request
    req_url <- paste0(conn@base_url, "/", conn@config@org_id)
    request <- hd_request(req_url)
    if (length(current_params) > 0) {
      request <- request |> httr2::req_url_query(!!!current_params)
    }

    # Execute Request. Transient statuses (rate limit, gateway errors) are
    # retried with backoff; any failure that survives the retries is fatal.
    resp <- tryCatch({
      request |>
        httr2::req_auth_bearer_token(conn@access_token) |>
        httr2::req_retry(
          max_tries = 3,
          is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 502, 503, 504)
        ) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()
    }, error = function(e) {
      stop_page(paste0("Request failed on page ", page_count, ": ", conditionMessage(e)))
    })

    status <- httr2::resp_status(resp)

    if (status == 200) {
      body <- tryCatch(
        httr2::resp_body_json(resp, simplifyVector = TRUE, check_type = TRUE),
        error = function(e) {
          stop_page(paste0(
            "Could not parse the response on page ", page_count, ": ", conditionMessage(e)
          ))
        }
      )

      if (body$count > 0) {
        # Process Data — select by name, not by fixed index. Different
        # filter types (e.g. teamId) can return responses with fewer or
        # reordered columns, which would break `df[1:3]` / `df[[4]]` /
        # `df[6:ncol(df)]`.
        df <- body$data

        trial_info_cols <- intersect(c("id", "timestamp", "segment"), names(df))
        trialInfo <- df[, trial_info_cols, drop = FALSE]

        TestTypeData <- if ("testType" %in% names(df)) {
          TestTypePrep(df$testType)
        } else {
          data.frame(row.names = seq_len(nrow(df)))
        }

        AthleteData <- if ("athlete" %in% names(df)) {
          AthletePrep(df$athlete)
        } else {
          data.frame(row.names = seq_len(nrow(df)))
        }

        # Everything not a known structural column is a metric. `metrics` is the
        # nested array returned for nestMetrics = TRUE and is expanded below.
        structural <- c("id", "timestamp", "segment", "testType", "athlete", "metrics")
        metric_cols <- setdiff(names(df), structural)
        trialMetrics <- if (length(metric_cols) > 0) {
          janitor::clean_names(df[, metric_cols, drop = FALSE])
        } else {
          data.frame(row.names = seq_len(nrow(df)))
        }

        # Combine
        page_res <- cbind(trialInfo, TestTypeData, AthleteData, trialMetrics)

        # nestMetrics = TRUE (API v1.16): one row per test and metric
        if ("metrics" %in% names(df)) {
          page_res <- expand_nested_metrics(page_res, df$metrics)
        }

        # Add timestamps
        page_res$last_test_time <- body$lastTestTime
        page_res$last_sync_time <- body$lastSyncTime

        all_results[[page_count]] <- page_res
      }

      logger::log_trace("hawkinR/get_tests -> Page {page_count}: {body$count} tests")

      # Cursor handling — loop until the API stops returning a cursor.
      next_cursor <- body$nextCursor
      if (is.null(next_cursor)) break

      # Safeguard: if the cursor fails to advance, error rather than loop
      # forever on a malformed response or return a truncated result.
      if (!is.null(cursor) && identical(next_cursor, cursor)) {
        stop_page(paste0("Pagination cursor did not advance on page ", page_count))
      }
      cursor <- next_cursor

    } else {
      error_message <- if (status == 401) {
        "Error 401: Refresh Token is invalid or expired."
      } else if (status == 500) {
        "Error 500: Something went wrong. Please contact dev-team@hawkindynamics.com"
      } else {
        paste0("Unexpected HTTP status: ", status)
      }
      stop_page(paste0(error_message, " (page ", page_count, ")"))
    }
  }

  # 5. ----- Finalize Output -----
  if (length(all_results) == 0) {
    logger::log_warn("hawkinR/get_tests -> No tests returned.")
    return(data.frame())
  }

  # Sanitize pages to prevent logical/list conflicts
  all_results <- sanitize_chunks(all_results)

  # Robust Bind: Use bind_rows to handle schema mismatch (e.g. new metrics)
  final_df <- dplyr::bind_rows(all_results)

  # Deduplicate
  final_df <- unique(final_df)

  logger::log_success("hawkinR/get_tests -> {nrow(final_df)} total tests returned across {page_count} pages.")
  return(final_df)
}
