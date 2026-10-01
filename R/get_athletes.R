#' Get Athletes
#'
#' @description
#' Get the athletes for an account. Inactive players will only be included if
#' `includeInactive` parameter is set to TRUE.
#'
#' @usage
#' get_athletes(includeInactive = FALSE, ...)
#'
#' @param includeInactive FALSE by default to exclude inactive players in database. Set to TRUE if you want
#' inactive players included in the return.
#'
#' @param ... Optional arguments.
#' \itemize{
#'   \item `profile`: A `HawkinAuth` object. If not provided, the active connection is used.
#' }
#'
#' @return
#' Response will be a data frame containing the athletes that match this query.
#' Each athlete includes the following variables:
#'
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **id** | *chr* | athlete's unique ID |
#' | **name** | *chr* | athlete's given name (First Last) |
#' | **active** | *bool* | athlete is active (TRUE) |
#' | **teams** | *chr* | team ids separated by "," |
#' | **groups** | *chr* | group ids separated by "," |
#' | **image** | *chr* | URL to the athlete's profile image. `NA` when never set or cleared. |
#' | **position** | *chr* | Free-text playing position (e.g. "Forward"). `NA` when blank. |
#' | **dob** | *chr* | Date of birth as an ISO-8601 date string (`YYYY-MM-DD`). `NA` when blank. |
#' | **sport** | *chr* | Free-text sport name (e.g. "Basketball"). `NA` when blank. |
#' | **height** | *num* | Athlete height in centimeters when present. `NA` otherwise. |
#' | **lastTestedOn** | *num* | Unix epoch **seconds** of the athlete's most recent test session. `NA` when no tests on file. |
#' | **external** | *chr* | external properties will have a column of their name with the appropriate values for the athlete of `NA` if it does not apply |
#'
#' The optional profile columns (image, position, dob, sport, height, lastTestedOn)
#' and any external property columns only appear when at least one athlete in the
#' response has that field populated.
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called. If you only wish to call active players,
#' # you don't need to provide any parameters.
#'
#' df_athletes <- get_athletes()
#'
#' # If you want to include all athletes, including inactive athletes, include the optional
#' # `includeInactive` parameter.
#'
#' df_wInactive <- get_athletes( includeInactive = TRUE)
#'
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 req_url_query req_auth_bearer_token req_error req_perform resp_status resp_body_json
#' @importFrom logger log_trace log_debug log_success log_error
#'
#' @export


# Get Athletes -----
get_athletes <- function(includeInactive = FALSE, ...) {

  # 1. ----- Set Logger -----
  logger::log_trace("hawkinR -> Run: get_athletes")

  # 2. ----- Authentication -----
  logger::log_trace("hawkinR/get_athletes -> Resolving connection")
  extra_args <- list(...)

  if (!is.null(extra_args$profile)) {
    if (is.character(extra_args$profile)) {
      # User passed a name string, so we connect
      conn <- hd_connect(profile = extra_args$profile)
    } else {
      # User passed the object directly
      conn <- extra_args$profile
    }
  } else {
    conn <- get_active_conn()
  }

  # Validate
  if (!is.object(conn) || is.null(conn@access_token)) {
    stop("A valid HawkinAuth connection is required. Run hd_connect() first.", call. = FALSE)
  }

  # Token Lifecycle Management
  token_remaining <- token_seconds_remaining(conn)
  logger::log_debug("hawkinR/get_athletes -> Token expires in {token_remaining} seconds")
  if (token_remaining < 300) {
    logger::log_info("hawkinR/get_athletes -> Token expiring soon. Refreshing...")
    conn <- authenticate(conn)
    set_active_conn(conn)
  }

  # 3. ----- Build URL Request -----
  logger::log_trace("hawkinR/get_athletes -> Building request with includeInactive={includeInactive}")

  # Query Parameters
  params <- list()

  # Include inactive athletes
  if (isTRUE(includeInactive)) {
    params$includeInactive <- "true"
  }

  request <- hd_request(paste0(conn@base_url, "/", conn@config@org_id)) |>
    httr2::req_url_path_append("athletes")

  # Only add the query if params list is not empty
  if (length(params) > 0) {
    request <- request |> httr2::req_url_query(!!!params)
  }

  # Log the Debug
  reqPath <- httr2::req_dry_run(request, quiet = TRUE)

  # Safe logging for the query string
  query_string <- if (length(reqPath$query) > 0) paste0("?", reqPath$query) else ""

  logger::log_debug("hawkinR/get_athletes -> {reqPath$method}: {reqPath$headers$host}{reqPath$path}{query_string}")

  # 4. ----- Execute Call -----
  logger::log_trace("hawkinR/get_athletes -> Executing API request")
  resp <-  request |>
    httr2::req_auth_bearer_token(conn@access_token) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  # Response Status
  status <- httr2::resp_status(resp = resp)
  logger::log_debug("hawkinR/get_athletes -> Response status: {status}")

  # 5. ----- Error Handling -----
  error_message <- NULL

  if (status == 401) {
    error_message <- "Error 401: Refresh Token is invalid or expired."
  } else if (status == 500) {
    error_message <- "Error 500: Something went wrong. Please contact dev-team@hawkindynamics.com"
  }

  if (!base::is.null(error_message)) {
    logger::log_error("hawkinR/get_athletes -> {error_message}")
    stop(error_message)
  }

  # 6. ----- Parse Response -----
  if (status == 200) {
    logger::log_trace("hawkinR/get_athletes -> Parsing JSON response")
    body <- httr2::resp_body_json(resp = resp,
                                  check_type = TRUE,
                                  simplifyVector = TRUE)

    # Reshape athletes: core columns + API v1.14 profile fields + unnested
    # external properties, all selected by name (bare column names, no prefix).
    # Optional fields and varying external keys are tolerated by AthletePrep().
    logger::log_trace("hawkinR/get_athletes -> Converting to data frame")
    df_raw <- if (!base::is.null(body$data)) body$data else body[[1]]
    df <- AthletePrep(df_raw, prefix = "")

    logger::log_success("hawkinR/get_athletes -> {nrow(df)} athletes returned")
    return(df)
  } else {
    logger::log_error("hawkinR/get_athletes -> Unexpected HTTP status: {status}")
    stop(paste0("Unexpected HTTP status: ", status), call. = FALSE)
  }
}
