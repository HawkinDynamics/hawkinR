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
#' | **external** | *chr* | external properties will have a column of their name with the appropriate values for the athlete of `NA` if it does not apply |
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
  token_remaining <- round(as.numeric(difftime(conn@expires_at, Sys.time(), units = "secs")))
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

  request <- httr2::request(paste0(conn@base_url, "/", conn@config@org_id)) |>
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

    # Create data frame from returns data
    logger::log_trace("hawkinR/get_athletes -> Converting to data frame")
    df <- base::as.data.frame(body[[1]])

    # Handle External Properties
    if (base::ncol(df) > 5) {
      logger::log_trace("hawkinR/get_athletes -> Processing {base::ncol(df) - 5} external properties")
      a <- df[, 1:5]
      b <- df[, 6:base::ncol(df)]
      df <- base::cbind(a, b)
    }

    logger::log_success("hawkinR/get_athletes -> {body[[2]]} athletes returned")
    return(df)
  } else {
    logger::log_error("hawkinR/get_athletes -> Unexpected HTTP status: {status}")
    stop(paste0("Unexpected HTTP status: ", status), call. = FALSE)
  }
}
