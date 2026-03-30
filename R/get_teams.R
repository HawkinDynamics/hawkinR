#' Get Teams
#'
#' @description
#' Get the team names and ids for all the teams in the org.
#'
#' @usage
#' get_teams(...)
#'
#' @param ... Optional arguments.
#' \itemize{
#'   \item `profile`: A `HawkinAuth` object. If not provided, the active connection is used.
#' }
#'
#' @return
#' Response will be a data frame containing the teams that are in the organization.
#' Each team has the following variables:
#'
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **id** | *chr* | team's unique ID |
#' | **name** | *chr* | team's given name |
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#' df_teams <- get_teams()
#'
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 req_error req_perform resp_status resp_body_json
#' @importFrom logger log_trace log_debug log_success log_error
#'
#' @export


# Get Teams -----
get_teams <- function(...) {

  # 1. ----- Set Logger -----
  logger::log_trace("hawkinR -> Run: get_teams")

  # 2. ----- Authentication -----
  logger::log_trace("hawkinR/get_teams -> Resolving connection")
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
  logger::log_debug("hawkinR/get_teams -> Token expires in {token_remaining} seconds")
  if (token_remaining < 300) {
    logger::log_info("hawkinR/get_teams -> Token expiring soon. Refreshing...")
    conn <- authenticate(conn)
    set_active_conn(conn)
  }

  # 3. ----- Build URL Request -----
  logger::log_trace("hawkinR/get_teams -> Building request")
  request <- httr2::request(paste0(conn@base_url, "/", conn@config@org_id)) |>
    httr2::req_url_path_append("teams")

  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug("hawkinR/get_teams -> {reqPath$method}: {reqPath$headers$host}{reqPath$path}")

  # 4. ----- Execute Call -----
  logger::log_trace("hawkinR/get_teams -> Executing API request")
  resp <-  request |>
    httr2::req_auth_bearer_token(conn@access_token) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  # Response Status
  status <- httr2::resp_status(resp = resp)
  logger::log_debug("hawkinR/get_teams -> Response status: {status}")

  # 5. ----- Error Handling -----
  error_message <- NULL

  if (status == 401) {
    error_message <- "Error 401: Refresh Token is invalid or expired."
  } else if (status == 500) {
    error_message <- "Error 500: Something went wrong. Please contact support@hawkindynamics.com"
  }

  if (!base::is.null(error_message)) {
    logger::log_error("hawkinR/get_teams -> {error_message}")
    stop(error_message, call. = FALSE)
  }

  # 6. ----- Parse Response -----
  if (status == 200) {
    logger::log_trace("hawkinR/get_teams -> Parsing JSON response")
    body <- httr2::resp_body_json(
      resp = resp,
      check_type = TRUE,
      simplifyVector = TRUE
    )

    logger::log_trace("hawkinR/get_teams -> Converting to data frame")
    logger::log_success("hawkinR/get_teams -> {body[[2]]} teams returned")
    return(base::as.data.frame(body[[1]]))
  }
}
