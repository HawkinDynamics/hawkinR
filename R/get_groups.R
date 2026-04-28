#' Get Groups
#'
#' @description
#' Get the group names and ids for all the groups in the org.
#'
#' @usage
#' get_groups(...)
#'
#' @param ... Optional arguments.
#' \itemize{
#'   \item `profile`: A `HawkinAuth` object. If not provided, the active connection is used.
#' }
#'
#' @return
#' Response will be a data frame containing the groups that are in the organization.
#' Each group has the following variables:
#'
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **id** | *chr* | group's unique ID |
#' | **name** | *chr* | group's given name |
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#' df_groups <- get_groups()
#'
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 req_url_path_append req_error req_perform resp_status resp_body_json
#' @importFrom logger log_trace log_debug log_success log_error
#'
#' @export


# Get Groups -----
get_groups <- function(...) {

  # 1. ----- Set Logger -----
  logger::log_trace("hawkinR -> Run: get_groups")

  # 2. ----- Authentication -----
  logger::log_trace("hawkinR/get_groups -> Resolving connection")
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
  logger::log_debug("hawkinR/get_groups -> Token expires in {token_remaining} seconds")
  if (token_remaining < 300) {
    logger::log_info("hawkinR/get_groups -> Token expiring soon. Refreshing...")
    conn <- authenticate(conn)
    set_active_conn(conn)
  }

  # 3. ----- Build URL Request -----
  logger::log_trace("hawkinR/get_groups -> Building request")
  request <- httr2::request(paste0(conn@base_url, "/", conn@config@org_id)) |>
    httr2::req_url_path_append("groups")

  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug("hawkinR/get_groups -> {reqPath$method}: {reqPath$headers$host}{reqPath$path}")

  # 4. ----- Execute Call -----
  logger::log_trace("hawkinR/get_groups -> Executing API request")
  resp <-  request |>
    httr2::req_auth_bearer_token(conn@access_token) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  # Response Status
  status <- httr2::resp_status(resp = resp)
  logger::log_debug("hawkinR/get_groups -> Response status: {status}")

  # 5. ----- Error Handling -----
  error_message <- NULL

  if (status == 401) {
    error_message <- "Error 401: Refresh Token is invalid or expired."
  } else if (status == 500) {
    error_message <- "Error 500: Something went wrong. Please contact dev-team@hawkindynamics.com"
  }

  if (!base::is.null(error_message)) {
    logger::log_error("hawkinR/get_groups -> {error_message}")
    stop(error_message, call. = FALSE)
  }

  # 6. ----- Parse Response -----
  if (status == 200) {
    logger::log_trace("hawkinR/get_groups -> Parsing JSON response")
    body <- httr2::resp_body_json(
      resp = resp,
      check_type = TRUE,
      simplifyVector = TRUE
    )

    logger::log_trace("hawkinR/get_groups -> Converting to data frame")
    logger::log_success("hawkinR/get_groups -> {body[[2]]} groups returned")
    return(base::as.data.frame(body[[1]]))
  } else {
    logger::log_error("hawkinR/get_groups -> Unexpected HTTP status: {status}")
    stop(paste0("Unexpected HTTP status: ", status), call. = FALSE)
  }
}
