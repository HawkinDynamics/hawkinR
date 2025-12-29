#' Get Tags
#'
#' @description
#' Get the tag names and ids for all the tags in the system.
#'
#' @usage
#' get_tags(x = NULL)
#'
#' @param x Optional. A `HawkinAuth` object. If NULL, the active connection is used.
#'
#' @return
#' Response will be a data frame containing the tags that are in the organization.
#' Each tag has the following variables:
#'
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **id**          | *chr*    | tag's unique ID |
#' | **name**        | *chr*    | tag's given name |
#' | **description** | *chr*    | description of tag provided by user |
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#' df_tags <- get_tags()
#'
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 req_error req_perform resp_status resp_body_json
#' @importFrom logger log_trace log_debug log_success log_error
#'
#' @export


# Get Tags -----
get_tags <- function(x = NULL) {

 # 1. ----- Set Logger -----
  logger::log_trace("hawkinR -> Run: get_tags")


  # 2. ----- Authentication (new auth manager) -----
  logger::log_trace("hawkinR/get_tags -> Resolving connection")
  if (is.null(x)) x <- get_active_conn()

  # Token Lifecycle Management
  token_remaining <- round(as.numeric(difftime(x@expires_at, Sys.time(), units = "secs")))
  logger::log_debug("hawkinR/get_tags -> Token expires in {token_remaining} seconds")
  if (token_remaining < 300) {
    logger::log_info("hawkinR/get_tags -> Token expiring soon. Refreshing...")
    x <- authenticate(x)
    set_active_conn(x)
  }

  # 3. ----- Build URL Request -----
  logger::log_trace("hawkinR/get_tags -> Building request")
  request <- httr2::request(paste0(x@base_url, "/", x@config@org_id)) |>
    httr2::req_url_path_append("tags")

  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug("hawkinR/get_tags -> {reqPath$method}: {reqPath$headers$host}{reqPath$path}")

  # 4. ----- Execute Call -----
  logger::log_trace("hawkinR/get_tags -> Executing API request")
  resp <-  request |>
    httr2::req_auth_bearer_token(x@access_token) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  # Response Status
  status <- httr2::resp_status(resp = resp)
  logger::log_debug("hawkinR/get_tags -> Response status: {status}")

  # 5. ----- Error Handling -----
  error_message <- NULL

  if (status == 401) {
    error_message <- "Error 401: Refresh Token is invalid or expired."
  } else if (status == 500) {
    error_message <- "Error 500: Something went wrong. Please contact support@hawkindynamics.com"
  }

  if (!base::is.null(error_message)) {
    logger::log_error("hawkinR/get_tags -> {error_message}")
    stop(error_message, call. = FALSE)
  }

  # 6. ----- Parse Response -----
  if (status == 200) {
    logger::log_trace("hawkinR/get_tags -> Parsing JSON response")
    x <- httr2::resp_body_json(
      resp = resp,
      check_type = TRUE,
      simplifyVector = TRUE
    )

    logger::log_trace("hawkinR/get_tags -> Converting to data frame")
    df <- base::as.data.frame(x[[1]])

    logger::log_success("hawkinR/get_tags -> {base::nrow(df)} tags returned")
    return(df)
  }
}

