#' Get Groups
#'
#' @description
#' Get the group names and ids for all the groups in the org.
#'
#' @usage
#' get_groups(x = NULL)
#'
#' @param x Optional. A `HawkinAuth` object. If NULL, the active connection is used.
#'
#' @return
#' Response will be a data frame containing the groups that are in the organization.
#' Each group has the following variables:
#'
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **id**          | *chr*    | group's unique ID |
#' | **name**        | *chr*    | group's given name |
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
get_groups <- function(x = NULL) {

  # 1. ----- Set Logger -----
  logger::log_trace(base::paste0("hawkinR -> Run: get_groups"))


  # 2. ----- Authentication (new auth manager) -----
  if (is.null(x)) x <- get_active_conn()

  # Token Lifecycle Management
  if (difftime(x@expires_at, Sys.time(), units = "secs") < 300) {
    x <- authenticate(x)
    set_active_conn(x)
  }

  # Log Debug
  request <- httr2::request(paste0(x@base_url, "/", x@config@org_id)) |>
    httr2::req_url_path_append("groups")

  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug(base::paste0(
    "hawkinR/get_groups -> ",
    reqPath$method, ": ",
    reqPath$headers$host, reqPath$path
  ))

  # Execute Call
  resp <-  request |>
    httr2::req_auth_bearer_token(x@access_token) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()


  # Response Status
  status <- httr2::resp_status(resp = resp)

  # 3. ----- Create Response Outputs -----


  error_message <- NULL


  if (status == 401) {
    error_message <- "Error 401: Refresh Token is invalid or expired."
  } else if (status == 500) {
    error_message <- "Error 500: Something went wrong. Please contact support@hawkindynamics.com"
  }


  if (!base::is.null(error_message)) {
    logger::log_error(base::paste0(
      "hawkinR/get_groups -> ", error_message
    ))
    stop(error_message, call. = FALSE)
  }


  if (status == 200) {
    x <- httr2::resp_body_json(
      resp = resp,
      check_type = TRUE,
      simplifyVector = TRUE
    )


    logger::log_success(base::paste0("hawkinR/get_groups -> ", x[[2]], " groups returned"))
    return(base::as.data.frame(x[[1]]))
  }
}

