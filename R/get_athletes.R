#' Get Athletes
#'
#' @description
#' Get the athletes for an account. Inactive players will only be included if
#' `includeInactive` parameter is set to TRUE.
#'
#' @usage
#' get_athletes(includeInactive = FALSE, x = NULL)
#'
#' @param includeInactive FALSE by default to exclude inactive players in database. Set to TRUE if you want
#' inactive players included in the return.
#'
#' @param x Optional. A `HawkinAuth` object. If NULL, the active connection is used.
#'
#' @return
#' Response will be a data frame containing the athletes that match this query.
#' Each athlete includes the following variables:
#'
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **id**          | *chr*    | athlete's unique ID |
#' | **name**        | *chr*    | athlete's given name (First Last) |
#' | **active**      | *bool*   | athlete is active (TRUE) |
#' | **teams**       | *chr*    | team ids separated by "," |
#' | **groups**      | *chr*    | group ids separated by "," |
#' | **external**    | *chr*    | external properties will have a column of their name with the appropriate values for the athlete of `NA` if it does not apply |
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
get_athletes <- function(includeInactive = FALSE, x = NULL) {


  # 1. ----- Set Logger -----
  logger::log_trace(base::paste0("hawkinR -> Run: get_athletes"))


  # 2. ----- Authentication (new auth manager) -----
  if (is.null(x)) x <- get_active_conn()

  # Token Lifecycle Management
  if (difftime(x@expires_at, Sys.time(), units = "secs") < 300) {
    x <- authenticate(x)
    set_active_conn(x)
  }

  # 3. ----- Build URL Request -----

  # Query Parameters
  params <- list()

  # Include inactive athletes
  if (isTRUE(includeInactive)) {
    params$includeInactive <- "true"
  }

  request <- httr2::request(paste0(x@base_url, "/", x@config@org_id)) |>
    httr2::req_url_path_append("athletes")

  # Only add the query if params list is not empty
  if (length(params) > 0) {
    request <- request |> httr2::req_url_query(!!!params)
  }

  # Log the Debug
  reqPath <- httr2::req_dry_run(request, quiet = TRUE)

  # Safe logging for the query string
  query_string <- if (length(reqPath$query) > 0) paste0("?", reqPath$query) else ""

  logger::log_debug(base::paste0(
    "hawkinR/get_athletes -> ",
    reqPath$method, ": ",
    reqPath$headers$host, reqPath$path,
    query_string
  ))

  # Execute Call
  resp <-  request |>
    httr2::req_auth_bearer_token(x@access_token) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  # Response Status
  status <- httr2::resp_status(resp = resp)

  # 4. ----- Create Response Outputs -----


  # Error Handler
  error_message <- NULL


  if (status == 401) {
    error_message <- "Error 401: Refresh Token is invalid or expired."
  } else if (status == 500) {
    error_message <- "Error 500: Something went wrong. Please contact support@hawkindynamics.com"
  }


  if (!base::is.null(error_message)) {
    logger::log_error(base::paste0(
      "hawkinR/get_athletes -> ", error_message
    ))
    stop(error_message)
  }

  # Response Table
  if (status == 200) {
    # Convert JSON Response
    x <- httr2::resp_body_json(resp = resp,
                               check_type = TRUE,
                               simplifyVector = TRUE)

    # 5. ----- Sort Response Athlete Data -----


    logger::log_success(base::paste0("hawkinR/get_athletes -> ", x[[2]], " athletes returned"))


    # Create data frame from returns data
    df <- base::as.data.frame(x[[1]])


    # Handle External Properties
    if (base::ncol(df) > 5) {
      a <- df[, 1:5]
      b <- df[, 6:base::ncol(df)]
      return(base::cbind(a, b))
    }


    return(df)
  }
}


