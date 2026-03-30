#' Update Athletes
#'
#' @description
#' Update an athlete or athletes for an account. Bulk update up to 500 athletes at a time.
#'
#' @details
#' The data frame passed as the argument must use the following schema:
#'
#' | **Column Name** | **Type** | **Inclusion** | **Description** |
#' |-----------------|----------|---------------|-----------------|
#' | **id** | *chr* | **REQUIRED** | athlete's Hawkin Dynamics unique ID |
#' | **name** | *chr* | *optional* | athlete's given name (First Last) |
#' | **image** | *chr* | *optional* | URL path to image. `default = null` |
#' | **active** | *logi* | *optional* | athlete is active (TRUE). `default = null` |
#' | **teams** | *list* | *optional* | a single team id as a string or list of team ids. `default = [defaultTeamId]` |
#' | **groups** | *list* | *optional* | a single group id as a string or list of group ids. `default = []` |
#' | **external property** | *chr* | *optional* | External properties can be added by adding any additional columns of equal length. The name of the column will become the external property name, and the row value will become the external property value. Use "lowercase" or "snake_case" styles for column names. |
#'
#' *If optional fields are not present in an update request, those properties will be left unchanged. However, when updating external properties, custom properties that are not present will be removed.*
#'
#' @usage
#' update_athletes(athleteData, ...)
#'
#' @param athleteData Provide a data frame of the athlete or athletes to be updated.
#'
#' @param ... Optional arguments.
#' \itemize{
#'   \item `profile`: A `HawkinAuth` object. If not provided, the active connection is used.
#' }
#'
#' @return
#' If successful, a confirmation message will be printed with the number of successful athletes created.
#' If there are failures, a data frame containing the athletes that failed to be created will be returned with columns:
#'
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **reason** | *chr* | Reason for failed creation |
#' | **name** | *chr* | Athlete's given name (First Last) |
#'
#' @examples
#' \dontrun{
#' # Example data frame following the required schema
#' df <- data.frame(
#'   id = c("uniqueAthleteIdOne", "uniqueAthleteIdTwo"),
#'   name = c("John Doe", "Jane Smith"),
#'   image = c("http://example.com/johndoe.jpg", "http://example.com/janesmith.jpg"),
#'   active = c(TRUE, FALSE),
#'   teams = c("team1", c("team2", "team3"))),
#'   groups = c(NULL, "group1")),
#'   external_property = c("value1", "value2")
#' )
#'
#' # Update athletes using the example data frame
#' update_athletes(athleteData = df)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 req_url_path_append req_method req_body_raw req_error req_perform resp_status resp_body_json
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows group_by summarise mutate
#' @importFrom logger log_trace log_debug log_warn log_success log_error
#'
#' @export


# Update Athletes -----
update_athletes <- function(athleteData, ...) {

  # 1. ----- Set Logger -----
  logger::log_trace(base::paste0("hawkinR -> Run: update_athletes"))


  # 2. ----- Authentication -----
  logger::log_trace("hawkinR/update_athletes -> Resolving connection")
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
  logger::log_debug("hawkinR/update_athletes -> Token expires in {token_remaining} seconds")
  if (token_remaining < 300) {
    logger::log_info("hawkinR/update_athletes -> Token expiring soon. Refreshing...")
    conn <- authenticate(conn)
    set_active_conn(conn)
  }

  # 3. ----- Build URL Request -----

  # Athletes Data to Send
  payload <- AddAthleteJSON(arg_df = athleteData)

  request <- httr2::request(paste0(conn@base_url, "/", conn@config@org_id)) |>
    httr2::req_url_path_append("athletes/bulk") |>
    httr2::req_method("PUT") |>
    httr2::req_body_raw(body = payload, type = "application/json")

  # Log Debug
  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug(base::paste0(
    "hawkinR/update_athletes -> ",
    reqPath$method, ": ",
    reqPath$headers$host, reqPath$path
  ))


  # Execute Call
  resp <-  request |>
    httr2::req_auth_bearer_token(conn@access_token) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()


  # Response Status
  status <- httr2::resp_status(resp = resp)

  # 4. ----- Create Response Outputs -----


  error_message <- NULL


  if (status == 401) {
    error_message <- "Error 401: Refresh Token is invalid or expired."
  } else if (status == 500) {
    error_message <- "Error 500: Something went wrong. Please contact support@hawkindynamics.com"
  }


  if (!base::is.null(error_message)) {
    logger::log_error(base::paste0(
      "hawkinR/update_athletes -> ", error_message
    ))
    stop(error_message, call. = FALSE)
  }


  if (status == 200) {
    body <- httr2::resp_body_json(
      resp = resp,
      check_type = TRUE,
      simplifyVector = TRUE
    )

    # 5. ----- Sort Athlete Response Data -----


    d <- body$data
    successCount <- base::nrow(d)


    if (!base::is.null(successCount)) {
      allSuccess <- if (successCount > 1) {
        base::paste0(d$name, collapse = ", ")
      } else {
        base::paste0(d$name)
      }
    }


    hasFailures <- body$hasFailures


    if (base::isTRUE(hasFailures)) {
      failures <- base::nrow(body$failures)
      reasons <- body$failures$reason
      failedNames <- body$failures$data[, 1]


      allFails <- base::data.frame(reasons, failedNames) %>%
        dplyr::group_by(.data$reasons) %>%
        dplyr::summarise(values = base::paste0(failedNames, collapse = ", ")) %>%
        dplyr::mutate(output = base::paste0(.data$reasons, " [", .data$values, "]"))


      allFails <- base::paste0(allFails$output, collapse = " | ")
    }

    # Report Response
    if (base::isTRUE(hasFailures) && isTRUE(is.null(successCount))) {
      logger::log_warn("hawkinR/update_athletes -> {failures} athletes failed || {allFails}")
    } else if (base::isTRUE(hasFailures) && successCount > 0) {
      logger::log_success("hawkinR/update_athletes -> {successCount} athletes were updated successfully: {allSuccess}")
      logger::log_warn("hawkinR/update_athletes -> {failures} athletes failed || {allFails}")
    } else if (base::isFALSE(hasFailures)) {
      logger::log_success("hawkinR/update_athletes -> {successCount} athletes updated successfully: {allSuccess}")
    } else {
      stop(logger::log_error("hawkinR/update_athletes -> Unexpected status code: {status}"))
    }
  }
}
