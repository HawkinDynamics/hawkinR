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
#' | **teams** | *list* or *chr* | *optional* | team ids, as a list column (`I(list("team1", c("team2", "team3")))`) or a comma-separated string (`"team2, team3"`). `NA`, `NULL` or blank leaves the athlete's teams unchanged. |
#' | **groups** | *list* or *chr* | *optional* | group ids, as a list column or a comma-separated string, like `teams`. `NA`, `NULL` or blank leaves the athlete's groups unchanged. |
#' | **position** | *chr* | *optional* | playing position (e.g. "Forward"). Surrounding whitespace is trimmed. |
#' | **sport** | *chr* | *optional* | sport name (e.g. "Basketball"). Surrounding whitespace is trimmed. |
#' | **dob** | *chr*, *Date*, *POSIXct* or *num* | *optional* | date of birth: `"YYYY-MM-DD"` or a 4-digit year (`"1998"` or `1998`). The API rejects years before 1900 or after the current year and full dates in the future; hawkinR doesn't check these itself. See **Dates of birth** below. |
#' | **height** | *num* | *optional* | height in **centimeters**. The API rejects values outside 0 to 300; hawkinR doesn't check the range itself. `0` counts as not provided and isn't sent. Decimals are allowed; it is sent and read back rounded to 1 decimal place. See **Height** below. |
#' | **external property** | *chr* | *optional* | External properties can be added by adding any additional columns of equal length. The name of the column will become the external property name, and the row value will become the external property value. `NA` and blank values are not sent. Use "lowercase" or "snake_case" styles for column names. |
#'
#' *If optional fields are not present in an update request, those properties will be left unchanged.*
#'
#' **External properties.** The API replaces an athlete's external properties
#' on update; it does not merge them. External properties are only sent for an
#' athlete with at least one non-`NA`, non-blank custom value, and then every
#' custom property without a value in that row is removed. With no custom
#' columns, or with every custom cell `NA` or blank, nothing is sent and the
#' athlete's custom properties are left unchanged.
#'
#' `position`, `sport`, `dob` and `height` are written as native athlete
#' fields, never as external properties. A value that is `NA` or blank (`""`)
#' leaves that field **unchanged**: the API cannot clear these fields. To clear
#' one, edit the athlete in the Hawkin app.
#'
#' The output of `get_athletes()` can be modified and passed straight back in.
#' `lastTestedOn` is read-only and is not sent. If an athlete has an old
#' external property named `position`, `sport`, `dob`, `height` or
#' `lastTestedOn`, `get_athletes()` returns it as a suffixed column such as
#' `position.1`. That column is not sent, with a warning, and because external
#' properties are replaced the old external key is removed from the athlete.
#'
#' **Dates of birth.** `Date` values are sent as `YYYY-MM-DD`. `POSIXct`
#' date-times are sent as the calendar date in their own time zone (their
#' `tzone` attribute, or the session time zone if they have none), with no
#' conversion to UTC, so the day you see printed is the day that is stored. The
#' API stores and returns the string exactly as sent.
#'
#' **Height.** The API reads values below 100 back as legacy feet or inches, so
#' a height of 95 is read back as 241.3. A warning is raised when any height
#' below 100 is sent; the request still goes ahead.
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
#' If successful, a confirmation message will be printed with the number of successful athletes updated.
#' If there are failures, a data frame containing the athletes that failed to be updated will be returned with columns:
#'
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **reason** | *chr* | Reason for failed update |
#' | **name** | *chr* | Athlete's given name (First Last) |
#'
#' @examples
#' \dontrun{
#' # Example data frame following the required schema
#' df <- data.frame(
#'   id = c("uniqueAthleteIdOne", "uniqueAthleteIdTwo"),
#'   name = c("John Doe", "Jane Smith"),
#'   image = c("https://example.com/johndoe.jpg", "https://example.com/janesmith.jpg"),
#'   active = c(TRUE, FALSE),
#'   teams = I(list("team1", c("team2", "team3"))),
#'   groups = I(list(NULL, "group1")),
#'   position = c("Forward", NA),
#'   height = c(190.5, 172),
#'   external_property = c("value1", "value2")
#' )
#'
#' # Update athletes using the example data frame
#' update_athletes(athleteData = df)
#'
#' # Or modify the output of get_athletes() and send it back
#' athletes <- get_athletes()
#' athletes$sport <- "Basketball"
#' update_athletes(athleteData = athletes)
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
  token_remaining <- token_seconds_remaining(conn)
  logger::log_debug("hawkinR/update_athletes -> Token expires in {token_remaining} seconds")
  if (token_remaining < 300) {
    logger::log_info("hawkinR/update_athletes -> Token expiring soon. Refreshing...")
    conn <- authenticate(conn)
    set_active_conn(conn)
  }

  # 3. ----- Build URL Request -----

  # Athletes Data to Send
  payload <- UpdateAthleteJSON(arg_df = athleteData)

  request <- hd_request(paste0(conn@base_url, "/", conn@config@org_id)) |>
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
    error_message <- "Error 500: Something went wrong. Please contact dev-team@hawkindynamics.com"
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
    allSuccess <- ""

    if (!base::is.null(successCount) && successCount > 0) {
      allSuccess <- base::paste0(d$name, collapse = ", ")
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
      fail_df <- base::data.frame(
        reason = body$failures$reason,
        name = body$failures$data[, 1],
        stringsAsFactors = FALSE
      )
      return(invisible(fail_df))
    } else if (base::isTRUE(hasFailures) && successCount > 0) {
      logger::log_success("hawkinR/update_athletes -> {successCount} athletes were updated successfully: {allSuccess}")
      logger::log_warn("hawkinR/update_athletes -> {failures} athletes failed || {allFails}")
      fail_df <- base::data.frame(
        reason = body$failures$reason,
        name = body$failures$data[, 1],
        stringsAsFactors = FALSE
      )
      return(invisible(fail_df))
    } else if (base::isFALSE(hasFailures)) {
      logger::log_success("hawkinR/update_athletes -> {successCount} athletes updated successfully: {allSuccess}")
      return(invisible(TRUE))
    } else {
      logger::log_error("hawkinR/update_athletes -> Unexpected status code: {status}")
      stop("Unexpected status code.", call. = FALSE)
    }
  }
}
