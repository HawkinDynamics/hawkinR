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
#' | **id**          | *chr*    | **REQUIRED**  | athlete's Hawkin Dynamics unique ID |
#' | **name**        | *chr*    | *optional*    | athlete's given name (First Last) |
#' | **image**       | *chr*    | *optional*    | URL path to image. `default = null` |
#' | **active**      | *logi*   | *optional*    | athlete is active (TRUE). `default = null` |
#' | **teams**       | *list*   | *optional*    | a single team id as a string or list of team ids. `default = [defaultTeamId]` |
#' | **groups**      | *list*   | *optional*    | a single group id as a string or list of group ids. `default = []` |
#' | **external property** | *chr* | *optional* | External properties can be added by adding any additional columns of equal length. The name of the column will become the external property name, and the row value will become the external property value. Use "lowercase" or "snake_case" styles for column names. |
#'
#' *If optional fields are not present in an update request, those properties will be left unchanged. However, when updating external properties, custom properties that are not present will be removed.*
#'
#' @usage
#' update_athletes(athleteData)
#'
#' @param athleteData Provide a data frame of the athlete or athletes to be updated.
#'
#' @return
#' If successful, a confirmation message will be printed with the number of successful athletes created.
#' If there are failures, a data frame containing the athletes that failed to be created will be returned with columns:
#'
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **reason**      | *chr*    | Reason for failed creation |
#' | **name**        | *chr*    | Athlete's given name (First Last) |
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
#' @importFrom httr2 request req_url_path_append req_method req_body_raw req_auth_bearer_token req_error req_perform resp_status resp_body_json
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows
#' @importFrom logger log_info
#'
#' @export


# Update Athletes -----
update_athletes <- function(athleteData) {

  # 1. ----- Set Logger -----
  # Log Trace
  logger::log_trace(base::paste0("hawkinR -> Run: update_athletes"))

  # Save the current setting
  old_show_error_messages <- base::getOption("show.error.messages")
  base::on.exit(base::options(show.error.messages = old_show_error_messages),
                add = TRUE)

  # Disable error messages
  base::options(show.error.messages = FALSE)

  # 2. ----- Parameter Validation -----

  # Retrieve access token and expiration from environment variables
  aToken <- base::Sys.getenv("accessToken")

  #-----#

  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration"))

  #-----#

  # Check for Access Token and Expiration
  if (base::is.null(aToken) ||
      token_exp <= base::as.numeric(base::Sys.time())) {
    stop(logger::log_error("Access token not available or expired. Call get_access() to obtain it."))
  } else {
    # Log Debug
    logger::log_debug(base::paste0("hawkinR/update_athletes -> Temporary access token expires: ", as.POSIXct(token_exp)))
  }

  # 3. ----- Build URL Request -----

  # Athletes Data to Send
  payload <- UpdateAthleteJSON(arg_df = athleteData)

  # Build Request
  request <- httr2::request(base::Sys.getenv("urlRegion")) %>%
    # Add URL Path
    httr2::req_url_path_append("/athletes/bulk") %>%
    # Change HTTP Method
    httr2::req_method("PUT") %>%
    # Add JSON body
    httr2::req_body_raw(body = payload, type = "application/json") %>%
    # Supply Bearer Authentication
    httr2::req_auth_bearer_token(token = aToken)

  # Log Debug
  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug(base::paste0(
    "hawkinR/update_athletes -> ",
    reqPath$method,
    ": ",
    reqPath$headers$host,
    reqPath$path
  ))

  # Execute Call
  resp <- request %>%
    httr2::req_error(
      is_error = function(resp)
        FALSE
    ) %>%
    httr2::req_perform()

  # Response Status
  status <- httr2::resp_status(resp = resp)

  # 4. ----- Create Response Outputs -----

  # Error Handler
  error_message <- NULL

  if (status == 401) {
    error_message <- 'Error 401: Refresh Token is invalid or expired.'
  } else if (status == 500) {
    error_message <-
      'Error 500: Something went wrong. Please contact support@hawkindynamics.com'
  }

  if (!base::is.null(error_message)) {
    stop(logger::log_error(
      base::paste0("hawkinR/update_athletes -> ", error_message)
    ))
  }

  # Response Table
  if (status == 200) {
    # Response GOOD - Run rest of script
    # Convert JSON Response
    x <- httr2::resp_body_json(resp = resp,
                               check_type = TRUE,
                               simplifyVector = TRUE)

    # 5. ----- Sort Athlete Response Data -----

    # Number of Athletes Entered
    n <- base::nrow(athleteData)

    # Successful Data
    d <- x$data
    successCount <- base::nrow(d)
    if (!base::is.null(successCount)) {
      if(successCount > 1) {
        allSuccess <- base::paste0(d$name, collapse = ", ")
      } else {
        allSuccess <- base::paste0(d$name)
      }
    }

    # Has Failures Boolean
    hasFailures <- x$hasFailures

    # Failures Data
    if (base::isTRUE(hasFailures)) {
      # Count of Failures
      failures <- base::nrow(x$failures)
      # Failures Reasons
      reasons <- x$failures$reason
      # Names of Failed Athletes
      failedNames <- x$failures$data[,1]
      # String of Fails
      allFails <- base::data.frame(reasons, failedNames) %>%
        # Group by reasons
        dplyr::group_by(.data$reasons) %>%
        # Create value strings
        dplyr::summarise(values = base::paste0(failedNames, collapse = ", ")) %>%
        # Create Outputs
        dplyr::mutate(output = base::paste0(.data$reasons, " [", .data$values, "]"))

      # Create Finished Failures
      allFails <- base::paste0(allFails$output, collapse = " | ")
    }

    # Report Response
    if (base::isTRUE(hasFailures) && isTRUE(is.null(successCount))) {
      # All Failures
      logger::log_warn(("hawkinR/update_athletes -> {failures} athletes failed || {allFails}"))
    } else if (base::isTRUE(hasFailures) && successCount > 0) {
      # Fails and Success
      logger::log_success(("hawkinR/update_athletes -> {successCount} athletes were updated successfully: {allSuccess}"))
      logger::log_warn(("hawkinR/update_athletes -> {failures} athletes failed || {allFails}"))
    } else if (base::isFALSE(hasFailures)) {
      # All Success
      logger::log_success("hawkinR/update_athletes -> {successCount} athletes updated successfully: {allSuccess}")
    } else {
      stop(logger::log_error("hawkinR/update_athletes -> Unexpected status code: {status}"))
    }
  }
}

