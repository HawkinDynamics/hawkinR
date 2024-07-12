#' Create Athletes
#'
#' @description
#' Create a new athlete or athletes for an account. Bulk create up to 500 athletes at a time.
#'
#' @details
#' The data frame passed as the argument must use the following schema:
#' | **Column Name** | **Type** | **Inclusion** |**Description** |
#' |-----------------|----------|---------------|----------------|
#' | **name**        | *chr*    | **REQUIRED**  | athlete's given name (First Last) |
#' | **image**       | *chr*    | *optional*    | URL path to image. `default = null` |
#' | **active**      | *logi*   | *optional*    | athlete is active (TRUE). `default = null` |
#' | **teams**       | *list*   | *optional*    | a single team id as a string or list of team ids. `default = [defaultTeamId]` |
#' | **groups**      | *list*   | *optional*    | a single group id as a string or list of group ids. `default = []` |
#' | **external property** | *chr* | *optional* | External properties can be added by adding any additional columns of equal length. The name of the column will become the external property name, and the row value will become the external property value. Use "lowercase" or "snake_case" styles for column names. |
#'
#' @usage
#' create_athletes(athleteData)
#'
#' @param athleteData A data frame of the athletes to be created. The data frame must follow the schema:
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
#'   name = c("John Doe", "Jane Smith"),
#'   image = c("http://example.com/johndoe.jpg", "http://example.com/janesmith.jpg"),
#'   active = c(TRUE, FALSE),
#'   teams = c("team1", c("team2", "team3"))),
#'   groups = c(NULL, "group1")),
#'   external_property = c("value1", "value2")
#' )
#'
#' # Create athletes using the example data frame
#' create_athletes(athleteData = df)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 request req_url_path_append req_method req_body_raw req_auth_bearer_token req_error req_perform resp_status resp_body_json
#' @importFrom logger log_info
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows
#'
#' @export


# Create Athletes -----
create_athletes <- function(athleteData) {
  # 1. ----- Set Logger -----
  # Log Trace
  logger::log_trace(base::paste0("hawkinR -> Run: create_athletes"))

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
    stop("Access token not available or expired. Call get_access() to obtain it.")
  } else {
    # Log Debug
    logger::log_debug(base::paste0("hawkinR/create_athletes -> Temporary access token expires: ", as.POSIXct(token_exp)))
  }

  # 3. ----- Build URL Request -----

  # Athletes Data to Send
  payload <- AddAthleteJSON(arg_df = athleteData)

  # Build Request
  request <- httr2::request(base::Sys.getenv("urlRegion")) %>%
    # Add URL Path
    httr2::req_url_path_append("/athletes/bulk") %>%
    # Change HTTP Method
    httr2::req_method("POST") %>%
    # Add JSON body
    httr2::req_body_raw(body = payload, type = "application/json") %>%
    # Supply Bearer Authentication
    httr2::req_auth_bearer_token(token = aToken)

  # Log Debug
  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug(base::paste0(
    "hawkinR/create_athletes -> ",reqPath$method, ": ", reqPath$headers$host, reqPath$path))

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
      base::paste0("hawkinR/create_athletes -> ", error_message)
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
      logger::log_warn(("hawkinR/create_athletes -> {failures} athletes failed || {allFails}"))
    } else if (base::isTRUE(hasFailures) && successCount > 0) {
      # Fails and Success
      logger::log_success(("hawkinR/create_athletes -> {successCount} athletes were added successfully: {allSuccess}"))
      logger::log_warn(("hawkinR/create_athletes -> {failures} athletes failed || {allFails}"))
    } else if (base::isFALSE(hasFailures)) {
      # All Success
      logger::log_success("hawkinR/create_athletes -> {successCount} athletes added successfully: {allSuccess}")
    } else {
      stop(logger::log_error("hawkinR/create_athletes -> Unexpected status code: {status}"))
    }
  }
}
