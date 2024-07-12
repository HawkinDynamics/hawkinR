#' Get Teams
#'
#' @description
#' Get the team names and ids for all the teams in the org.
#'
#' @usage
#' get_teams()
#'
#' @return
#' Response will be a data frame containing the teams that are in the organization.
#' Each team has the following variables:
#'
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **id**          | *chr*    | team's unique ID |
#' | **name**        | *chr*    | team's given name |
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
#' @importFrom httr2 request req_url_path_append req_auth_bearer_token req_error req_perform resp_status resp_body_json
#' @importFrom rlang .data
#' @importFrom logger log_info
#'
#' @export


# Get Teams -----
get_teams <- function() {

  # 1. ----- Set Logger -----
  # Log Trace
  logger::log_trace(base::paste0("hawkinR -> Run: get_teams"))

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
    logger::log_debug(base::paste0("hawkinR/get_teams -> Temporary access token expires: ", as.POSIXct(token_exp)))
  }

  # 3. ----- Build URL Request -----

  # Build Request
  request <- httr2::request(base::Sys.getenv("urlRegion")) %>%
    # Add URL Path
    httr2::req_url_path_append("/teams") %>%
    # Supply Bearer Authentication
    httr2::req_auth_bearer_token(token = aToken)

  # Log Debug
  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug(base::paste0("hawkinR/get_teams -> ", reqPath$method, ": ", reqPath$headers$host, reqPath$path))

  # Execute Call
  resp <- request %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()

  # Response Status
  status <- httr2::resp_status(resp = resp)

  # 4. ----- Create Response Outputs -----

  # Error Handler
  error_message <- NULL

  if (status == 401) {
    error_message <- 'Error 401: Refresh Token is invalid or expired.'
  } else if (status == 500) {
    error_message <- 'Error 500: Something went wrong. Please contact support@hawkindynamics.com'
  }

  if (!base::is.null(error_message)) {
    stop(logger::log_error(base::paste0("hawkinR/get_teams -> ",error_message)))
  }

  # Response Table
  if(status == 200){
    # Response GOOD - Run rest of script
    # Convert JSON Response
    x <- httr2::resp_body_json(
      resp = resp,
      check_type = TRUE,
      simplifyVector = TRUE
    )

    # Print confirmation response
    logger::log_success(base::paste0("hawkinR/get_teams -> ", x[[2]], " teams returned"))
    # Create data frame from returns data
    return(base::as.data.frame(x[[1]]))
  }
}

