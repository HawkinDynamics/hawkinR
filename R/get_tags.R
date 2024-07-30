#' Get Tags
#'
#' @description
#' Get the tag names and ids for all the tags in the system.
#'
#' @usage
#' get_tags()
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
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom httr2 request req_url_path_append req_auth_bearer_token req_error req_perform resp_status resp_body_json
#' @importFrom logger log_info
#'
#' @export


# Get Tags -----
get_tags <- function() {

  # 1. ----- Set Logger -----
  # Log Trace
  logger::log_trace(base::paste0("hawkinR -> Run: get_tags"))

  # 2. ----- Parameter Validation -----

  # Retrieve access token and expiration from environment variables
  aToken <- base::Sys.getenv("accessToken")

  #-----#

  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration"))

  #-----#

  # Check for Access Token and Expiration
  if (base::is.null(aToken) ||
      token_exp <= base::as.numeric(base::Sys.time())) {
    logger::log_error("hawkinR/get_tags -> Access token not available or expired. Call get_access() to obtain it.")
    stop("Access token not available or expired. Call get_access() to obtain it.")
  } else {
    # Log Debug
    logger::log_debug(base::paste0("hawkinR/get_tags -> Temporary access token expires: ", base::as.POSIXct(token_exp)))
  }

  # 3. ----- Build URL Request -----

  # Build Request
  request <- httr2::request(base::Sys.getenv("urlRegion")) %>%
    # Add URL Path
    httr2::req_url_path_append("/tags") %>%
    # Supply Bearer Authentication
    httr2::req_auth_bearer_token(token = aToken)

  # Log Debug
  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug(base::paste0("hawkinR/get_tags -> ", reqPath$method, ": ", reqPath$headers$host, reqPath$path))

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
    logger::log_error(base::paste0("hawkinR/get_tags -> ", error_message))
    stop(error_message)
  }

  # Response Table
  if (status == 200) {
    # Response GOOD - Run rest of script
    # Convert JSON Response
    x <- httr2::resp_body_json(resp = resp,
                               check_type = TRUE,
                               simplifyVector = TRUE)

    # 5. ----- Sort Tag Data -----

    df <- base::as.data.frame(x[[1]])

    # Print confirmation response
    logger::log_success(base::paste0("hawkinR/get_tags -> ", base::nrow(df), " tags returned"))
    # Create data frame from returns data
    return(df)
  }
}

