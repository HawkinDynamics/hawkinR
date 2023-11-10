#' Get Test Types
#'
#' @description
#' Get the test type names and ids for all the test types in the system.
#'
#' @usage
#' get_testTypes()
#'
#' @return
#' Response will be a data frame containing the tests that are in the HD system.
#' Each test type includes the following variables:
#'
#' **id**   *chr*   test's unique ID
#'
#' **name**   *chr*   test's given name
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#' df_tests <- get_testTypes()
#'
#' }
#'
#' @importFrom rlang .data
#' @export


get_testTypes <- function() {

  # Retrieve access token and expiration from environment variables
  aToken <- base::Sys.getenv("accessToken")
  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration" ))

  #-----#

  # Check for Access Token and Expiration
  if(base::is.null(aToken) || token_exp <= base::as.numeric(base::Sys.time())) {
    stop("Access token not available or expired. Call get_access() to obtain it.")
  }

  #-----#

  # API Cloud URL
  urlCloud <- base::Sys.getenv("urlRegion")

  # Create URL for request
  URL <-base::paste0(urlCloud,"/test_types")

  #-----#

  # Call Variables
  payload <- ""
  encode <- "raw"

  #-----#

  # GET Request
  response <- httr::VERB("GET",
                         URL,
                         body = payload,
                         httr::add_headers(Authorization = base::paste0("Bearer ", aToken)),
                         httr::content_type("application/octet-stream"),
                         encode = encode
  )

  #-----#

  # Response Table
  Resp <- if(response$status_code == 401) {
    # Invalid Token Response
    base::stop("Invalid Access Token.")
  } else  if(response$status_code == 500){
    # Contact Support Response
    base::stop("Something went wrong. Please contact support@hawkindynamics.com")
  } else  if(response$status_code == 200){
    # Response GOOD - Run rest of script
    x <- data.frame(
      jsonlite::fromJSON(
        httr::content(response, "text")
      )
    )

    x
  }

  #-----#

  return(Resp)

}
