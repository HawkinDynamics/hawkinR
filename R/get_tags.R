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
#' **id**   *chr*   tag's unique ID
#'
#' **name**   *chr*   tag's given name
#'
#' **description**   *chr*   description of tag provided by user
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
#' @export

# Get Tags -----
get_tags <- function() {

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
  URL <-base::paste0(urlCloud,"/tags")

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
    base::stop("Error 401: Invalid Access Token.")
  } else  if(response$status_code == 500){
    # Contact Support Response
    base::stop("Error 500: Something went wrong. Please contact support@hawkindynamics.com")
  } else  if(response$status_code == 200){
    # Response GOOD - Run rest of script
    x <- data.frame(
      jsonlite::fromJSON(
        httr::content(response, "text")
      )
    )

    # create df
    df <- x %>%
      dplyr::transmute(
        "id" = .data$data.id,
        "name" = .data$data.name,
        "description" = .data$data.name
      )

    df
  }

  #-----#

  return(Resp)

}
