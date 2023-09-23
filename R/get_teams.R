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
#' **id**   *chr*   team's unique ID
#'
#' **name**   *chr*   team's given name
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#' df_teams <- get_teams()
#'
#' }
#'
#' @importFrom rlang .data
#' @export


get_teams <- function() {

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
  URL <-base::paste0(urlCloud,"/teams")

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
    "Invalid Access Token."
  } else  if(response$status_code == 500){
    # Contact Support Response
    "Something went wrong. Please contact support@hawkindynamics.com"
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

  return(
    if(response$status_code == 200) {
      teamTbl <- Resp %>%
        dplyr::transmute(
          "id" = .data$data.id,
          "name" = .data$data.name
        )

      teamTbl
    } else {
      base::print(Resp)
    }
  )

}
