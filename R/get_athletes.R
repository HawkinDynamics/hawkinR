#' Get Athletes
#'
#' @description
#' Get the athletes for an account. Inactive players will only be included if
#' `inactive` parameter is set to TRUE.
#'
#' @usage
#' get_athletes(inactive = FALSE)
#'
#' @param inactive FALSE by default to exclude inactive players in database. Set to TRUE if you want
#' inactive players included in the return.
#'
#' @return
#' Response will be a data frame containing the athletes that match this query.
#' Each athlete includes the following variables:
#'
#' **id**   *chr*   athlete's unique ID
#'
#' **name**   *chr*   athlete's given name (First Last)
#'
#' **active**   *logi*   athlete is active (TRUE)
#'
#' **teams**   *chr*   team ids separated by ","
#'
#' **groups**   *chr*  group ids separated by ","
#'
#' **external**   *chr* external ids as strings of "externalName:externalId" separated by ","
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called. If you only wish to call active players,
#' # you don't need to provide any parameters.
#'
#' df_athletes <- get_athletes()
#'
#' # If you want to include all athletes, including inactive athletes, include the optional
#' # `inactive` parameter.
#'
#' df_wInactive <- get_athletes(inactive = TRUE)
#'
#' }
#'
#'
#' @importFrom rlang .data
#' @export

# Get Athletes -----
get_athletes <- function(inactive = FALSE) {

  # Retrieve access token and expiration from environment variables
  aToken <- base::Sys.getenv("accessToken")
  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration" ))

  #-----#

  # Check for Access Token and Expiration
  if(base::is.null(aToken) || token_exp <= base::as.numeric(base::Sys.time())) {
    stop("Access token not available or expired. Call accToken() to obtain it.")
  }

  #-----#

  # API Cloud URL
  urlCloud <- base::Sys.getenv("urlRegion")

  # Create URL for request
  URL <- if( isTRUE(inactive) ) {
    base::paste0(urlCloud,"/athletes","?includeInactive=true")
  } else {
    base::paste0(urlCloud,"/athletes")
  }

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

    # Prepare externalId vector
    external <- c()

    # Loop externalId columns
    for (i in 1:nrow(x)) {

      extRow <- NA

      # Check if data.external is not empty
      if (length(x$data.external) > 0) {

        for (n in 1:length(x$data.external)) {

          # get ext name
          extN <- base::names(x$data.external)[n]

          # get ext id
          extId <- x$data.external[[i, n]]

          # create new external id name:id string
          newExt <- base::paste0(extN, ":", extId)

          # add new externalId string to row list if needed
          extRow <- if (base::is.na(extId)) {
            # if extId NA, no change
            extRow
          } else {
            # Add new string to extId Row
            extRow <- if (base::is.na(extRow)) {
              base::paste0(newExt)
            } else {
              base::paste0(extRow, ",", newExt)
            }
          }

        }
      }

      external <- base::c(external, extRow)
    }

    # Create df
    df <- x %>%
      dplyr::transmute(
        "id" = .data$data.id,
        "name" = .data$data.name,
        "active" = .data$data.active,
        "teams" = base::sapply(.data$data.teams, function(x) base::paste(x, collapse = ",")),
        "groups" = base::sapply(.data$data.groups, function(x) base::paste(x, collapse = ","))
      )

    # add externalId column
    df <- base::cbind(df, external)

    # return clean data frame
    df
  }

  #-----#

  return(Resp)

}
