#' Create Athletes
#'
#' @description
#' Create a new athlete or athletes for an account. Bulk create up to 500 athletes at a time.
#'
#' @usage
#' create_athletes(df)
#'
#' @param df Provide a data frame of the athletes to be created. The data frame must follow the schema of:
#'
#' **name**   *chr*   **REQUIRED** athlete's given name (First Last)
#'
#' **image**   *chr*   *optional* URL path to image. `default=null`
#'
#' **active**   *logi*   *optional* athlete is active (TRUE). `default=null`
#'
#' **teams**   *list*   *optional* a single team id as a string or list of team ids. `default=[defaultTeamId]`
#'
#' **groups**   *list*  *optional* a single group id as a string or list of group ids. `default=[]`
#'
#' **external property**   *chr* External properties can be added by adding any additional columns of equal length.
#' The name of the column will become the external property name, and the row value will become the external property
#' value. Use "lowercase" or "snake_case"styles for column names.
#'
#' @return
#' If successful, a confirmation message will be printed with the number of successful athletes created.
#' If there are failures, a data frame containing the athletes that will be returned:
#'
#' **reason**   *chr*   reason for failed creation
#'
#' **name**   *chr*   athlete's given name (First Last)
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called. If you only wish to call active players,
#' # you don't need to provide any parameters.
#'
#' new_athletes <- create_athletes()
#' }
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows
#' @export

# Create Athletes -----
create_athletes <- function(df) { # _nolint: cyclocomp_linter.

    # Retrieve access token and expiration from environment variables
    aToken <- base::Sys.getenv("accessToken")
    token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration"))

    #-----#

    # Check for Access Token and Expiration
    if (base::is.null(aToken) || token_exp <= base::as.numeric(base::Sys.time())) {
        stop("Access token not available or expired. Call accToken() to obtain it.")
    }

    #-----#

    # API Cloud URL
    urlCloud <- base::Sys.getenv("urlRegion")

    # Create URL for request
    URL <- if (nrow(df) > 1) {
        base::paste0(urlCloud, "/athletes/bulk")
    } else {
        base::paste0(urlCloud, "/athletes")
    }

    #-----#

    # Call Variables
    payload <- AthleteJSON(arg_df = df)
    encode <- "raw"

    #-----#

    # POST Request
    response <- httr::VERB(
        "POST",
        URL,
        body = payload,
        httr::add_headers(Authorization = base::paste0("Bearer ", aToken)),
        httr::content_type("application/octet-stream"),
        encode = encode
    )

    #-----#

    # Response Table
    Resp <- if (response$status_code == 401) {
        # Invalid Token Response
        base::stop("Error 401: Invalid Access Token.")
    } else if (response$status_code == 413) {
        # Too many athletes
        base::stop("Error 413: Payload Too Large")
    } else if (response$status_code == 500) {
        # Contact Support Response
        base::stop("Error 500: Something went wrong. Please contact support@hawkindynamics.com")
    } else if (response$status_code == 200) {
        # Response GOOD - Run rest of script
        returned <- jsonlite::fromJSON(
            httr::content(response, "text")
        )

        # Iterate over failed responses
        if (returned$hasFailures) {
            # get failures df
            x <- returned$failures

            # get failures data
            y <- x$data

            # create failures summary df
            reason <- c()
            name <- c()
            failTable <- base::data.frame(name, reason)

            # create failure summary
            for (row in base::seq_len(nrow(returned$failures))) {
                reason <- x$reason[row]
                name <- y$name[row]
                df <- base::data.frame(name, reason)
                failTable <- dplyr::bind_rows(failTable, df)
            }

            if (!is.null(nrow(returned$data))) {
                # Print number of successful creations
                print(paste0("You created ", nrow(returned$data), " athletes successfully."))
            } else {
                # Print no success
                print(paste0("No athletes were successfully created."))
            }
            # Announce number of failures
            print(paste0("You had ", nrow(x), " failed creations:"))
            # Provide fail feedback
            print(failTable)
            failTable
        } else {
            # Announce success
            print(paste0("You created ", nrow(returned$data), " successfully."))
        }
    }

    #-----#

    return(Resp)
}
