#' Get Test Trials By Test Type
#'
#' @description
#' Get only tests of the specified type for an account.
#'
#' @usage
#' get_tests_type(typeId, from, to, sync = FALSE, active = TRUE)
#'
#' @param typeId Supply a test type id to only retrieve tests of that type.
#'
#' @param from Optionally supply a time (Unix timestamp) you want the tests from. If you do not
#' supply this value you will receive every test. This parameter is best suited for bulk exports of
#' historical data.
#'
#' @param to Optionally supply a time (Unix timestamp) you want the tests to. If you do not
#' supply this value you will receive every test from the beginning of time or the optionally
#' supplied `from` parameter. This parameter is best suited for bulk exports of historical data.
#'
#' @param sync  The result set will include updated and newly created tests. This parameter is best
#' suited to keep your database in sync with the Hawkin database. If you do not supply this value
#' you will receive every test.
#'
#' @param active There was a change to the default API configuration to reflect the majority of
#' users API configuration. Inactive tests or tests where `active:false` are returned in these
#' configuration. Be default, `active` is set to TRUE. To return all tests, including disabled
#' trials, set `active` to FALSE.
#'
#' @return
#' Response will be a data frame containing the trials of the specified type and within the time range (if specified).
#'
#' **id**   *str*   Test trial unique ID
#'
#' **active**   *logi*   The trial is active and not disabled
#'
#' **timestamp**   *int*   UNIX timestamp of trial
#'
#' **segment**   *chr*   Description of the test type and trial number of the session (testType:trialNo)
#'
#' **testType.id**   *chr*   Id of the test type of the trial
#'
#' **testType.name**   *chr*   Name of the test type of the trial
#'
#' **testType.canonicalId**   *chr*   Canonical Id of the test type of the trial
#'
#' **test_type_tag_ids**   *chr*   String of Ids associated with tags used during the test trial
#'
#' **test_type_tag_names**   *chr*   String of names of tags used during the test trial
#'
#' **test_type_tag_desc**   *chr*   String of descriptions of tags used during the test trial
#'
#' **athlete.id**   *chr*   Unique Id of the athlete
#'
#' **athlete.name**   *chr*   Athlete given name
#'
#' **athlete.active**   *logi*   The athlete is active
#'
#' **athlete.teams**   *list*   List containing Ids of each team the athlete is on
#'
#' **athlete.groups**   *list*   List containing Ids of each group the athlete is in
#'
#' Only metrics of the given test type are included as the remaining variables.
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#'
#' ## Call for all CMJ tests
#' df_cmj <- get_tests_type(typeId = "7nNduHeM5zETPjHxvm7s")
#'
#'
#' ## Call for Free Run tests within a  specific time frame
#' df_free <- get_tests_type(typeId = "5pRSUQVSJVnxijpPMck3", from = 1689958617, to = 1691207356)
#'
#'
#' ## Call for Squat Jump tests since a specific date
#' df_sjSince <- get_tests_type("QEG7m7DhYsD6BrcQ8pic", from = 1689958617)
#'
#' }
#'
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @export


## Get Tests Data by Test Type -----
get_tests_type <- function(typeId, from = NULL, to = NULL, sync = FALSE, active= TRUE) {

  # Retrieve Access Token and Expiration from Environment Variables
  aToken <- base::Sys.getenv("accessToken")
  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration"))

  #-----#

  # Check for Access Token and Expiration
  if(base::is.null(aToken) || token_exp <= base::as.numeric(base::Sys.time())) {
    stop("Access token not available or expired. Call accToken() to refresh.")
  }

  # Check for Proper EPOCH Times
  if(!is.null(from) && !is.numeric(from)) {
    base::stop("Error: `from` expecting numeric EPOCH/Unix timestamp.")
  } else if(!is.null(to) && !is.numeric(to)) {
    base::stop("Error: `to` expecting numeric EPOCH/Unix timestamp.")
  }

  #-----#

  # API Cloud URL
  urlCloud <- base::Sys.getenv("urlRegion")


  # From DateTime
  fromDT <- if(base::is.null(from)) {
    ""
  } else if(!is.numeric(from)) {
    base::stop("date must be in numeric unix format")
  } else if(base::is.numeric(from) && base::isTRUE(sync)) {
    base::paste0("&syncFrom=",from)
  } else if(base::is.numeric(from) && base::isFALSE(sync)) {
    base::paste0("&from=",from)
  }

  # To DateTime
  toDT <- if(base::is.null(to)) {
    ""
  } else if(!is.numeric(to)) {
    base::stop("date must be in numeric unix format")
  } else if(base::is.numeric(to) && base::isTRUE(sync)) {
    base::paste0("&syncTo=",to)
  } else if(base::is.numeric(to) && base::isFALSE(sync)){
    base::paste0("&to=",to)
  }

  # Valid test type IDs
  valid_typeIds <- c(
    "7nNduHeM5zETPjHxvm7s",
    "QEG7m7DhYsD6BrcQ8pic",
    "2uS5XD5kXmWgIZ5HhQ3A",
    "gyBETpRXpdr63Ab2E0V8",
    "5pRSUQVSJVnxijpPMck3",
    "pqgf2TPUOQOQs6r0HQWb",
    "r4fhrkPdYlLxYQxEeM78",
    "ubeWMPN1lJFbuQbAM97s",
    "rKgI4y3ItTAzUekTUpvR"
  )

  # check typeId
  tId <- if (typeId %in% valid_typeIds) {
    typeId
  } else {
    base::stop("typeId incorrect. Check your entry")
  }

  # Create URL for request!!!!!!!
  URL <- base::paste0(urlCloud,"?testTypeId=", tId, fromDT, toDT)

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
                         httr::content_type("application/octet-stream"), encode = encode
  )

  #-----#

  # Response
  Resp <- if(response$status_code == 401) {
    base::stop("Invalid Access token.")
  } else if(response$status_code == 500) {
    base::stop("Someting went wrong. Please contact support@hawkindynamics.com")
  } else if(response$status_code == 200) {
    x <- tryCatch({
      base::data.frame(
        jsonlite::fromJSON(
          httr::content(response, "text")
        )
      )
    }, error = function(e) {
      # Handle the error here
      base::stop("No tests returned. If you feel this is incorrect, check the typeId and date range.")
    })

    # Check if an error occurred
    if (base::inherits(x, "try-error")) {
      # Handle the error case here
    } else {
      # The code ran successfully, and 'result' contains the data frame
    }

    #-----#
    ### Create data frame ###
    #-----#

    # Clean Resp Headers
    base::names(x) <- base::sub("^data\\.", "", base::names(x))

    ##-- External IDs --##

    # Create externalId df
    extDF <- x$athlete$external

    # Prepare externalId vector
    external <- base::c()

    # Identify External Ids
    if( base::ncol(extDF) > 0) {
      # Loop externalId columns
      for (i in 1:base::nrow(extDF)) {

        extRow <- NA

        for (n in 1:base::ncol(extDF)) {

          # get externalId name
          extN <- base::names(extDF)[n]

          # get ext id
          extId <- extDF[i,n]

          # create new external id name:id string
          newExt <- base::paste0(extN, ":", extId)

          # add new externalId string to row list if needed
          extRow <- if( base::is.na(extId) ) {
            # if extId NA, no change
            extRow
          } else {
            # Add new string to extId Row
            extRow <- if( base::is.na(extRow) ) {
              base::paste0(newExt)
            } else{
              base::paste0(extRow, ",", newExt)
            }
          }

        }

        external <- base::c(external, extRow)
      }
    } else {
      external <- base::rep('NA', nrow(x))
    }

    # Athlete df from original df
    a <- x$athlete

    # Remove old external from athlete df
    a <- dplyr::select(.data = a, -dplyr::starts_with('external'))

    # Bind external column to athlete df
    a <- base::cbind(a, external)

    # append Athlete prefix
    base::names(a) <- base::paste0('athlete_', base::names(a))

    ##-- Test Types --##

    # Create testType df
    t <- x$testType

    # extract tags array from data frame
    tagList <- t$tags

    # Define a function to pad empty data frames with NA values
    # and condense multiple rows into one with values separated by '|'
    pad_and_condense <- function(df) {
      if (base::is.null(df) || base::nrow(df) == 0) {
        return(base::data.frame(tagIds = NA, tagNames = NA, tagDesc = NA))
      } else {
        condensed_row <- base::data.frame(
          tagIds = base::paste(df$id, collapse = ','),
          tagNames = base::paste(df$name, collapse = ','),
          tagDesc = base::ifelse(all(df$description == ""), NA, base::paste(df$description, collapse = '|'))
        )
        return(condensed_row)
      }
    }

    # Apply the padding function to all data frames in the list
    paddedTagList <- base::lapply(tagList, pad_and_condense)

    # Combine all data frames into one
    tagsDF <- base::do.call(rbind, paddedTagList)

    # Replace new tags columns to testType df
    t <-  dplyr::select(t, -'tags')
    t <- base::cbind(t,tagsDF)

    # append testType prefix
    base::names(t) <- base::paste0('testType_', base::names(t))

    ##-- finish data frame --##

    # select trial metadata metrics from DF
    x1 <- dplyr::select(.data = x, base::c('id', 'timestamp', 'segment'))

    # select all metrics from DF
    x2 <- dplyr::select(.data = x, -base::c('id', 'timestamp', 'segment', 'testType', 'athlete'))

    # create new complete DF
    x <- base::cbind(x1, t, a, x2)

    # Clean colnames with janitor
    x <- janitor::clean_names(x)

    # filter inactive tests
    x <- if( base::isTRUE(active) ) {
      filt <- dplyr::filter(.data = x, active == TRUE)

      filt <- dplyr::relocate(.data = filt, 'active', .before = 'timestamp')

      filt
    } else if( base::isFALSE(active) ){
      x <- dplyr::relocate(.data = x, 'active', .before = 'timestamp')

      x
    }

    # return final DF
    x
  }

  #-----#

  # Return Response
  return(Resp)

}
