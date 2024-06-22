#' Check for Proper EPOCH Times
#'
#' Check `from` and `to` parameters for proper class.
#'
#' @param from from argument provided into function.
#' @param to to argument provided into function.
#' @return no object returned.
#' @importFrom stats na.omit
#' @keywords internal
EpochArgCheck <- function(arg_from, arg_to) {
  if (!is.null(arg_from) && !is.numeric(arg_from)) {
    stop("Error: `from` expecting numeric EPOCH/Unix timestamp.")
  } else if (!is.null(arg_to) && !is.numeric(arg_to)) {
    stop("Error: `to` expecting numeric EPOCH/Unix timestamp.")
  }
}


#--------------------#


#' Construct DateTime Parameters
#'
#' This is an internal function used for constructing date-time parameters
#' for API requests within the package.
#'
#' @param param The name of the parameter.
#' @param value The value of the parameter.
#' @param sync Logical indicating whether to use "sync" prefix.
#'
#' @return A string representing the constructed parameter.
#' @keywords internal
DateTimeParam <- function(param, value, sync) {

  x <- if (is.null(value)) {
    ""
  } else if (isTRUE(sync) && param == "from") {
    paste0("&syncFrom=", value)
  } else if (isTRUE(sync) && param == "to") {
    paste0("&syncTo=", value)
  } else {
    paste0("&", param, "=", value)
  }

  return(x)

}


#--------------------#


#' Construct Test Type
#'
#' Take testType section from data frame and prep for final data frame
#'
#' @param arg_df Data frame to be evaluated.
#' @return data frame of test type information
#' @keywords internal
TagPrep <- function(arg_df) {


  # Define a function to pad empty data frames with NA values
  # and condense multiple rows into one with values separated by '|'
  # Define the pad_and_condense function
  pad_and_condense <- function(df) {
    if (!is.data.frame(df) || base::is.null(df) || base::nrow(df) == 0) {
      return(base::data.frame(tagIds = NA, tagNames = NA, tagDesc = NA, stringsAsFactors = FALSE))
    } else {
      condensed_row <- base::data.frame(
        tagIds = base::paste(df$id, collapse = ","),
        tagNames = base::paste(df$name, collapse = ","),
        tagDesc = if (base::all(base::is.na(df$description) | df$description == "")) {
          NA
        } else {
          base::paste(stats::na.omit(df$description), collapse = "|")
        },
        stringsAsFactors = FALSE
      )
      return(condensed_row)
    }
  }

  # take in arg
  t <- arg_df

  # Ensure tagList is a list where each element corresponds to a row in t
  tagList <- base::lapply(t$tags, function(x) if (base::is.null(x)) base::data.frame() else x)

  # Apply the pad_and_condense function to each element of tagList
  paddedTagList <- base::lapply(tagList, pad_and_condense)

  # Combine all data frames into one
  tagsDF <- base::do.call(rbind, paddedTagList)

  # Replace new tags columns in the t data frame
  t <- dplyr::select(.data = t, -base::c("tags"))
  t <- base::cbind(t, tagsDF)

  # append testType prefix
  base::names(t) <- base::paste0("testType_", base::names(t))

  return(t)
}


#--------------------#


#' Construct Athlete df
#'
#' Take athlete section from data frame and prep for final data frame
#'
#' @param arg_df Data frame to be evaluated.
#' @return data frame of test type information
#' @keywords internal
AthletePrep <- function(arg_df) {
  # Athlete df from original df
  a <- arg_df

  # Create externalId df
  extDF <- a$external

  # Prepare externalId vector
  external <- base::c()

  # Identify External Ids
  if (base::ncol(extDF) > 0) {
    # Loop externalId columns
    for (i in seq_len(base::nrow(extDF))) {

      extRow <- NA

      for (n in seq_len(base::ncol(extDF))) {

        # get externalId name
        extN <- base::names(extDF)[n]

        # get ext id
        extId <- extDF[i, n]

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

      external <- base::c(external, extRow)
    }
  } else {
    external <- base::rep("NA", base::nrow(a))
  }

  # Remove old external from athlete df
  a <- dplyr::select(.data = a, -dplyr::starts_with("external"))

  # Bind external column to athlete df
  a <- base::cbind(a, external)

  # append Athlete prefix
  base::names(a) <- base::paste0("athlete_", base::names(a))

  return(a)
}


#--------------------#


#' Check Test Id
#'
#' Take testId argument and assess for correct format and validate before API call
#'
#' @param arg.id the testId argument provided in the function
#' @return testId or error
#' @importFrom dplyr filter
#' @keywords internal
testIdCheck <- function(arg_id) {

  # Create the data frame
  type_df <- base::data.frame(
    id = c(
      "7nNduHeM5zETPjHxvm7s", "QEG7m7DhYsD6BrcQ8pic", "2uS5XD5kXmWgIZ5HhQ3A",
      "gyBETpRXpdr63Ab2E0V8", "5pRSUQVSJVnxijpPMck3", "pqgf2TPUOQOQs6r0HQWb",
      "r4fhrkPdYlLxYQxEeM78", "ubeWMPN1lJFbuQbAM97s", "rKgI4y3ItTAzUekTUpvR"
    ),
    name = c(
      "Countermovement Jump", "Squat Jump", "Isometric Test", "Drop Jump",
      "Free Run", "CMJ Rebound", "Multi Rebound", "Weigh In", "Drop Landing"
    ),
    abbreviation = c("CMJ", "SJ", "ISO", "DJ", "FR", "CMJR", "MR", "WI", "DL")
  )

  # Check typeId and extract corresponding id
  filtered_df <- dplyr::filter(type_df, .data$id == arg_id | .data$name == arg_id | .data$abbreviation == arg_id)

  a <- if (nrow(filtered_df) > 0) {
    tId <- filtered_df$id[1]

    tId
  } else {
    stop("Error: typeId incorrect. Check your entry")
  }

  return(a)
}

#--------------------#


#' Athlete Data Frame to JSON
#'
#' Take the athlete data frame passed and convert to JSON for POST/PATCH method payload
#'
#' @param arg_df the testId argument provided in the function
#' @return JSON string
#' @importFrom jsonlite toJSON
#' @keywords internal
AthleteJSON <- function(arg_df) {
  # Create blank list for athletes
  x <- list()

  for (i in seq_len(nrow(arg_df))) {
    # create list with required name
    ath <- list(
      name = arg_df$name[i]
    )

    # Check for IMAGE column
    if ("image" %in% base::names(arg_df)) {
      if (!is.na(arg_df$image[i])) {
        ath$image <- arg_df$image[i]
      }
    }

    # Check for ACTIVE column
    if ("active" %in% base::names(arg_df)) {
      if (!is.na(arg_df$active[i])) {
        ath$active <- arg_df$active[i]
      }
    }

    # Check for TEAMS column
    if ("teams" %in% base::names(arg_df)) {
      if (!is.na(arg_df$teams[i])) {
        ath$teams <- base::ifelse(is.list(arg_df$teams[i]), arg_df$teams[i], list(arg_df$teams[i]))
      }
    }

    # Check for GROUPS column
    if ("groups" %in% base::names(arg_df)) {
      if (!is.na(arg_df$groups[i])) {
        ath$groups <- base::ifelse(is.list(arg_df$groups[i]), arg_df$groups[i], list(arg_df$groups[i]))
      }
    }

    # Create external list
    ath$external <- list()

    # Handle columns that are not "name", "image", "active", "teams", "groups"
    other_columns <- base::setdiff(base::names(arg_df), c("name", "image", "active", "teams", "groups"))

    for (column in other_columns) {
      if (!is.na(arg_df[[column]][i])) {
        ath$external[[column]] <- arg_df[[column]][i]
      }
    }

    x <- base::append(x, list(ath))
  }

  # Convert lists to JSON format
  y <- jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE)

  return(y)
}