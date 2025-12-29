# Internal Utility Functions
#--------------------#

#' @title Utility Functions
#' @description Internal helper functions for parameter validation and data cleaning.


#--------------------#

#' Check for interactive mode for sensitive prompts
#' @noRd
check_interactive <- function() {
  if (!interactive()) {
    stop("This function requires an interactive session to securely enter credentials.", call. = FALSE)
  }
}


#--------------------#


#' Validate Timestamp
#' @param x Date string or numeric timestamp
#' @return Numeric Unix timestamp
#' @noRd
validate_timestamp <- function(x) {
  logger::log_trace("hawkinR/utils -> validate_timestamp: input type={class(x)[1]}")
  if (is.null(x)) return(NULL)
  if (is.numeric(x)) return(x)
  if (is.character(x)) {
    # Attempt conversion from YYYY-MM-DD
    ts <- tryCatch(as.POSIXct(x), error = function(e) NA)
    if (is.na(ts)) stop("Invalid date format. Use 'YYYY-MM-DD' or Unix timestamp.", call. = FALSE)
    return(as.numeric(ts))
  }
  stop("Timestamp must be numeric or character string.", call. = FALSE)
}


#--------------------#


#' Validate GetTest Parameters
#'
#' Check `athleteId`, `testTypeId`, `teamId`, and `groupId` parameters.
#'
#' @return no object returned.
#' @importFrom stats na.omit
#' @keywords internal
#' @noRd
ParamValidation <- function(arg_athleteId = NULL, arg_testTypeId = NULL, arg_teamId = NULL, arg_groupId = NULL) {
  logger::log_trace("hawkinR/utils -> ParamValidation: validating query parameters")

  # 1. Validate Parameter Classes
  # Athlete Id
  if (!is.null(arg_athleteId) && !is.character(arg_athleteId)) {
    stop("Error: athleteId should be a character string of an athlete ID. Example: 'athleteId'")
  }
  # Test Type Id
  if (!is.null(arg_testTypeId)) {
    if (!is.character(arg_testTypeId)) {
      stop("Error: typeId incorrect. Check your entry")
    }

    # NEW: Validate that the ID actually maps to a known test type
    validated_id <- TestIdCheck(arg_testTypeId)
    if (validated_id == "") {
      stop(paste0("Error: Unknown test type '", arg_testTypeId,
                  "'. Please check the name or abbreviation."), call. = FALSE)
    }
  }
  # Team Id
  if (!is.null(arg_teamId) && !(is.character(arg_teamId) || is.list(arg_teamId))) {
    stop("Error: teamId should be a character string or a list of team IDs.")
  }
  # Group Id
  if (!is.null(arg_groupId) && !(is.character(arg_groupId) || is.list(arg_groupId))) {
    stop("Error: groupId should be a character string or a list of group IDs.")
  }

  # Validate that only one or none of athleteId, testTypeId, teamId, or groupId is provided
  is_active <- function(id) {
    if (is.null(id)) {
      return(FALSE)
    }
    if (!is.null(id)) {
      return(TRUE)
    }
  }

  # List of params provided
  ids <- list(arg_athleteId, arg_testTypeId, arg_teamId, arg_groupId)

  # Remove nulls and empty lists/strings from list
  provided_ids <- sapply(ids, is_active)

  # Count active ids and validate no more than 1 exist
  if (sum(provided_ids) > 1) {
    stop("You can only specify one or none of 'athleteId', 'testTypeId', 'teamId', or 'groupId'.")
  }

  logger::log_debug("hawkinR/utils -> ParamValidation: {sum(provided_ids)} filter(s) active")
}


#--------------------#


#' Construct Test Type
#'
#' Take testType section from data frame and prep for final data frame
#'
#' @param arg_df Data frame to be evaluated.
#' @return data frame of test type information
#' @keywords internal
#' @noRd
TestTypePrep <- function(arg_df) {
  logger::log_trace("hawkinR/utils -> TestTypePrep: processing {nrow(arg_df)} test type rows")

  # 1. Separate Test Type Columns from Tags
  testTypeData <- arg_df[1:3]
  TestTypeData <-

  base::colnames(testTypeData) <- c("testType_uuid", "testType_name", "testType_canonicalId")

  # 2. Create Empty Tags Data frame
  testType_tags_id <- rep(NA, nrow(arg_df))
  testType_tags_name <- rep(NA, nrow(arg_df))
  testType_tags_desc <- rep(NA, nrow(arg_df))

  tagsData <- base::data.frame(
    testType_tags_id,
    testType_tags_name,
    testType_tags_desc
  )

  # 3. Loop rows to find cases with tags and apply to Tags data frame
  for (row in 1:nrow(arg_df)) {
    # Check if the 4th column (tags) is not NULL and is a data frame
    if (!is.null(arg_df[[row, 4]]) && is.data.frame(arg_df[[row, 4]]) && base::nrow(arg_df[[row, 4]]) > 0) {
      # Isolate Row with nested data frame
      t <- arg_df[[row, 4]]
      # extract and apply tag ID
      tagsData$testType_tags_id[[row]] <- base::list(t$id)
      # extract and apply tag name
      tagsData$testType_tags_name[[row]] <- base::list(t$name)
      # extract and apply tag desc
      tagsData$testType_tags_desc[[row]] <- base::list(t$description)
    }
  }

  # 4. Use unwrap function to remove tags values from lists
  unwrap <- function(x) {
    if (base::is.list(x)) {
      base::lapply(x, function(y) if (length(y) == 1) y[[1]] else y)
    } else {
      x
    }
  }

  # Unwrap tag IDs
  tagsData$testType_tags_id <- unwrap(tagsData$testType_tags_id)
  # Unwrap tag names
  tagsData$testType_tags_name <- unwrap(tagsData$testType_tags_name)
  # Unwrap tag desc
  tagsData$testType_tags_desc <- unwrap(tagsData$testType_tags_desc)

  # 5. Combine Tag data back to Test Type data
  tags_found <- sum(!is.na(tagsData$testType_tags_id))
  logger::log_trace("hawkinR/utils -> TestTypePrep: {tags_found} rows with tags extracted")
  return(base::cbind(testTypeData, tagsData))
}


#--------------------#


#' Construct Athlete df
#'
#' Take athlete section from data frame and prep for final data frame
#'
#' @param arg_df Data frame to be evaluated.
#' @return data frame of test type information
#' @keywords internal
#' @noRd
AthletePrep <- function(arg_df) {
  logger::log_trace("hawkinR/utils -> AthletePrep: processing {nrow(arg_df)} athlete rows")

  # 1. Isolate Expected Athlete Columns from Athlete Section
  athleteData <- arg_df[1:5]
  base::colnames(athleteData) <- base::paste0("athlete_", base::colnames(athleteData))

  # 2. Check for External
  externalData <- arg_df[[6]]

  # 3. Reformat and Add External Data if Present
  if (ncol(externalData) > 0) {
    logger::log_trace("hawkinR/utils -> AthletePrep: {ncol(externalData)} external properties found")
    # A. Reformat External Data names
    externalData <- janitor::clean_names(externalData)
    base::colnames(externalData) <- base::paste0("athlete_", base::colnames(externalData))

    # B. Combine Basic Athlete and External data
    return(base::cbind(athleteData, externalData))
  } else {
    # A. Return Basic Athlete Data
    return(athleteData)
  }
}


#--------------------#


#' Check Test Id
#'
#' Take testId argument and assess for correct format and validate before API call
#'
#' @param arg_id the testId argument provided in the function
#' @return testId or error
#' @importFrom dplyr filter
#' @keywords internal
#' @noRd
TestIdCheck <- function(arg_id) {
  logger::log_trace("hawkinR/utils -> TestIdCheck: resolving '{arg_id}'")

  # Create the data frame
  type_df <- base::data.frame(
    id = c(
      "7nNduHeM5zETPjHxvm7s", "QEG7m7DhYsD6BrcQ8pic", "2uS5XD5kXmWgIZ5HhQ3A",
      "gyBETpRXpdr63Ab2E0V8", "5pRSUQVSJVnxijpPMck3", "pqgf2TPUOQOQs6r0HQWb",
      "r4fhrkPdYlLxYQxEeM78", "ubeWMPN1lJFbuQbAM97s", "rKgI4y3ItTAzUekTUpvR",
      "4KlQgKmBxbOY6uKTLDFL", "umnEZPgi6zaxuw0KhUpM"
    ),
    name = c(
      "Countermovement Jump", "Squat Jump", "Isometric Test", "Drop Jump",
      "Free Run", "CMJ Rebound", "Multi Rebound", "Weigh In", "Drop Landing",
      "TS Free Run", "TS Isometric Test"
    ),
    abbreviation = c("CMJ", "SJ", "ISO", "DJ", "FR", "CMJR", "MR", "WI", "DL","TSFR","TSISO")
  )

  # Check typeId and extract corresponding id
  filtered_df <- dplyr::filter(type_df, .data$id == arg_id | .data$name == arg_id | .data$abbreviation == arg_id)

  if (nrow(filtered_df) > 0) {
    tId <- filtered_df$id[1]
    logger::log_trace("hawkinR/utils -> TestIdCheck: resolved to '{tId}'")
    return(tId)
  } else {
    stop("Error: typeId incorrect. Check your entry")
  }
}


#--------------------#


#' Add Athlete Data Frame to JSON
#'
#' Take the athlete data frame passed and convert to JSON for POST method payload
#'
#' @param arg_df the athlete data frame argument provided in the function
#' @return JSON string
#' @importFrom jsonlite toJSON
#' @keywords internal
#' @noRd
AddAthleteJSON <- function(arg_df) {
  logger::log_trace("hawkinR/utils -> AddAthleteJSON: converting {nrow(arg_df)} athletes")
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

  logger::log_debug("hawkinR/utils -> AddAthleteJSON: payload {nchar(y)} bytes")
  return(y)
}


#--------------------#


#' Update Athlete Data Frame to JSON
#'
#' Take the athlete data frame passed and convert to JSON for PUT method payload
#'
#' @param arg_df the athlete data frame argument provided in the function
#' @return JSON string
#' @importFrom jsonlite toJSON
#' @keywords internal
#' @noRd
UpdateAthleteJSON <- function(arg_df) {
  logger::log_trace("hawkinR/utils -> UpdateAthleteJSON: converting {nrow(arg_df)} athletes")
  # Create blank list for athletes
  x <- list()

  if ("id" %in% base::names(arg_df)) {
    for (i in seq_len(nrow(arg_df))) {
      # create list with required id
      ath <- list(
        id = arg_df$id[i]
      )

      # Check for NAME column
      if ("name" %in% base::names(arg_df)) {
        if (!is.na(arg_df$name[i])) {
          ath$name <- arg_df$name[i]
        }
      }

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
      other_columns <- base::setdiff(base::names(arg_df), c("id","name", "image", "active", "teams", "groups"))

      for (column in other_columns) {
        if (!is.na(arg_df[[column]][i])) {
          ath$external[[column]] <- arg_df[[column]][i]
        }
      }

      x <- base::append(x, list(ath))
    }

    # Convert lists to JSON format
    y <- jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE)

    logger::log_debug("hawkinR/utils -> UpdateAthleteJSON: payload {nchar(y)} bytes")
    return(y)
  } else {
    stop(logger::log_error("athleteData must contain ID column"))
  }
}


#--------------------#


#' Flatten Nested Lists in Athlete Test Output
#'
#' This function takes a data frame of athlete test output, which may contain
#' nested lists or tables, and flattens them into a simple data frame. It works
#' specifically on the columns that contain lists or other complex structures.
#'
#' @param arg_df A data frame containing athlete test data, including columns with nested lists or tables.
#' @return A data frame where nested lists or tables have been flattened, making it easier to manipulate.
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @keywords internal
#' @noRd
dfTests_flat <- function(arg_df) {
  logger::log_trace("hawkinR/utils -> dfTests_flat: flattening {nrow(arg_df)} rows")

  # Supplied data frame
  df <- arg_df

  # Columns to flatten
  flatten_cols <- c(
    "testType_tags_id",
    "testType_tags_name",
    "testType_tags_desc",
    "athlete_teams",
    "athlete_groups"
  )

  # Add missing columns as empty vectors
  for (col in flatten_cols) {
    if (!col %in% base::colnames(df)) {
      df[[col]] <- NA_character_  # Add column with NA as placeholder
    }
  }

  # Mutate Selected Columns to single comma-separated character strings
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(flatten_cols),
        ~ base::sapply(., function(x) paste(unlist(x), collapse = ","))
      )
    )

  logger::log_trace("hawkinR/utils -> dfTests_flat: complete")
  return(df)
}


#--------------------#


#' Expand Comma-Separated Values To Nested Lists
#'
#' This function takes a data frame of athlete test output, which contains
#' comma-separated strings (test tag name, test tag id, test tag description,
#' athlete team, and athlete group), and expands them into nested lists.
#'
#' @param arg_df A data frame containing flattened athlete test data.
#' @return A data frame where the specified columns have been converted to nested lists.
#' @importFrom dplyr mutate across
#' @keywords internal
#' @noRd
dfTests_expand <- function(arg_df) {
  logger::log_trace("hawkinR/utils -> dfTests_expand: expanding {nrow(arg_df)} rows")

  # Supplied data frame
  df <- arg_df

  # Revert the specified columns back to lists using column names
  df <- df %>%
    mutate(
      across(
        c("testType_tags_id",
          "testType_tags_name",
          "testType_tags_desc",
          "athlete_teams",
          "athlete_groups"),
        ~ strsplit(., ",")
      )
    )

  logger::log_trace("hawkinR/utils -> dfTests_expand: complete")
  return(df)
}


#--------------------#


#' Convert Date-Time formats to Character Strings
#'
#' This function takes a data frame of test trials and searches for any columns
#' with a date class. Then it will convert them to a character class.
#'
#' @param arg_df A data frame containing flattened athlete test data.
#' @return A data frame where the specified columns have been converted to nested lists.
#' @importFrom dplyr mutate across
#' @importFrom tidyselect where
#' @keywords internal
#' @noRd
dfDatetoChar <- function(arg_df) {
  logger::log_trace("hawkinR/utils -> dfDatetoChar: converting date columns")

  # Supplied data frame
  df <- arg_df

  # Identify date formats and convert to character
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(~ base::inherits(., c("Date", "POSIXct", "POSIXt"))), as.character
      )
    )

  # Convert any columns that start with 'athlete_' (excluding specific columns) to character
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("athlete_") &
          !dplyr::all_of(c("athlete_teams", "athlete_groups", "athlete_active")),
        as.character
      )
    )

  logger::log_trace("hawkinR/utils -> dfDatetoChar: complete")
  return(df)
}


#--------------------#
