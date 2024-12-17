#--------------------#


#' Check Authentication Validity
#'
#' Takes the current token expiration and validates it
#' @keywords internal
check_token_validity <- function() {
  token_expiration <- as.numeric(Sys.getenv("accessToken_expiration"))
  if (Sys.time() > as.POSIXct(token_expiration, origin = "1970-01-01")) {
    Sys.setenv(accessToken_valid = FALSE)
  } else {
    Sys.setenv(accessToken_valid = TRUE)
  }
  invisible(NULL)  # To avoid printing
}


#--------------------#


#' Monitor Authentication Validity
#'
#' Looped checks of access taken validation
#'
#' @keywords internal
monitor_token <- function() {
  tryCatch(
    {
      check_token_validity()  # Perform the validity check
      later::later(monitor_token, 5)  # Schedule the next check
    },
    error = function(e) {
      message("An error occurred in monitor_token: ", e$message)
      # Optionally retry scheduling in case of recoverable errors
      later::later(monitor_token, 5)
    }
  )
}


#--------------------#


#' Validate time stamp arguments and allow for date character
#'
#' This function checks `from` and `to` arguments for valid inputs of epoch or
#' a date as a character string.
#'
#' @param x A data frame containing flattened athlete test data.
#' @return An epoch time stamp
#' @keywords internal
validate_timestamp <- function(x) {

  # Helper function to check if a string is in "yyyy-mm-dd" format
  is_valid_date <- function(date_str) {
    tryCatch({
      base::as.Date(date_str, format = "%Y-%m-%d")
      TRUE
    }, error = function(e) {
      FALSE
    })
  }

  # If argument is numeric, assume it's an epoch timestamp
  if (base::is.numeric(x)) {
    return(x)
  }

  # If argument is a character, check if it's a valid date string
  if (base::is.character(x)) {
    if (is_valid_date(x)) {
      # Convert date string to epoch timestamp
      d <- base::as.numeric(
        base::as.POSIXct(x, format = "%Y-%m-%d", tz = "UTC")
      )

      # Check for incorrect format
      if (base::is.na(d)) {
        stop(
          logger::log_error(
            paste("Error: The argument is not a valid date in 'yyyy-mm-dd' format.")
          )
        )
      } else {
        return(d)
      }

    } else {
      stop(
        logger::log_error(
          paste("Error: The argument is not a valid date in 'yyyy-mm-dd' format.")
        )
      )
    }
  }

  # If neither, return an error
  stop(
    logger::log_error(
      paste(
        "Error: The argument is neither a valid epoch timestamp nor a valid date string."
      )
    )
  )
}


#--------------------#


#' Validate GetTest Parameters
#'
#' Check `from`, `to`, `athleteId`, `testTypeId`, `teamId`, and `groupId` parameters.
#'
#' @param from from argument provided into function.
#' @param to to argument provided into function.
#' @return no object returned.
#' @importFrom stats na.omit
#' @keywords internal
ParamValidation <- function(arg_from, arg_to, arg_athleteId = NULL, arg_testTypeId = NULL, arg_teamId = NULL, arg_groupId = NULL) {

  # 1. Validate Parameter Classes
  # From
  if (!is.null(arg_from) && !is.numeric(arg_from)) {
    stop("Error: `from` expecting numeric EPOCH/Unix timestamp.")
  }
  # To
  if (!is.null(arg_to) && !is.numeric(arg_to)) {
    stop("Error: `to` expecting numeric EPOCH/Unix timestamp.")
  }
  # Athlete Id
  if (!is.null(arg_athleteId) && !is.character(arg_athleteId)) {
    stop("Error: athleteId should be a character string of an athlete ID. Example: 'athleteId'")
  }
  # Test Type Id
  if (!is.null(arg_testTypeId) && !is.character(arg_testTypeId)) {
    stop("Error: typeId incorrect. Check your entry")
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
}


#--------------------#


#' Construct Test Type
#'
#' Take testType section from data frame and prep for final data frame
#'
#' @param arg_df Data frame to be evaluated.
#' @return data frame of test type information
#' @keywords internal
TestTypePrep <- function(arg_df) {

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
AthletePrep <- function(arg_df) {

  # 1. Isolate Expected Athlete Columns from Athlete Section
  athleteData <- arg_df[1:5]
  base::colnames(athleteData) <- base::paste0("athlete_", base::colnames(athleteData))

  # 2. Check for External
  externalData <- arg_df[[6]]

  # 3. Reformat and Add External Data if Present
  if (ncol(externalData) > 0) {
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
TestIdCheck <- function(arg_id) {

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
AddAthleteJSON <- function(arg_df) {
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


#--------------------#


#' Update Athlete Data Frame to JSON
#'
#' Take the athlete data frame passed and convert to JSON for PUT method payload
#'
#' @param arg_df the athlete data frame argument provided in the function
#' @return JSON string
#' @importFrom jsonlite toJSON
#' @keywords internal
UpdateAthleteJSON <- function(arg_df) {
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
dfTests_flat <- function(arg_df) {

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
dfTests_expand <- function(arg_df) {

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
dfDatetoChar <- function(arg_df) {

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

  return(df)
}


#--------------------#


#' Clean Test Metric headers
#'
#' This function calls metric ids from the metric dictionary to clean metric name headers for test outputs.
#'
#' @param df A data frame containing flattened test metric data.
#' @param testType the testType of the query
#' @return A data frame with cleaned headers
#' #' @importFrom tidyselect everything
#' @keywords internal

# Function to clean test metric headers
replace_headers <- function(df, typeId) {
  # Get the metrics lookup table
  metrics_lookup <- if (!base::is.null(typeId)) {
    get_metrics(testType = typeId)
  } else {
    get_metrics(testType = "all")
  }

  # Create a named vector for lookup: id as the value, header as the name
  header_lookup <- stats::setNames(metrics_lookup$id, metrics_lookup$label_unit)

  # Trial Metrics
  trialMetrics <- df

  # Deduplicate column mapping to keep only the first occurrence
  deduplicated_lookup <- header_lookup[!duplicated(header_lookup)]

  # Check for missing headers
  missing_headers <- setdiff(names(trialMetrics), names(deduplicated_lookup))
  if (length(missing_headers) > 0) {
    warning("The following columns have no match in the header lookup: ", paste(missing_headers, collapse = ", "))
  }

  # Rename columns using deduplicated lookup
  trialMetrics <- trialMetrics %>%
    dplyr::rename_with(
      ~ ifelse(!is.na(deduplicated_lookup[.]), deduplicated_lookup[.], .),
      .cols = tidyselect::everything()
    )

  return(trialMetrics)
}




