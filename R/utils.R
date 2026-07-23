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


#' Seconds Until Access Token Expiry
#'
#' Returns the number of seconds remaining on the active access token. Guards
#' against a missing, `NULL`, or non-`POSIXct` expiration so callers never hit
#' `missing value where TRUE/FALSE needed` when deciding whether to refresh.
#'
#' @param conn A `HawkinAuth` connection object.
#' @return Numeric seconds remaining (may be negative if already expired).
#' @keywords internal
#' @noRd
token_seconds_remaining <- function(conn) {
  exp <- conn@expires_at
  if (base::is.null(exp) || !base::inherits(exp, "POSIXct") || base::anyNA(exp)) {
    stop(
      "No valid access token expiration found. Please run `hd_connect()` to (re)authenticate.",
      call. = FALSE
    )
  }
  base::round(base::as.numeric(base::difftime(exp, base::Sys.time(), units = "secs")))
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
#' Take an athlete section (from `get_athletes()` or the nested `athlete` block
#' of `get_tests()`) and reshape it into a flat data frame.
#'
#' Columns are selected by **name**, never by position, so responses that omit
#' optional fields or reorder columns are handled gracefully. The athlete profile
#' fields added in API v1.14 (`image`, `position`, `dob`, `sport`, `height`,
#' `lastTestedOn`) are included when present.
#'
#' External (custom) properties are unnested into one column per key. The API may
#' return `external` either as a uniform sub-data-frame (every athlete shares the
#' same keys) or as a list-column (keys vary across athletes). Both shapes are
#' handled; athletes missing a given key receive `NA`.
#'
#' @param arg_df Data frame (or coercible list) to be evaluated.
#' @param prefix Character prefix applied to every output column. Defaults to
#'   `"athlete_"` for the `get_tests()` path; pass `""` for `get_athletes()`.
#' @return data frame of athlete information
#' @keywords internal
#' @noRd
AthletePrep <- function(arg_df, prefix = "athlete_") {

  # 0. Defensive guards — empty or non-data-frame input
  if (base::is.null(arg_df) || base::length(arg_df) == 0L) {
    return(base::data.frame())
  }
  if (!base::is.data.frame(arg_df)) {
    # Replace NULL / zero-length elements with NA so columns line up
    if (base::is.list(arg_df)) {
      arg_df <- base::lapply(arg_df, function(x) {
        if (base::is.null(x) || base::length(x) == 0L) NA else x
      })
    }
    arg_df <- base::as.data.frame(arg_df, stringsAsFactors = FALSE)
  }

  logger::log_trace("hawkinR/utils -> AthletePrep: processing {nrow(arg_df)} athlete rows")

  # 1. Select known athlete columns by NAME (API v1.14 schema)
  core_cols    <- c("id", "name", "active", "teams", "groups", "image")
  profile_cols <- c("position", "dob", "sport", "height", "lastTestedOn")

  present_core    <- base::intersect(core_cols,    base::names(arg_df))
  present_profile <- base::intersect(profile_cols, base::names(arg_df))

  known_df <- arg_df[, c(present_core, present_profile), drop = FALSE]
  base::colnames(known_df) <- base::paste0(prefix, base::colnames(known_df))

  # 2. No external block -> return the known columns
  if (!"external" %in% base::names(arg_df)) {
    return(known_df)
  }

  externalData <- arg_df[["external"]]
  ext_df <- NULL

  # 3. Normalize external to a data frame, handling both shapes
  if (base::is.data.frame(externalData) && base::ncol(externalData) > 0L) {
    # Uniform keys across athletes -> already a sub-data-frame
    ext_df <- externalData

  } else if (base::is.list(externalData)) {
    # Varying keys across athletes -> union of keys, NA-filled per athlete
    ext_keys <- base::unique(base::unlist(base::lapply(externalData, base::names)))
    ext_keys <- ext_keys[!base::is.na(ext_keys)]

    if (base::length(ext_keys) > 0L) {
      ext_df <- base::as.data.frame(
        base::lapply(ext_keys, function(k) {
          base::vapply(externalData, function(row_ext) {
            if (base::is.null(row_ext) || base::is.null(row_ext[[k]])) {
              NA_character_
            } else {
              base::as.character(row_ext[[k]])
            }
          }, character(1))
        }),
        stringsAsFactors = FALSE
      )
      base::colnames(ext_df) <- ext_keys
    }
  }

  # 4. Combine if we have external data, with cleaned + prefixed names
  if (!base::is.null(ext_df) && base::ncol(ext_df) > 0L) {
    logger::log_trace("hawkinR/utils -> AthletePrep: {ncol(ext_df)} external properties found")
    ext_df <- janitor::clean_names(ext_df)
    base::colnames(ext_df) <- base::paste0(prefix, base::colnames(ext_df))
    combined_df <- base::cbind(known_df, ext_df)

    # Disambiguate any external property whose name collides with a known
    # column (e.g. an external "position" -> <prefix>position clashes with the
    # profile column). Left unguarded, the duplicate names propagate downstream
    # and trigger "Can't transform a data frame with duplicate names." The
    # external copy is suffixed (e.g. athlete_position.1).
    base::colnames(combined_df) <- base::make.unique(base::colnames(combined_df))

    return(combined_df)
  }

  return(known_df)
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
    logger::log_error("hawkinR/utils -> UpdateAthleteJSON: athleteData must contain 'id' column")
    stop("athleteData must contain 'id' column", call. = FALSE)
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

#' Sanitize Chunks for Binding
#'
#' Ensures consistent column types across a list of data frames before binding.
#' Specifically handles the conflict between logical NAs and List columns.
#'
#' @param chunks A list of data frames
#' @return A list of data frames with consistent list columns
#' @keywords internal
#' @noRd
sanitize_chunks <- function(chunks) {
  # If list is empty or has 1 item, no conflict possible
  if (length(chunks) < 2) return(chunks)

  # 1. Identify all column names across all chunks
  all_cols <- unique(unlist(lapply(chunks, names)))

  # 2. Find which columns are lists in AT LEAST one chunk
  list_cols <- all_cols[vapply(all_cols, function(col) {
    any(vapply(chunks, function(df) {
      col %in% names(df) && inherits(df[[col]], "list")
    }, logical(1)))
  }, logical(1))]

  if (length(list_cols) > 0) {
    logger::log_trace("hawkinR/utils -> sanitize_chunks: enforcing list type for {paste(list_cols, collapse=', ')}")
    chunks <- lapply(chunks, function(df) {
      for (col in list_cols) {
        # If chunk has the column AND it's not a list (likely logical NA), coerce it
        if (col %in% names(df) && !inherits(df[[col]], "list")) {
          df[[col]] <- as.list(df[[col]])
        }
      }
      return(df)
    })
  }

  # 3. Harmonize scalar-type mismatches across pages. When one page has
  #    `athlete_unique_id` as character and another has it as double (this
  #    happens with mixed numeric / alphanumeric external IDs across the
  #    org's athletes), dplyr::bind_rows refuses to combine them. Coerce
  #    the mismatched column to character on every page — the safest
  #    common type for ID-like columns.
  non_list_cols <- setdiff(all_cols, list_cols)
  for (col in non_list_cols) {
    types <- unique(vapply(chunks, function(df) {
      if (col %in% names(df)) typeof(df[[col]]) else NA_character_
    }, character(1)))
    types <- types[!is.na(types) & types != "logical"]  # logical NA is safely auto-coerced
    if (length(types) > 1) {
      logger::log_trace("hawkinR/utils -> sanitize_chunks: unifying '{col}' across types [{paste(types, collapse='/')}] as character")
      chunks <- lapply(chunks, function(df) {
        if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
        df
      })
    }
  }

  return(chunks)
}
