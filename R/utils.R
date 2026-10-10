# Internal Utility Functions
#--------------------#

#' @title Utility Functions
#' @description Internal helper functions for parameter validation and data cleaning.


#--------------------#

#' Identify this client to the Hawkin API
#'
#' Every integration authenticates with an org-scoped token, so the API can
#' already attribute a call to an organisation. It cannot tell the clients
#' apart, because each one sends only an Authorization header. This label
#' makes "which integrations are used, and how often" answerable server-side.
#'
#' It carries the package and its version, and nothing about the user.
#'
#' @return A string like `"hawkinr/2.1.0"`.
#' @noRd
hd_client_id <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("hawkinR")),
    error = function(e) "unknown"
  )
  paste0("hawkinr/", version)
}


#--------------------#

#' Start a request that identifies itself to the Hawkin API
#'
#' Use in place of `httr2::request()` so no call site can forget the header.
#'
#' @param url The request URL.
#' @return An `httr2` request carrying the client header.
#' @importFrom httr2 request req_headers
#' @noRd
hd_request <- function(url) {
  httr2::req_headers(
    httr2::request(url),
    `X-Hawkin-Client` = hd_client_id()
  )
}


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


# Athlete write helpers -----
#
# The athlete write endpoints (POST/PUT /athletes and /athletes/bulk) take
# these profile fields as native body properties. They are never sent inside
# `external`, which is a separate property bag that update replaces wholesale.
athlete_profile_fields <- c("position", "sport", "dob", "height")

# Columns get_athletes() returns that the write endpoints ignore. They are kept
# out of both the body and `external`, so a get -> modify -> update round trip
# does not invent external properties.
athlete_read_only_fields <- c("lastTestedOn")


#' Format an Athlete Text Field for a Write Payload
#'
#' @param x A single value from a `position` or `sport` column.
#' @return The trimmed string, or `NULL` when `x` is `NA` or blank, so the
#'   field is omitted and the API leaves it unset (create) or unchanged
#'   (update).
#' @keywords internal
#' @noRd
athlete_text_value <- function(x) {
  if (base::length(x) != 1L || base::is.na(x)) {
    return(NULL)
  }
  x <- base::trimws(base::as.character(x))
  if (base::nzchar(x)) x else NULL
}


#' Format an Athlete Date of Birth for a Write Payload
#'
#' The API takes `"YYYY"` or `"YYYY-MM-DD"` and stores the string verbatim.
#' `Date` and date-time values are formatted as `YYYY-MM-DD` in their own time
#' zone (a `POSIXct`'s `tzone` attribute, or the session time zone when it has
#' none). They are never converted to UTC first, so the calendar day the user
#' sees is the day that is sent. A whole number such as `1998` is sent as
#' `"1998"`. Character values are trimmed and passed through for the API to
#' validate.
#'
#' @param x A single value from a `dob` column.
#' @return A string, or `NULL` when `x` is `NA` or blank.
#' @keywords internal
#' @noRd
athlete_dob_value <- function(x) {
  if (base::length(x) != 1L || base::is.na(x)) {
    return(NULL)
  }
  if (base::inherits(x, c("Date", "POSIXt"))) {
    return(base::format(x, "%Y-%m-%d"))
  }
  if (base::is.numeric(x) && x == base::round(x)) {
    return(base::sprintf("%.0f", x))
  }
  athlete_text_value(x)
}


#' Format an Athlete Height for a Write Payload
#'
#' @param x A single value from a `height` column, in centimeters. Numeric
#'   strings are accepted.
#' @return The height rounded to 1 decimal place (the precision the API reads
#'   back), or `NULL` when `x` is `NA` or blank.
#' @keywords internal
#' @noRd
athlete_height_value <- function(x) {
  if (base::length(x) != 1L || base::is.na(x)) {
    return(NULL)
  }
  if (!base::is.numeric(x)) {
    text <- athlete_text_value(x)
    if (base::is.null(text)) {
      return(NULL)
    }
    x <- base::suppressWarnings(base::as.numeric(text))
    if (base::is.na(x)) {
      stop(
        "height must be a number in centimeters; got '", text, "'.",
        call. = FALSE
      )
    }
  }
  # 0 means "not provided" to the API; leave it out like a blank cell.
  if (x == 0) {
    return(NULL)
  }
  base::round(x, 1)
}


#' Collect the Native Profile Fields for One Athlete
#'
#' @param arg_df The athlete data frame.
#' @param i Row index.
#' @return A named list holding only the profile fields that have a value.
#' @keywords internal
#' @noRd
athlete_profile_values <- function(arg_df, i) {
  formatters <- list(
    position = athlete_text_value,
    sport = athlete_text_value,
    dob = athlete_dob_value,
    height = athlete_height_value
  )
  out <- list()
  for (field in base::intersect(athlete_profile_fields, base::names(arg_df))) {
    value <- formatters[[field]](arg_df[[field]][i])
    if (!base::is.null(value)) {
      out[[field]] <- value
    }
  }
  out
}


#' Choose the Columns Sent as External Properties
#'
#' Every column that is not a body field, a native profile field or a
#' read-only field becomes an external property.
#'
#' `get_athletes()` renames an external property that shares a name with a
#' native column using a `make.unique()` suffix, so an old external `position`
#' comes back as `position.1`. Sending it back would either write a profile
#' field into `external` or create a junk `position.1` key, so it is dropped
#' with a warning.
#'
#' @param arg_df The athlete data frame.
#' @param body_cols Columns the caller already writes to the body.
#' @return Character vector of column names.
#' @keywords internal
#' @noRd
athlete_external_columns <- function(arg_df, body_cols) {
  reserved <- c(athlete_profile_fields, athlete_read_only_fields)
  cols <- base::setdiff(base::names(arg_df), c(body_cols, reserved))

  pattern <- base::paste0(
    "^(", base::paste(reserved, collapse = "|"), ")\\.[0-9]+$"
  )
  suffixed <- cols[base::grepl(pattern, cols)]
  populated <- suffixed[base::vapply(
    suffixed,
    function(col) base::any(!base::is.na(arg_df[[col]])),
    logical(1)
  )]

  if (base::length(populated) > 0L) {
    warning(
      "Not sent: ", base::paste(populated, collapse = ", "), ". These ",
      "columns are external properties that get_athletes() renamed because ",
      "they share a name with a native profile field, and profile fields are ",
      "never written to external properties. On update, external properties ",
      "are replaced, so the old external key is removed. Copy any value you ",
      "want to keep into the native column.",
      call. = FALSE
    )
  }

  base::setdiff(cols, suffixed)
}


#' Format Athlete Team or Group IDs for a Write Payload
#'
#' Accepts one element of a list column (a character vector of IDs) or a
#' comma-separated string such as `"t1, t2"`, the format the Sheets and Excel
#' add-ins use. IDs are split on `,`, trimmed, and blanks are dropped.
#'
#' @param x One value from a `teams` or `groups` column, taken with `[[i]]`.
#' @return A list of ID strings, so `toJSON(auto_unbox = TRUE)` writes a JSON
#'   array even for a single ID, or `NULL` when there are no IDs, so the field
#'   is omitted.
#' @keywords internal
#' @noRd
athlete_id_values <- function(x) {
  ids <- base::as.character(base::unlist(x, use.names = FALSE))
  ids <- ids[!base::is.na(ids)]
  ids <- base::trimws(base::unlist(base::strsplit(ids, ",", fixed = TRUE)))
  ids <- ids[base::nzchar(ids)]
  if (base::length(ids) == 0L) NULL else base::as.list(ids)
}


#' Collect the External Properties for One Athlete
#'
#' @param arg_df The athlete data frame.
#' @param i Row index.
#' @param columns Columns chosen by `athlete_external_columns()`.
#' @return A named list of the properties with a value. `NA` and blank values
#'   are skipped. An empty list means `external` should not be sent, because
#'   the API replaces external properties on update.
#' @keywords internal
#' @noRd
athlete_external_values <- function(arg_df, i, columns) {
  out <- list()
  for (column in columns) {
    # `[[i]]` returns the element itself for both atomic and list columns.
    # A list-column element may be NULL, empty or hold several values; the
    # API takes a string per custom property, so several values are joined.
    value <- arg_df[[column]][[i]]
    if (base::is.list(value)) value <- base::unlist(value, use.names = FALSE)
    value <- value[!base::is.na(value)]
    # Trim text (numbers stay numbers) and drop blank elements.
    if (base::is.character(value)) value <- base::trimws(value)
    value <- value[base::nzchar(base::as.character(value))]
    if (base::length(value) == 0L) {
      next
    }
    if (base::length(value) > 1L) {
      value <- base::paste(base::as.character(value), collapse = ", ")
    }
    out[[column]] <- value
  }
  out
}


#' Warn About Heights That Look Like Feet or Inches
#'
#' The API stores height in centimeters but reads values below 100 back as
#' legacy feet or inches (95 reads back as 241.3). Sending such a value is
#' allowed, so this only warns.
#'
#' @param athletes List of athlete payloads built by `AddAthleteJSON()` or
#'   `UpdateAthleteJSON()`.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
athlete_height_warning <- function(athletes) {
  low <- base::Filter(
    function(a) !base::is.null(a$height) && a$height > 0 && a$height < 100,
    athletes
  )
  if (base::length(low) == 0L) {
    return(invisible(NULL))
  }
  labels <- base::vapply(low, function(a) {
    label <- if (!base::is.null(a$name)) a$name else a$id
    base::paste0(label, " (", a$height, ")")
  }, character(1))
  shown <- utils::head(labels, 5L)
  if (base::length(labels) > 5L) {
    shown <- c(shown, base::paste0("and ", base::length(labels) - 5L, " more"))
  }
  warning(
    "height is in centimeters, but ", base::length(low), " athlete(s) have ",
    "a height below 100: ", base::paste(shown, collapse = ", "), ". The ",
    "Hawkin API reads values below 100 back as legacy feet or inches (95 ",
    "reads back as 241.3). Check these are centimeters.",
    call. = FALSE
  )
  invisible(NULL)
}


#--------------------#


#' Add Athlete Data Frame to JSON
#'
#' Take the athlete data frame passed and convert to JSON for POST method payload
#'
#' `position`, `sport`, `dob` and `height` are written as native fields, never
#' as external properties. `id` (assigned by the API) and `lastTestedOn`
#' (read-only) are not sent, so the output of `get_athletes()` can be passed
#' straight back in.
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

  # Columns that are not body fields, profile fields or read-only fields
  other_columns <- athlete_external_columns(
    arg_df,
    body_cols = c("id", "name", "image", "active", "teams", "groups")
  )

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

    # Check for TEAMS column (list column or comma-separated string)
    if ("teams" %in% base::names(arg_df)) {
      ath$teams <- athlete_id_values(arg_df$teams[[i]])
    }

    # Check for GROUPS column (list column or comma-separated string)
    if ("groups" %in% base::names(arg_df)) {
      ath$groups <- athlete_id_values(arg_df$groups[[i]])
    }

    # Native profile fields: position, sport, dob, height
    ath <- c(ath, athlete_profile_values(arg_df, i))

    # External properties. Only sent when there is at least one value:
    # the API replaces them on update, so an empty `external` would
    # delete every custom property the athlete has.
    external <- athlete_external_values(arg_df, i, other_columns)
    if (base::length(external) > 0L) {
      ath$external <- external
    }

    x <- base::append(x, list(ath))
  }

  athlete_height_warning(x)

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
#' `position`, `sport`, `dob` and `height` are written as native fields, never
#' as external properties. `lastTestedOn` (read-only) is not sent, so the output
#' of `get_athletes()` can be modified and passed straight back in.
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
    # Columns that are not body fields, profile fields or read-only fields
    other_columns <- athlete_external_columns(
      arg_df,
      body_cols = c("id", "name", "image", "active", "teams", "groups")
    )

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

      # Check for TEAMS column (list column or comma-separated string)
      if ("teams" %in% base::names(arg_df)) {
        ath$teams <- athlete_id_values(arg_df$teams[[i]])
      }

      # Check for GROUPS column (list column or comma-separated string)
      if ("groups" %in% base::names(arg_df)) {
        ath$groups <- athlete_id_values(arg_df$groups[[i]])
      }

      # Native profile fields: position, sport, dob, height
      ath <- c(ath, athlete_profile_values(arg_df, i))

      # External properties. Only sent when there is at least one value:
      # the API replaces them on update, so an empty `external` would
      # delete every custom property the athlete has.
      external <- athlete_external_values(arg_df, i, other_columns)
      if (base::length(external) > 0L) {
        ath$external <- external
      }

      x <- base::append(x, list(ath))
    }

    athlete_height_warning(x)

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

# Expand a nested `metrics` column into a long table.
#
# Used by get_tests() when nestMetrics = TRUE (API v1.16). Each element of
# `metrics_list` is the data frame httr2::resp_body_json(simplifyVector = TRUE)
# produces for one test's `metrics` array (columns metricId, metricLabel,
# metricUnits, metricValue), or an empty list when the test has no numeric
# metrics. Returns one row per test and metric with the columns metric_id,
# metric_label, metric_units and metric_value; a test with no metrics keeps a
# single row with NA metric fields.
expand_nested_metrics <- function(meta_df, metrics_list) {
  col_or_na <- function(m, col, n, as_fn) {
    if (!is.null(m[[col]])) as_fn(m[[col]]) else rep(as_fn(NA), n)
  }
  placeholder <- data.frame(
    metric_id = NA_character_, metric_label = NA_character_,
    metric_units = NA_character_, metric_value = NA_real_,
    stringsAsFactors = FALSE
  )
  per_test <- lapply(metrics_list, function(m) {
    if (!is.data.frame(m) || nrow(m) == 0) return(placeholder)
    n <- nrow(m)
    data.frame(
      metric_id = col_or_na(m, "metricId", n, as.character),
      metric_label = col_or_na(m, "metricLabel", n, as.character),
      metric_units = col_or_na(m, "metricUnits", n, as.character),
      metric_value = col_or_na(m, "metricValue", n, as.numeric),
      stringsAsFactors = FALSE
    )
  })
  n_per <- vapply(per_test, nrow, integer(1))
  idx <- rep(seq_len(nrow(meta_df)), n_per)
  rownames(meta_df) <- NULL
  out <- cbind(meta_df[idx, , drop = FALSE], dplyr::bind_rows(per_test))
  rownames(out) <- NULL
  out
}
