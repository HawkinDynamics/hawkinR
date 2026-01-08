#' @include get_tests.R
#' @include get_forcetime.R
NULL

#' Get Raw Force-Time Data (Bulk)
#'
#' @description
#' Retrieves raw force-time data for multiple tests. This function acts as a wrapper
#' for both `get_tests` and `get_forcetime`.
#'
#' @details
#' **Exporting Data:**
#' If `export = TRUE`, the function creates a manifest file (e.g., `metadata_manifest.csv`)
#' in the `export_dir`. This file matches the selected `format` and acts as a summary index,
#' containing the metadata for every test exported.
#'
#' **File Naming:**
#' The `file_naming` argument supports standard properties (e.g., `"athlete_name"`)
#' and nested list properties using `$` syntax (e.g., `"athlete_external$custom_id"`).
#' Dates are automatically formatted as `YYYYMMDDTHHMMSS` (e.g., `20231025T143000`).
#'
#' **Supported Formats:**
#' \itemize{
#'   \item **"csv", "tsv"**: Saves raw data frame. Manifest is .csv or .tsv.
#'   \item **"parquet"**: Saves raw data frame (requires `arrow` package). Manifest is .parquet.
#'   \item **"json"**: Saves nested list with metadata and data. Manifest is .json.
#'   \item **"rds"**: Saves the full `HawkinForceTime` S7 object. Manifest is .rds.
#' }
#'
#' @param test_ids Optional. A character vector of specific Test IDs to retrieve.
#' @param export logical. If TRUE, data is written to files in `export_dir`.
#' @param export_dir character. The directory path to save exported files.
#' @param format character. Options: "csv", "tsv", "json", "rds", "parquet".
#' @param file_naming character vector. Properties to construct the filename.
#' Default: `c("test_id")`. Supports nested access like `"athlete_external$id"`.
#' @param deidentify logical. If TRUE, replaces `athlete_name` with "De-identified".
#' @param x Optional `HawkinAuth` object.
#' @param ... Additional arguments passed to `get_tests` (e.g., `from`, `typeId`).
#'
#' @importFrom progress progress_bar
#' @importFrom logger log_info log_error log_success log_warn
#' @importFrom jsonlite write_json
#' @importFrom utils write.csv write.table read.csv
#' @importFrom arrow write_parquet
#' @importFrom S7 prop
#' @export
get_forcetime_bulk <- function(test_ids = NULL,
                               export = FALSE,
                               export_dir = NULL,
                               format = c("csv", "json", "rds", "tsv", "parquet"),
                               file_naming = c("test_id"),
                               deidentify = FALSE,
                               x = NULL,
                               ...) {

  # 1. Validation & Setup ---------------------------------------------------
  if (is.null(x)) x <- get_active_conn()
  format <- match.arg(format)

  # Export Directory Logic
  temp_manifest_path <- NULL

  if (isTRUE(export)) {
    if (is.null(export_dir)) {
      stop("When export = TRUE, you must provide an 'export_dir'.", call. = FALSE)
    }

    # A. Attempt Creation & Validation
    if (!dir.exists(export_dir)) {
      dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (!dir.exists(export_dir)) {
      stop(paste0("Error: The directory '", export_dir, "' could not be created or found."), call. = FALSE)
    }
    if (file.access(export_dir, mode = 2) != 0) {
      stop(paste0("Error: You do not have write permissions for '", export_dir, "'."), call. = FALSE)
    }

    # Define Temporary Manifest (Always CSV for safe appending during loop)
    temp_ext <- if (format == "tsv") "tsv" else "csv"
    temp_sep <- if (format == "tsv") "\t" else ","
    temp_manifest_path <- file.path(export_dir, paste0("manifest_temp.", temp_ext))
  }

  # 2. Target Resolution ----------------------------------------------------
  targets <- test_ids

  if (is.null(targets)) {
    logger::log_info("hawkinR/get_forcetime_bulk -> Querying get_tests() for targets...")
    tests_df <- get_tests(x = x, ...)

    if (is.null(tests_df) || nrow(tests_df) == 0) {
      logger::log_warn("hawkinR/get_forcetime_bulk -> No matching tests found.")
      return(NULL)
    }
    targets <- tests_df$id
    logger::log_info("hawkinR/get_forcetime_bulk -> Found {length(targets)} tests.")
  }

  # 3. Processing Loop ------------------------------------------------------
  pb <- progress::progress_bar$new(
    format = "  Exporting [:bar] :percent in :elapsed | :current/:total",
    total = length(targets), clear = FALSE, width = 60
  )

  results <- list()
  saved_files <- character()
  manifest_init <- FALSE

  for (tid in targets) {
    pb$tick()

    tryCatch({
      # A. Fetch Data
      obj <- get_forcetime(testId = tid, x = x)

      if (isTRUE(deidentify)) obj@athlete_name <- "De-identified"

      # B. Handle Export vs Return
      if (isTRUE(export)) {

        # --- 1. Construct Filename ---
        name_parts <- vapply(file_naming, function(prop) {

          # A. Handle Date (Alias 'date' to 'test_date')
          if (prop == "date" || prop == "test_date") {
            return(format(obj@test_date, "%Y%m%dT%H%M%S"))
          }

          # B. Handle Nested Properties (e.g., "athlete_external$updateDate")
          if (grepl("$", prop, fixed = TRUE)) {
            parts <- strsplit(prop, "$", fixed = TRUE)[[1]]
            main_val <- tryCatch(S7::prop(obj, parts[1]), error = function(e) NULL)

            if (is.list(main_val) && !is.null(main_val)) {
              val <- main_val[[parts[2]]]
            } else {
              return("NA")
            }
          } else {
            # C. Standard Property
            val <- tryCatch(S7::prop(obj, prop), error = function(e) "")
          }

          if (length(val) == 0 || is.null(val)) return("NA")

          # --- Sanitization Update ---
          raw_str <- as.character(val)
          # 1. Replace non-alphanumeric with underscore
          safe_str <- gsub("[^[:alnum:]]", "_", raw_str)
          # 2. Collapse multiple underscores into one (e.g., "___" -> "_")
          safe_str <- gsub("_+", "_", safe_str)
          # 3. Trim leading/trailing underscores
          safe_str <- gsub("^_|_$", "", safe_str)

          return(safe_str)

        }, FUN.VALUE = character(1))

        fname <- paste(paste(name_parts, collapse = "_"), format, sep = ".")
        full_path <- file.path(export_dir, fname)

        # --- 2. Append Metadata to Temp Manifest ---
        meta_row <- data.frame(
          filename      = fname,
          test_id       = obj@test_id,
          athlete_id    = obj@athlete_id,
          athlete_name  = obj@athlete_name,
          test_type     = obj@testType_name,
          sampling_rate = obj@test_sampling_rate,
          timestamp     = obj@timestamp,
          date          = format(obj@test_date),
          stringsAsFactors = FALSE
        )

        utils::write.table(
          meta_row,
          file = temp_manifest_path,
          sep = temp_sep,
          row.names = FALSE,
          col.names = !manifest_init,
          append = manifest_init
        )
        manifest_init <- TRUE

        # --- 3. Write Data File ---
        if (format == "csv") {
          utils::write.csv(obj@data, full_path, row.names = FALSE)
        } else if (format == "tsv") {
          utils::write.table(obj@data, full_path, sep = "\t", row.names = FALSE)
        } else if (format == "parquet") {
          if (!requireNamespace("arrow", quietly = TRUE)) {
            stop("The 'arrow' package is required for Parquet export. Please install it: install.packages('arrow')", call. = FALSE)
          }
          arrow::write_parquet(obj@data, full_path)
        } else if (format == "rds") {
          saveRDS(obj, full_path)
        } else if (format == "json") {
          json_list <- list(metadata = as.list(meta_row), samples = obj@data)
          jsonlite::write_json(json_list, full_path, auto_unbox = TRUE, pretty = TRUE)
        }

        saved_files <- c(saved_files, full_path)

      } else {
        # Return Object to List
        results[[tid]] <- obj
      }

    }, error = function(e) {
      logger::log_error("Failed to process test {tid}: {e$message}")
    })
  }

  # 4. Finalize Manifest & Return -------------------------------------------
  if (isTRUE(export)) {
    if (length(saved_files) > 0 && file.exists(temp_manifest_path)) {

      final_manifest_path <- file.path(export_dir, paste0("metadata_manifest.", format))

      if (format %in% c("csv", "tsv")) {
        file.rename(temp_manifest_path, final_manifest_path)
      } else {
        # Convert Temp CSV to Target Format
        df_manifest <- utils::read.csv(temp_manifest_path, stringsAsFactors = FALSE)

        if (format == "rds") {
          saveRDS(df_manifest, final_manifest_path)
        } else if (format == "parquet") {
          arrow::write_parquet(df_manifest, final_manifest_path)
        } else if (format == "json") {
          jsonlite::write_json(df_manifest, final_manifest_path, pretty = TRUE)
        }

        file.remove(temp_manifest_path)
      }

      logger::log_success("hawkinR/get_forcetime_bulk -> Export complete. Manifest created: {basename(final_manifest_path)}")
    } else {
      logger::log_warn("hawkinR/get_forcetime_bulk -> Process completed but NO files were saved.")
    }

    invisible(saved_files)
  } else {
    logger::log_success("hawkinR/get_forcetime_bulk -> Returned {length(results)} objects.")
    return(results)
  }
}
