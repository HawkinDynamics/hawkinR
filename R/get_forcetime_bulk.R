#' @include get_tests.R
#' @include get_forcetime.R
NULL

#' Get Raw Force-Time Data (Bulk)
#'
#' @description
#' Retrieves raw force-time data for multiple tests. This function acts as a wrapper
#' for both `get_tests` and `get_forcetime`.
#'
#' @param test_ids Optional. A character vector of specific Test IDs to retrieve.
#' @param export logical. If TRUE, data is written to files in `export_dir`.
#' @param export_dir character. The directory path to save exported files.
#' @param format character. Options: "csv", "tsv", "json", "rds", "parquet".
#' @param file_naming character vector. Properties to construct the filename.
#' @param deidentify logical. If TRUE, replaces `athlete_name` with "De-identified".
#' @param ... Additional arguments passed to `get_tests` (e.g., `from`, `typeId`).
#'   Also accepts `profile` (HawkinAuth object).
#'
#' @importFrom progress progress_bar
#' @importFrom logger log_info log_error log_success log_warn
#' @importFrom jsonlite write_json
#' @importFrom utils write.csv write.table read.csv
#' @importFrom S7 prop
#' @export

get_forcetime_bulk <- function(test_ids = NULL,
                               export = FALSE,
                               export_dir = NULL,
                               format = c("csv", "json", "rds", "tsv", "parquet"),
                               file_naming = c("test_id"),
                               deidentify = FALSE,
                               ...) {

  # 1. ----- Resolve Connection -----
  format <- match.arg(format)
  logger::log_trace("hawkinR/get_forcetime_bulk -> Resolving connection")
  extra_args <- list(...)

  if (!is.null(extra_args$profile)) {
    if (is.character(extra_args$profile)) {
      # User passed a name string, so we connect
      conn <- hd_connect(profile = extra_args$profile)
    } else {
      # User passed the object directly
      conn <- extra_args$profile
    }
  } else {
    conn <- get_active_conn()
  }

  # Validate
  if (!is.object(conn) || is.null(conn@access_token)) {
    stop("A valid HawkinAuth connection is required. Run hd_connect() first.", call. = FALSE)
  }

  # Token Lifecycle Management
  token_remaining <- round(as.numeric(difftime(conn@expires_at, Sys.time(), units = "secs")))
  logger::log_debug("hawkinR/get_forcetime_bulk -> Token expires in {token_remaining} seconds")
  if (token_remaining < 300) {
    logger::log_info("hawkinR/get_forcetime_bulk -> Token expiring soon. Refreshing...")
    conn <- authenticate(conn)
    set_active_conn(conn)
  }

  # ... (Export Directory Logic matches previous versions) ...
  temp_manifest_path <- NULL
  if (isTRUE(export)) {
    if (is.null(export_dir)) stop("When export = TRUE, you must provide an 'export_dir'.", call. = FALSE)
    if (!dir.exists(export_dir)) dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(export_dir)) stop(paste0("Error: The directory '", export_dir, "' could not be created."), call. = FALSE)
    if (file.access(export_dir, mode = 2) != 0) stop(paste0("Error: You do not have write permissions for '", export_dir, "'."), call. = FALSE)

    temp_ext <- if (format == "tsv") "tsv" else "csv"
    temp_sep <- if (format == "tsv") "\t" else ","
    temp_manifest_path <- file.path(export_dir, paste0("manifest_temp.", temp_ext))
  }

  # 2. Target Resolution ----------------------------------------------------
  targets <- test_ids

  if (is.null(targets)) {
    logger::log_info("hawkinR/get_forcetime_bulk -> Querying get_tests() for targets...")

    # Pass '...' (which contains 'profile') to get_tests
    tests_df <- get_tests(...)

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
      # Pass 'profile' explicitly to get_forcetime via ...
      obj <- get_forcetime(testId = tid, profile = conn)

      if (isTRUE(deidentify)) obj@athlete_name <- "De-identified"

      if (isTRUE(export)) {
        # ... (File Naming & Write Logic matches previous) ...
        name_parts <- vapply(file_naming, function(prop) {
          if (prop == "date" || prop == "test_date") return(format(obj@test_date, "%Y%m%dT%H%M%S"))

          if (grepl("$", prop, fixed = TRUE)) {
            parts <- strsplit(prop, "$", fixed = TRUE)[[1]]
            main_val <- tryCatch(S7::prop(obj, parts[1]), error = function(e) NULL)
            if (is.list(main_val) && !is.null(main_val)) val <- main_val[[parts[2]]] else return("NA")
          } else {
            val <- tryCatch(S7::prop(obj, prop), error = function(e) "")
          }
          if (length(val) == 0 || is.null(val)) return("NA")

          safe_str <- gsub("[^[:alnum:]]", "_", as.character(val))
          safe_str <- gsub("_+", "_", safe_str)
          gsub("^_|_$", "", safe_str)
        }, FUN.VALUE = character(1))

        fname <- paste(paste(name_parts, collapse = "_"), format, sep = ".")
        full_path <- file.path(export_dir, fname)

        # Manifest Row
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

        utils::write.table(meta_row, file = temp_manifest_path, sep = temp_sep,
                           row.names = FALSE, col.names = !manifest_init, append = manifest_init)
        manifest_init <- TRUE

<<<<<<< HEAD
        # Save File
        if (format == "csv") utils::write.csv(obj@data, full_path, row.names = FALSE)
        else if (format == "tsv") utils::write.table(obj@data, full_path, sep = "\t", row.names = FALSE)
        else if (format == "parquet") {
          if (!requireNamespace("arrow", quietly = TRUE)) stop("Install 'arrow' for parquet.", call. = FALSE)
=======
        # --- 3. Write Data File ---
        if (format == "csv") {
          utils::write.csv(obj@data, full_path, row.names = FALSE)
        } else if (format == "tsv") {
          utils::write.table(obj@data, full_path, sep = "\t", row.names = FALSE)
        } else if (format == "parquet") {
          if (!requireNamespace("arrow", quietly = TRUE)) {
            stop("The 'arrow' package is required for Parquet export. Please install it: install.packages('arrow')", call. = FALSE)
          }
>>>>>>> d6a868dc825eac8606233c4d833b1991f695033d
          arrow::write_parquet(obj@data, full_path)
        }
        else if (format == "rds") saveRDS(obj, full_path)
        else if (format == "json") {
          jsonlite::write_json(list(metadata = as.list(meta_row), samples = obj@data), full_path, auto_unbox = TRUE, pretty = TRUE)
        }

        saved_files <- c(saved_files, full_path)
      } else {
        results[[tid]] <- obj
      }
    }, error = function(e) {
      logger::log_error("Failed to process test {tid}: {e$message}")
    })
  }

  # 4. Finalize Manifest ----------------------------------------------------
  if (isTRUE(export)) {
    if (length(saved_files) > 0 && file.exists(temp_manifest_path)) {
      final_manifest_path <- file.path(export_dir, paste0("metadata_manifest.", format))
      if (format %in% c("csv", "tsv")) {
        file.rename(temp_manifest_path, final_manifest_path)
      } else {
        df_manifest <- utils::read.csv(temp_manifest_path, stringsAsFactors = FALSE)
        if (format == "rds") saveRDS(df_manifest, final_manifest_path)
        else if (format == "parquet") arrow::write_parquet(df_manifest, final_manifest_path)
        else if (format == "json") jsonlite::write_json(df_manifest, final_manifest_path, pretty = TRUE)
        file.remove(temp_manifest_path)
      }
      logger::log_success("hawkinR/get_forcetime_bulk -> Export complete. Manifest created.")
    } else {
      logger::log_warn("hawkinR/get_forcetime_bulk -> No files saved.")
    }
    invisible(saved_files)
  } else {
    logger::log_success("hawkinR/get_forcetime_bulk -> Returned {length(results)} objects.")
    return(results)
  }
}
