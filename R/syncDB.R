#' Sync Database with Latest Test Data
#'
#' @description
#' This function updates an existing test database by retrieving new tests and syncing updates
#' for previous tests. It reads the current database from the specified file and returns an updated version.
#'
#' @usage syncDB(`file`, `includeInactive` =  FALSE, `newFile` = NULL)
#'
#' @param file A character string representing the path to the current database file. Supported file types include `csv`, `rds`, `xlsx`, `feather`, and `parquet`.
#' @param includeInactive A logical value indicating whether to include inactive tests in the sync. Defaults to `FALSE`.
#' @param newFile If NULL, updates will save to existing file path and overwrite previous version. Provide a new file path and name to create a new version of database and reserve original.
#'
#' @details
#' This function reads an existing database from the specified file and updates it by fetching new tests
#' and updating any changes to existing tests. It supports multiple file types and saves the updated database back to the original file.
#' Supported file formats include CSV, XLSX, Feather, Parquet, and RDS with optional compression.
#'
#' @importFrom magrittr %>%
#' @importFrom readr write_csv read_csv write_rds
#' @importFrom writexl write_xlsx
#' @importFrom dplyr bind_rows transmute filter full_join select select_if intersect setdiff
#' @importFrom stringr str_detect str_split_i str_starts
#' @importFrom arrow write_parquet write_feather read_parquet read_feather
#' @importFrom logger log_info
#'
#' @return The function saves the updated database to the specified file and logs the update process.
#'
#' @examples
#' \dontrun{
#' # Sync an existing database and include inactive tests
#'
#' syncDB("test_data.csv", includeInactive = TRUE, newFile = NULL)
#' }
#'
#' @export


#------------------------------------------------------------------------------#
# Database File Sync -----

syncDB <- function(file, includeInactive = FALSE, newFile = NULL) {

  # Log Trace
  logger::log_trace(base::paste0("hawkinR -> Run: syncDB"))

  # Retrieve access token and expiration from environment variables
  aToken <- base::Sys.getenv("accessToken")

  #-----#

  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration"))

  #-----#

  # Check for Access Token and Expiration
  if (base::is.null(aToken) ||
      token_exp <= base::as.numeric(base::Sys.time())) {
    logger::log_error("hawkinR/syncDB -> Access token not available or expired. Call get_access() to obtain it.")
    stop("Access token not available or expired. Call get_access() to obtain it.")
  } else {
    # Log Debug
    logger::log_debug(base::paste0("hawkinR/syncDB -> Temporary access token expires: ", base::as.POSIXct(token_exp)))
  }

  #----------------------------------------------------------------------------#
  #----- Read Original File -----
  #----------------------------------------------------------------------------#

  #--------------------#
  #----- RDS -----
  if(stringr::str_detect(file,".rds")) {
    # Read file from path
    df <- base::readRDS(file)
  }

  #--------------------#
  #----- CSV -----
  if(stringr::str_detect(file,".csv")) {
    # Read file from path
    df <- readr::read_csv(file)

    # Expand comma-separated columns
    df <- dfTests_expand(arg_df = df)

    # Optimize column types
    df <- dfDatetoChar(df)
  }

  #--------------------#
  #----- XLSX -----
  if(stringr::str_detect(file,".xlsx")) {
    # Identify Sheets from file from
    sheets <- readxl::excel_sheets(file)

    # Read and combine sheets
    for (i in 1:base::length(sheets)) {
      x <- readxl::read_xlsx(path = file, sheet = sheets[i])
      if (i == 1) {
        df <- x
      } else {
        df <- dplyr::full_join(df, x)
      }
    }

    # Expand comma-separated columns
    df <- dfTests_expand(arg_df = df)

    # Optimize column types
    df <- dfDatetoChar(df)
  }

  #--------------------#
  #----- FEATHER -----
  if(stringr::str_detect(file,".feather")) {
    # Read file from path
    df <- arrow::read_feather(file = file)

    # Expand comma-separated columns
    df <- dfTests_expand(arg_df = df)
  }

  #--------------------#
  #----- PARQUET -----
  if(stringr::str_detect(file,".parquet")) {
    # Read file from path
    df <- arrow::read_parquet(file = file)

    # Expand comma-separated columns
    df <- dfTests_expand(arg_df = df)
  }

  logger::log_info(base::paste0(
    "hawkinR/syncDB -> ", base::nrow(df), " tests retrieved from ", file
  ))
  #----------------------------------------------------------------------------#
  #----- Call New Tests -----
  #----------------------------------------------------------------------------#

  # Get `lastSync` from current Database
  lastSync <- base::max(df$last_sync_time)

  # Check if any specific database test type
  if(base::length(base::unique(df$testType_name)) > 1) {
    typeCheck <- NULL
    logger::log_info(base::paste0(
      "hawkinR/syncDB -> Checking for new tests and updates since ", base::as.Date.POSIXct(lastSync)
    ))
  } else {
    typeCheck <- stringr::str_split_i(base::unique(df$testType_name)[1],"-",1)
    logger::log_info(base::paste0(
      "hawkinR/syncDB -> Checking for new ",  typeCheck,
      " tests and updates since ", base::as.Date.POSIXct(lastSync)
    ))
  }

  # call test sync
  newdf <- base::tryCatch({
    # Code that might produce an error
    result_value <- get_tests(
      from = lastSync,
      sync = TRUE,
      typeId = typeCheck,
      includeInactive = includeInactive
    )

    # Value to return if no error occurs
    result_value
  }, error = function(err) {
    # Value return if error occurs
    error_value <- NULL
  })

  # Check for NULL response
  finalDF <- if(base::is.null(newdf)) {
    df
  } else {
    # Identify matching IDs between df and newdf
    matching_ids <- dplyr::intersect(
      base::as.list(df[,1]),
      base::as.list(newdf[,1])
    )

    # Update existing cases in df with data from newdf
    for (id in matching_ids) {
      df[df$id == id, ] <- newdf[newdf$id == id, ]
    }

    # Add new cases from newdf to df
    new_ids <- dplyr::setdiff(
      base::as.list(newdf$id),
      base::as.list(df$id)
    )

    # Ensure all columns exist in both data frames and have the same type
    common_cols <- intersect(names(df), names(newdf))

    # Align column types for consistency
    for (col in common_cols) {
      if (is.list(df[[col]]) && !is.list(newdf[[col]])) {
        newdf[[col]] <- as.list(newdf[[col]])  # Convert logical to list
      } else if (!is.list(df[[col]]) && is.list(newdf[[col]])) {
        df[[col]] <- as.list(df[[col]])  # Convert non-list to list
      }
    }

    # Bind rows safely
    df <- dplyr::bind_rows(df[, common_cols], newdf[newdf$id %in% new_ids, common_cols]) %>%
      dplyr::arrange(dplyr::desc(.data$timestamp))

    # Return Data Frame
    df
  }

  if(base::nrow(newdf) > 0) {
    logger::log_info(base::paste0(
      "hawkinR/syncDB ->  ",  base::nrow(newdf),
      " tests and updates since ", base::as.Date.POSIXct(lastSync)
    ))
  } else {
    logger::log_info(base::paste0(
      "hawkinR/syncDB ->  No new tests or updates found since ",
      base::as.Date.POSIXct(lastSync)
    ))
  }

  #----------------------------------------------------------------------------#
  #----- Save File -----
  #----------------------------------------------------------------------------#

  # Check File Type
  fileType <- stringr::str_split_i(file,"\\.", 2)

  if (fileType %in% c("csv", "xlsx", "feather", "parquet")) {
    #----------------#
    # CSV - PARQUET - FEATHER - XLSX
    #----------------#

    # Flatten Listed Columns
    df <- dfTests_flat(finalDF)

    if(fileType == "csv") {
      #----------------#
      # CSV
      #----------------#

      # Check for new file path
      f <- if(base::is.null(newFile)) {
        file
      } else {newFile}

      # Write to CSV File
      readr::write_csv(df, f)

    } else if(fileType == "parquet") {
      #----------------#
      # PARQUET
      #----------------#

      # Check for new file path
      f <- if(base::is.null(newFile)) {
        file
      } else {newFile}

      # Write to Parquet File
      arrow::write_parquet(df, f)

    } else if(fileType == "feather") {
      #----------------#
      # FEATHER
      #----------------#

      # Check for new file path
      f <- if(base::is.null(newFile)) {
        file
      } else {newFile}

      # Write to Feather File
      arrow::write_feather(df, f)

    } else if(fileType == "xlsx") {
      #----------------#
      # XLSX
      #----------------#

      # Initialize file sheet list
      sheets <- base::list()

      # Identify Test Types in df
      types <- df %>%
        dplyr::transmute(
          type = stringr::str_split_i(
            stringr::str_split_i(.data$segment,":",i = 1),"-",i = 1
          )
        ) %>% base::unique()

      # Create test type sheets
      for (i in 1:base::nrow(types)) {
        # Filter data frame by test type
        x <- df %>% dplyr::filter(stringr::str_starts(.data$segment, types$type[i]))
        # Isolate test and trial details
        x1 <- x %>% dplyr::select(1:14)
        # Isolate and select only relevant metric columns
        x2 <- x %>%
          dplyr::select(15:ncol(x)) %>%
          dplyr::select_if(~ !all(is.na(.)))
        # Combine trial info and metrics back together
        x <- dplyr::bind_cols(x1,x2)

        # assign data frame variable to sheets list with name
        sheets[[types$type[i]]] = x
      }

      # Check for new file path
      f <- if(base::is.null(newFile)) {
        file
      } else {newFile}

      # Write to file
      writexl::write_xlsx(sheets, path = f)
    }

  } else if (fileType == "rds") {
    #----------------#
    # RDS
    #----------------#

    # Check for new file path
    f <- if(base::is.null(newFile)) {
      file
    } else {newFile}

    # Write to File
    readr::write_rds(df, file = f)

  } else if (fileType == "rds_gz") {
    #----------------#
    # RDS_gz Compressed
    #----------------#

    # Check for new file path
    f <- if(base::is.null(newFile)) {
      file
    } else {newFile}

    # Write to File
    readr::write_rds(df, file = f, compress = "gz")

  } else if (fileType == "rds_bz2") {
    #----------------#
    # RDS_gz Compressed
    #----------------#

    # Check for new file path
    f <- if(base::is.null(newFile)) {
      file
    } else {newFile}

    # Write to File
    readr::write_rds(df, file = f, compress = "bz2")

  } else if (fileType == "rds_xz") {
    #----------------#
    # RDS_gz Compressed
    #----------------#

    # Check for new file path
    f <- if(base::is.null(newFile)) {
      file
    } else {newFile}

    # Write to File
    readr::write_rds(df, file = f, compress = "xz")
  }

  #----------------------------------------------------------------------------#
  # Check for new file path
  f <- if(base::is.null(newFile)) {
    file
  } else {newFile}

  logger::log_info(base::paste0(
    "hawkinR/syncDB -> Updated data saved to ", f
  ))
}


