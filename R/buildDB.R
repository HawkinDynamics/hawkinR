#' Build Local Database File
#'
#' @description
#' This function builds a database of prior test data from a set amount of prior days until today.
#' It allows setting increments to control the number of days downloaded at a time for more efficient download speeds.
#' The default span is 14, meaning tests will download in 14-day increments.
#'
#' @usage
#' buildDB(startDate, testType = "all", includeInactive = FALSE,
#'   fileName,
#'   fileType = c("csv", "xlsx", "rds", "rds_gz",
#'      "rds_bz2", "rds_xz", "parquet", "feather"),
#'   span = 14)
#'
#' @param startDate A date string or numeric value representing the starting date for retrieving test data. Date string must be format "YYYY-MM-DD", or numerical value as EPOCH timestamp.
#' @param testType A character string representing the type of test to retrieve. Defaults to `"all"`. Can be a specific test type.
#' @param includeInactive A logical value indicating whether to include inactive tests. Defaults to `FALSE`.
#' @param fileName A character string representing the name of the file to save the database.
#' @param fileType A character string specifying the format of the file to save. Supported formats include `"csv"`, `"xlsx"`, `"rds"`, `"rds_gz"`, `"rds_bz2"`, `"rds_xz"`, `"parquet"`, `"feather"`. Defaults to `"csv"`.
#' @param span An integer value indicating the number of days to download in each increment. Defaults to 14.
#'
#' @details
#' This function creates a time frame starting from the specified `startDate`, and iteratively downloads test data in
#' increments of `span` days. It saves the result as a file in the format specified by `fileType`. Supported file types include
#' CSV, XLSX, Parquet, Feather, and various RDS formats (with optional compression).
#'
#' @return The function saves the resulting database to a file in the specified format and logs the process.
#'
#' @importFrom magrittr %>%
#' @importFrom logger log_info
#' @importFrom dplyr bind_rows filter select transmute select_if
#' @importFrom stringr str_detect str_split_i str_starts
#' @importFrom readr write_csv write_rds
#' @importFrom arrow write_parquet write_feather
#' @importFrom writexl write_xlsx
#'
#' @examples
#' \dontrun{
#' # Build a database of test data from the last 30 days and save it as a CSV
#'
#' buildDB("2023-01-01", testType = "all", fileName = "test_data", fileType = "csv", span = 5)
#' }
#'
#' @export


#------------------------------------------------------------------------------#
# Database File Builder-----

buildDB <- function(
    startDate,
    testType = "all",
    includeInactive = FALSE,
    fileName,
    fileType = c("csv", "xlsx", "rds", "rds_gz", "rds_bz2", "rds_xz", "parquet", "feather"), span = 14) {

  # Log Trace
  logger::log_trace(base::paste0("hawkinR -> Run: buildDB"))

  # Retrieve access token and expiration from environment variables
  aToken <- base::Sys.getenv("accessToken")

  #-----#

  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration"))

  #-----#

  # Check for Access Token and Expiration
  if (base::is.null(aToken) ||
      token_exp <= base::as.numeric(base::Sys.time())) {
    logger::log_error("hawkinR/buildDB -> Access token not available or expired. Call get_access() to obtain it.")
    stop("Access token not available or expired. Call get_access() to obtain it.")
  } else {
    # Log Debug
    logger::log_debug(base::paste0("hawkinR/buildDB -> Temporary access token expires: ", base::as.POSIXct(token_exp)))
  }

  #----------------------------------------------------------------------------#
  # Create time frame -----
  #----------------------------------------------------------------------------#

  # Set epoc date value from `startDate`
  xDate <- validate_timestamp(startDate)

  # Set number of days since date
  days <- base::ceiling((base::as.numeric(base::Sys.time()) - xDate)/86400)

  # Create sequence from number of days
  dSeq <- base::seq(from = days, to = 0, by = -1*span)

  logger::log_info(base::paste0(
    "hawkinR/buildDB -> Calling tests starting from ",
    base::as.Date.POSIXct(xDate),
    " in ", span, " day incriments"
  ))

  #----------------------------------------------------------------------------#
  # Build Data Frame -----
  #----------------------------------------------------------------------------#

  # Create blank data frame
  df <- base::data.frame()

  #----------------#
  # Loop `GetTests` in `span` day increments
  #----------------#
  for ( i in base::seq_along(dSeq)) {

    # Set FROM date with i
    f <- base::as.numeric(base::as.POSIXct(base::Sys.Date() - dSeq[i]))

    # Set TO date with i+1
    t <- if(i == length(dSeq)) {
      NULL
    } else {
      base::as.numeric(base::as.POSIXct(base::Sys.Date() - dSeq[i+1]))
    }

    #----- Call tests
    x <- base::data.frame() # initialize x as an empty data frame

    # call tests
    x <- tryCatch({

      if (testType == "all") { # Call all test types
        tId <- NULL
      } else {
        # Check typeId and extract corresponding id
        tId <- TestIdCheck(testType)
      }

      # Call tests from cloud
      result_value <- get_tests(
        from = f,
        to = t,
        includeInactive = includeInactive,
        typeId = tId
      )

      # Value to return if no error occurs
      result_value
    }, error = function(err) {
      # Value return if error occurs
      error_value <- base::data.frame()
    })

    # Add tests to df
    df <- base::unique(dplyr::bind_rows(x,df)) %>%
      dplyr::arrange(dplyr::desc(.data$timestamp))
  }

  logger::log_info(base::paste0(
    "hawkinR/buildDB -> ",
    base::nrow(df) , " tests returned"
  ))

  #----------------------------------------------------------------------------#
  # Save File -----
  #----------------------------------------------------------------------------#

  # Check File Type
  fileType <- base::match.arg(fileType)

  if (fileType %in% c("csv", "xlsx", "feather", "parquet")) {
    #----------------#
    # CSV - PARQUET - FEATHER - XLSX
    #----------------#

    # Flatten Listed Columns
    df <- dfTests_flat(df)

    if(fileType == "csv") {
      #----------------#
      # CSV
      #----------------#

      # Verify File Name Extension
      f <- if(stringr::str_detect(fileName,".csv")) {
        fileName
      } else {
        base::paste0(fileName,".csv")
      }

      # Write to CSV File
      readr::write_csv(df, f)

    } else if(fileType == "parquet") {
      #----------------#
      # PARQUET
      #----------------#

      # Verify File Name Extension
      f <- if(stringr::str_detect(fileName,".parquet")) {
        fileName
      } else {
        base::paste0(fileName,".parquet")
      }

      # Write to Parquet File
      arrow::write_parquet(df, f)

    } else if(fileType == "feather") {
      #----------------#
      # FEATHER
      #----------------#

      # Verify File Name Extension
      f <- if(stringr::str_detect(fileName,".feather")) {
        fileName
      } else {
        base::paste0(fileName,".feather")
      }

      # Write to Feather File
      arrow::write_feather(df, f)

    } else if(fileType == "xlsx") {
      #----------------#
      # XLSX
      #----------------#

      # Initialize file sheet list
      sheets <- base::list()

      # Identify Test Types in DB
      types <- df %>%
        dplyr::transmute(
          type = stringr::str_split_i(
            stringr::str_split_i(.data$segment,":",i = 1),"-",i = 1
          )
        ) %>% base::unique()

      # Create test type sheets
      for (i in 1:base::nrow(types)) {
        # Filter data frame by test type
        x <- df %>%
          dplyr::filter(stringr::str_starts(.data$segment, types$type[i]))
        # Isolate test and trial details
        x1 <- x %>%
          dplyr::select(1:14)
        # Isolate and select only relevant metric columns
        x2 <- x %>%
          dplyr::select(15:ncol(x)) %>%
          dplyr::select_if(~ !all(is.na(.)))
        # Combine trial info and metrics back together
        x <- dplyr::bind_cols(x1,x2)

        # assign data frame variable to sheets list with name
        sheets[[types$type[i]]] = x
      }

      # Verify File Name Extension
      f <- if(stringr::str_detect(fileName,".xlsx")) {
        fileName
      } else {
        base::paste0(fileName,".xlsx")
      }

      # Write to file
      writexl::write_xlsx(sheets, path = f)
    }

  } else if (fileType == "rds") {
    #----------------#
    # RDS
    #----------------#

    # Verify File Name Extension
    f <- if(stringr::str_detect(fileName,".rds")) {
      fileName
    } else {
      base::paste0(fileName,".rds")
    }

    # Write to File
    readr::write_rds(df, f)

  } else if (fileType == "rds_gz") {
    #----------------#
    # RDS_gz Compressed
    #----------------#

    # Verify File Name Extension
    f <- if(stringr::str_detect(fileName,".rds")) {
      fileName
    } else {
      base::paste0(fileName,".rds")
    }

    # Write to File
    readr::write_rds(df, f, compress = "gz")

  } else if (fileType == "rds_bz2") {
    #----------------#
    # RDS_gz Compressed
    #----------------#

    # Verify File Name Extension
    f <- if(stringr::str_detect(fileName,".rds")) {
      fileName
    } else {
      base::paste0(fileName,".rds")
    }

    # Write to File
    readr::write_rds(df, f, compress = "bz2")

  } else if (fileType == "rds_xz") {
    #----------------#
    # RDS_gz Compressed
    #----------------#

    # Verify File Name Extension
    f <- if(stringr::str_detect(fileName,".rds")) {
      fileName
    } else {
      base::paste0(fileName,".rds")
    }

    readr::write_rds(df, f, compress = "xz")

  } else {

    stop("Unsupported file format")
  }

  #-------------------#

  f <- if(stringr::str_detect(fileName,"\\.")) {
    fileName
  } else {
    base::paste0(fileName,".",fileType)
  }

  logger::log_info(base::paste0(
    "hawkinR/buildDB -> data saved to ", f
  ))
}

