#' Get Test Metrics
#'
#' @description
#' Get the metrics and ids for all the metrics in the system.
#'
#' @usage
#' get_metrics(testType = "all")
#'
#' @param testType Supply a value of type string. Must be canonical test Id, test type name, or test
#' name abbreviation.
#'
#' Names | Abbreviations: "Countermovement Jump" | "CMJ", "Squat Jump" | "SJ",
#' "Isometric Test" | "ISO", "Drop Jump" | "DJ", "Free Run" | "FREE", "CMJ Rebound" | "CMJR",
#' "Multi Rebound" | "MR", "Weigh In" | "WI", "Drop Landing" | "DL"
#'
#' @return
#' Response will be a data frame containing the tests metrics that are in the HD system. The parameter
#' `testType` can be used to filter and return only metrics of the specified type.
#'
#' The returned data frame will follow the following schema:
#'
#' | **Column Name** | **Type** | **Description** |
#' |-----------------|----------|-----------------|
#' | **id**          | *chr*    | System metric ID |
#' | **label**       | *chr*    | Outward facing label or title |
#' | **units**       | *chr*    | Unit of measure (if any) |
#' | **description** | *chr*    | Description or definition of metric |
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#' df_testsMetrics <- get_metrics(testType = "CMJ")
#'
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom httr2 request req_url_path_append req_auth_bearer_token req_error req_perform resp_status resp_body_json
#' @importFrom dplyr filter
#'
#' @export


# Get Test Metrics -----
get_metrics <- function(testType = "all") {

  # 1. ----- Set Logger -----
  # Log Trace
  logger::log_trace(base::paste0("hawkinR -> Run: get_metrics"))

  # Save the current setting
  old_show_error_messages <- base::getOption("show.error.messages")
  base::on.exit(base::options(show.error.messages = old_show_error_messages),
                add = TRUE)

  # Disable error messages
  base::options(show.error.messages = FALSE)

  # 2. ----- Parameter Validation -----

  # Retrieve access token and expiration from environment variables
  aToken <- base::Sys.getenv("accessToken")

  #-----#

  token_exp <- base::as.numeric(base::Sys.getenv("accessToken_expiration"))

  #-----#

  # Check for Access Token and Expiration
  if (base::is.null(aToken) ||
      token_exp <= base::as.numeric(base::Sys.time())) {
    stop("Access token not available or expired. Call get_access() to obtain it.")
  } else {
    # Log Debug
    logger::log_debug(base::paste0("hawkinR/get_metrics -> Temporary access token expires: ", as.POSIXct(token_exp)))
  }

  #-----#

  # Validate Test Type Id argument
  tId <- if (testType == "all") {
    "all"
  } else {
    TestIdCheck(arg_id = testType)
  }

  # Log Trace
  logger::log_trace(base::paste0("hawkinR/get_metrics -> Test Type: ", tId))

  # 3. ----- Build URL Request -----

  # Build Request
  request <- httr2::request(base::Sys.getenv("urlRegion")) %>%
    # Add URL Path
    httr2::req_url_path_append("/metrics") %>%
    # Supply Bearer Authentication
    httr2::req_auth_bearer_token(token = aToken)

  # Log Debug
  reqPath <- httr2::req_dry_run(request, quiet = TRUE)
  logger::log_debug(base::paste0("hawkinR/get_metrics -> ", reqPath$method, ": ", reqPath$headers$host, reqPath$path))

  # Execute Call
  resp <- request %>%
    httr2::req_error(
      is_error = function(resp)
        FALSE
    ) %>%
    httr2::req_perform()

  # Response Status
  status <- httr2::resp_status(resp = resp)

  # 4. ----- Create Response Outputs -----

  # Error Handler
  error_message <- NULL

  if (status == 401) {
    error_message <- 'Error 401: Refresh Token is invalid or expired.'
  } else if (status == 500) {
    error_message <-
      'Error 500: Something went wrong. Please contact support@hawkindynamics.com'
  }

  if (!base::is.null(error_message)) {
    stop(logger::log_error(base::paste0(
      "hawkinR/get_metrics -> ", error_message
    )))
  }

  # Response Table
  if (status == 200) {
    # Response GOOD - Run rest of script
    # Convert JSON Response
    x <- httr2::resp_body_json(resp = resp,
                               check_type = TRUE,
                               simplifyVector = TRUE)

    # 5. ----- Sort Test Type Data -----

    if (tId == "all") {
      # Validate current metrics
      tests <- dplyr::filter(x, !base::is.na(x[[2]]))

      # Build Empty Data Frame
      df <- base::data.frame(
        canonicalTestTypeID = c(),
        testTypeName = c(),
        id = c(),
        label = c(),
        units = c(),
        description = c()
      )

      # Loop and Fill Data Frame
      for (i in 1:base::nrow(tests)) {
        r <- tests[i, ]
        metric <- r[[1, 3]]
        canonicalTestTypeID <-
          base::rep_len(r[[1, 1]], base::nrow(metric))
        testTypeName <- base::rep_len(r[[1, 2]], base::nrow(metric))

        newdf <-
          base::cbind(canonicalTestTypeID, testTypeName, metric)

        df <- base::rbind(df, newdf)
      }

      # Return Output
      logger::log_success(base::paste0(
        "hawkinR/get_metrics -> All ",
        base::nrow(df),
        " metrics returned"
      ))
      base::return(df)
    } else {
      # Filter for Test Type
      tests <- dplyr::filter(x, x[[1]] == tId)

      # Create Data Frame and Return Output
      logger::log_success(
        base::paste0(
          "hawkinR/get_metrics -> Returned ",
          base::nrow(tests),
          " metrics from ",
          testType
        )
      )
      base::return(tests[[1, 3]])
    }
  }
}

