#' Get Test Types
#'
#' @description
#' Get the test type names and ids for all the test types in the system.
#'
#' @usage
#' get_testTypes()
#'
#' @return
#' Response will be a data frame containing the tests that are in the HD system.
#' Each test type includes the following variables:
#'
#' | **Column Name**  | **Type** | **Description**              |
#' |------------------|----------|------------------------------|
#' | **canonicalId**  | *chr*    | test's unique canonical ID   |
#' | **name**         | *chr*    | test's given name            |
#' | **abbreviation** | *chr*    | test given name abbreviation |
#'
#' @examples
#' \dontrun{
#' # This is an example of how the function would be called.
#'
#' df_tests <- get_testTypes()
#'
#' }
#'
#' @importFrom logger log_info
#'
#' @export


# Get Test Types -----
get_testTypes <- function() {

  # Return Data Frame of Test Types and Abbreviations

  # Create the data frame
  type_df <- base::data.frame(
    canonicalId = c(
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

  # Print Outcome
  logger::log_success(base::paste0("hawkinR/get_testTypes -> 11 test types returned"))

  return(type_df)

}

