# Tests for create_athletes and update_athletes input validation and JSON payload
# These test the data preparation layer without making API calls.

test_that("AddAthleteJSON handles optional columns correctly", {
  # Only required column: name
  df_minimal <- data.frame(name = "Test", stringsAsFactors = FALSE)
  json <- hawkinR:::AddAthleteJSON(df_minimal)
  parsed <- jsonlite::fromJSON(json)
  expect_equal(parsed$name, "Test")

  # With active column

  df_active <- data.frame(
    name = "Test",
    active = TRUE,
    stringsAsFactors = FALSE
  )
  json2 <- hawkinR:::AddAthleteJSON(df_active)
  parsed2 <- jsonlite::fromJSON(json2)
  expect_true(parsed2$active)
})

test_that("AddAthleteJSON handles NA values by excluding them", {
  df <- data.frame(
    name = "Test",
    image = NA_character_,
    stringsAsFactors = FALSE
  )
  json <- hawkinR:::AddAthleteJSON(df)
  parsed <- jsonlite::fromJSON(json)

  # NA image should not be included in the JSON
  expect_null(parsed$image)
})

test_that("UpdateAthleteJSON preserves id field", {
  df <- data.frame(
    id = c("id1", "id2"),
    name = c("Alice", "Bob"),
    active = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  json <- hawkinR:::UpdateAthleteJSON(df)
  parsed <- jsonlite::fromJSON(json)

  expect_equal(parsed$id, c("id1", "id2"))
  expect_equal(parsed$name, c("Alice", "Bob"))
  expect_equal(parsed$active, c(TRUE, FALSE))
})

test_that("UpdateAthleteJSON handles external properties for updates", {
  df <- data.frame(
    id = "athlete-1",
    position = "Forward",
    jersey = "10",
    stringsAsFactors = FALSE
  )
  json <- hawkinR:::UpdateAthleteJSON(df)
  parsed <- jsonlite::fromJSON(json)

  expect_equal(parsed$id, "athlete-1")
  expect_equal(parsed$external$position, "Forward")
  expect_equal(parsed$external$jersey, "10")
})
