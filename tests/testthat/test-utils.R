# Tests for utility functions in utils.R
# These tests validate helpers that don't require API calls.

# --- TestIdCheck ---

test_that("TestIdCheck resolves canonical IDs correctly", {
  # By canonical ID
  expect_equal(
    hawkinR:::TestIdCheck("7nNduHeM5zETPjHxvm7s"),
    "7nNduHeM5zETPjHxvm7s"
  )
  # By full name
  expect_equal(
    hawkinR:::TestIdCheck("Countermovement Jump"),
    "7nNduHeM5zETPjHxvm7s"
  )
  # By abbreviation
  expect_equal(
    hawkinR:::TestIdCheck("CMJ"),
    "7nNduHeM5zETPjHxvm7s"
  )
  # Squat Jump by abbreviation
  expect_equal(
    hawkinR:::TestIdCheck("SJ"),
    "QEG7m7DhYsD6BrcQ8pic"
  )
})

test_that("TestIdCheck errors on invalid type ID", {
  expect_error(
    hawkinR:::TestIdCheck("NotARealTestType"),
    "typeId incorrect"
  )
})

# --- AddAthleteJSON ---

test_that("AddAthleteJSON converts minimal data frame to JSON", {
  df <- data.frame(name = c("John Doe", "Jane Smith"), stringsAsFactors = FALSE)
  json <- hawkinR:::AddAthleteJSON(df)

  # Should be valid JSON
  parsed <- jsonlite::fromJSON(json)
  expect_equal(nrow(parsed), 2)
  expect_equal(parsed$name, c("John Doe", "Jane Smith"))
})

test_that("AddAthleteJSON includes external properties", {
  df <- data.frame(
    name = "Test Athlete",
    jersey_number = "42",
    stringsAsFactors = FALSE
  )
  json <- hawkinR:::AddAthleteJSON(df)
  parsed <- jsonlite::fromJSON(json)

  expect_equal(parsed$external$jersey_number, "42")
})

# --- UpdateAthleteJSON ---

test_that("UpdateAthleteJSON requires id column", {
  df <- data.frame(name = "No ID", stringsAsFactors = FALSE)
  expect_error(
    hawkinR:::UpdateAthleteJSON(df),
    "id"
  )
})

test_that("UpdateAthleteJSON includes id in output", {
  df <- data.frame(
    id = "athlete-123",
    name = "Updated Name",
    stringsAsFactors = FALSE
  )
  json <- hawkinR:::UpdateAthleteJSON(df)
  parsed <- jsonlite::fromJSON(json)

  expect_equal(parsed$id, "athlete-123")
  expect_equal(parsed$name, "Updated Name")
})

# --- sanitize_chunks ---

test_that("sanitize_chunks handles mixed list/logical columns", {
  # Chunk 1 has a bare list column, chunk 2 has logical NA in same column
  chunk1 <- data.frame(id = "a", stringsAsFactors = FALSE)
  chunk1$val <- list(c("x", "y"))
  chunk2 <- data.frame(id = "b", val = NA, stringsAsFactors = FALSE)

  result <- hawkinR:::sanitize_chunks(list(chunk1, chunk2))

  # After sanitization, both should have list-type val column
  expect_true(inherits(result[[1]]$val, "list"))
  expect_true(inherits(result[[2]]$val, "list"))
})

test_that("sanitize_chunks returns empty list unchanged", {
  expect_equal(length(hawkinR:::sanitize_chunks(list())), 0)
})

test_that("sanitize_chunks returns single chunk unchanged", {
  chunk <- data.frame(id = "a", val = 1)
  result <- hawkinR:::sanitize_chunks(list(chunk))
  expect_equal(length(result), 1)
  expect_equal(result[[1]]$val, 1)
})
