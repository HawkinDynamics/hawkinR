# Tests for AthletePrep() and token_seconds_remaining() in utils.R
# These validate the v1.14 profile fields, robust external-property unnesting,
# and NA-safe token handling. No network access required.

# --- AthletePrep: external properties with varying keys ---

test_that("AthletePrep unnests external list-column with varying keys", {
  df <- data.frame(
    id = c("a1", "a2"),
    name = c("Athlete One", "Athlete Two"),
    active = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  df$teams <- list(list("t1"), list("t2"))
  df$groups <- list(list("g1"), list("g2"))
  # Athlete 2 is missing the 'school' key entirely
  df$external <- list(
    list(jersey = "10", school = "East"),
    list(jersey = "22")
  )

  res <- hawkinR:::AthletePrep(df, prefix = "")

  expect_true(all(c("id", "name", "jersey", "school") %in% names(res)))
  expect_equal(res$jersey, c("10", "22"))
  # Missing key for athlete 2 becomes NA, not an error
  expect_equal(res$school, c("East", NA_character_))
  # Bare prefix -> no athlete_ prefix
  expect_false(any(grepl("^athlete_", names(res))))
})

test_that("AthletePrep unnests external sub-data-frame with uniform keys", {
  df <- data.frame(
    id = c("a1", "a2"),
    name = c("One", "Two"),
    active = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  df$teams <- list(NA, NA)
  df$groups <- list(NA, NA)
  df$external <- data.frame(
    jersey = c("10", "22"),
    school = c("East", "West"),
    stringsAsFactors = FALSE
  )

  res <- hawkinR:::AthletePrep(df, prefix = "athlete_")

  expect_true(all(c("athlete_jersey", "athlete_school") %in% names(res)))
  expect_equal(res$athlete_jersey, c("10", "22"))
})

# --- AthletePrep: profile fields (API v1.14) ---

test_that("AthletePrep includes profile fields when present", {
  df <- data.frame(
    id = c("a1", "a2"),
    name = c("One", "Two"),
    active = c(TRUE, TRUE),
    image = c("http://img/1.png", NA),
    position = c("Forward", NA),
    dob = c("1998-04-01", NA),
    sport = c("Basketball", NA),
    height = c(190, NA),
    lastTestedOn = c(1718000000, NA),
    stringsAsFactors = FALSE
  )
  df$teams <- list(NA, NA)
  df$groups <- list(NA, NA)

  res <- hawkinR:::AthletePrep(df, prefix = "athlete_")

  expect_true(all(c(
    "athlete_image", "athlete_position", "athlete_dob",
    "athlete_sport", "athlete_height", "athlete_lastTestedOn"
  ) %in% names(res)))
  expect_equal(res$athlete_position, c("Forward", NA))
})

test_that("AthletePrep tolerates missing profile and external blocks", {
  df <- data.frame(
    id = c("a1", "a2"),
    name = c("One", "Two"),
    active = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  df$teams <- list(NA, NA)
  df$groups <- list(NA, NA)

  res <- hawkinR:::AthletePrep(df, prefix = "athlete_")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_true(all(c("athlete_id", "athlete_name") %in% names(res)))
  # No external columns invented
  expect_false(any(grepl("external", names(res))))
})

test_that("AthletePrep returns empty data frame for NULL/empty input", {
  expect_equal(nrow(hawkinR:::AthletePrep(NULL)), 0)
  expect_equal(nrow(hawkinR:::AthletePrep(list())), 0)
})

# --- token_seconds_remaining: NA-safe guard ---

test_that("token_seconds_remaining returns seconds for a valid token", {
  auth <- HawkinAuth(config = HawkinConfig())
  auth@expires_at <- as.POSIXct(Sys.time() + 3600)
  remaining <- hawkinR:::token_seconds_remaining(auth)
  expect_true(is.numeric(remaining))
  expect_gt(remaining, 3000)
})

test_that("token_seconds_remaining errors clearly on missing expiration", {
  # Default expires_at is NULL until authenticate() runs
  auth <- HawkinAuth(config = HawkinConfig())
  expect_error(
    hawkinR:::token_seconds_remaining(auth),
    "hd_connect"
  )
})
