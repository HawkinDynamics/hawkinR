# Tests for HawkinForceTime S7 class construction
# Validates the class definition and property types.

test_that("HawkinForceTime creates with correct property types", {
  ft <- HawkinForceTime(
    test_id = "test-123",
    test_sampling_rate = 1000L,
    testType_id = "type-1",
    testType_name = "Countermovement Jump",
    testType_canonical = "7nNduHeM5zETPjHxvm7s",
    testType_tags = list(),
    athlete_id = "ath-1",
    athlete_name = "Test Athlete",
    athlete_teams = "team-1",
    athlete_groups = "group-1",
    athlete_active = TRUE,
    athlete_external = list(),
    timestamp = 1700000000L,
    test_date = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    data = data.frame(time_s = c(0.001, 0.002), combined_force_N = c(500, 510)),
    data_rsi = NULL
  )

  expect_equal(ft@test_id, "test-123")
  expect_equal(ft@test_sampling_rate, 1000L)
  expect_equal(ft@testType_canonical, "7nNduHeM5zETPjHxvm7s")
  expect_equal(ft@athlete_name, "Test Athlete")
  expect_true(ft@athlete_active)
  expect_equal(nrow(ft@data), 2)
})

test_that("HawkinForceTime property testType_canonical is correctly named", {
  # Verify the typo fix (was testType_canoncical, now testType_canonical)
  ft <- HawkinForceTime(
    test_id = "t1",
    test_sampling_rate = 1000L,
    testType_id = "id",
    testType_name = "Test",
    testType_canonical = "canonical-id",
    testType_tags = list(),
    athlete_id = "a1",
    athlete_name = "Name",
    athlete_teams = "",
    athlete_groups = "",
    athlete_active = TRUE,
    athlete_external = list(),
    timestamp = 0L,
    test_date = as.POSIXct("2024-01-01", tz = "UTC"),
    data = data.frame(),
    data_rsi = NULL
  )

  expect_equal(ft@testType_canonical, "canonical-id")
})
