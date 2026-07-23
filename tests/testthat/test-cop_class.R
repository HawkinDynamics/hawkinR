# Tests for HawkinCOP S7 class construction
# Validates the class definition and property types.

test_that("HawkinCOP creates with correct property types", {
  cop <- HawkinCOP(
    test_id = "test-123",
    test_sampling_rate = 1000L,
    testType_id = "type-1",
    testType_name = "Free Run",
    testType_canonical = "5pRSUQVSJVnxijpPMck3",
    testType_tags = list(),
    athlete_id = "ath-1",
    athlete_name = "Test Athlete",
    athlete_teams = "team-1",
    athlete_groups = "group-1",
    athlete_active = TRUE,
    athlete_external = list(),
    timestamp = 1700000000L,
    test_date = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    eid = "plate-A",
    data = data.frame(
      time_s = c(0.001, 0.002),
      cop_x = c(1.2, 1.3),
      cop_y = c(0.4, 0.5),
      left_cop_x = c(1.0, NA),
      left_cop_y = c(0.2, NA),
      right_cop_x = c(1.4, 1.5),
      right_cop_y = c(0.6, 0.7)
    )
  )

  expect_equal(cop@test_id, "test-123")
  expect_equal(cop@test_sampling_rate, 1000L)
  expect_equal(cop@testType_canonical, "5pRSUQVSJVnxijpPMck3")
  expect_equal(cop@athlete_name, "Test Athlete")
  expect_true(cop@athlete_active)
  expect_equal(cop@eid, "plate-A")
  expect_equal(nrow(cop@data), 2)
  # Nullable COP entries are preserved as NA
  expect_true(is.na(cop@data$left_cop_x[2]))
})

test_that("HawkinCOP accepts a NULL eid (no plate id)", {
  cop <- HawkinCOP(
    test_id = "t1",
    test_sampling_rate = NA_integer_,
    testType_id = "id",
    testType_name = "Free Run",
    testType_canonical = "5pRSUQVSJVnxijpPMck3",
    testType_tags = list(),
    athlete_id = "a1",
    athlete_name = "Name",
    athlete_teams = "",
    athlete_groups = "",
    athlete_active = TRUE,
    athlete_external = list(),
    timestamp = 0L,
    test_date = as.POSIXct("2024-01-01", tz = "UTC"),
    eid = NULL,
    data = data.frame()
  )

  expect_null(cop@eid)
})
