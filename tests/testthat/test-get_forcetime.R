# Tests for get_forcetime() response parsing.
# All API calls are mocked — no network access required.
#
# These guard the column mapping specifically: the response is indexed by field
# NAME, so a reordered or partially-omitted payload must never shift a vector
# onto the wrong column label.

# Helper: a minimal but complete force-time payload. Every series carries
# deliberately distinct magnitudes so any column misalignment is visible.
ft_payload <- function(extra = list()) {
  base <- list(
    id = "test-123",
    testType = list(
      id          = "tt-id-123",
      name        = "Countermovement Jump",
      canonicalId = "7nNduHeM5zETPjHxvm7s",
      tags        = NULL
    ),
    athlete = list(
      id       = "ath-1",
      name     = "Test Athlete",
      teams    = "Team A",
      groups   = "Group A",
      active   = TRUE,
      external = list()
    ),
    timestamp           = 1551301560,
    `Time(s)`           = c(0.001, 0.002, 0.003),
    `LeftForce(N)`      = c(100, 101, 102),
    `RightForce(N)`     = c(200, 201, 202),
    `CombinedForce(N)`  = c(300, 302, 304),
    `Velocity(m/s)`     = c(0, 0.1, 0.2),
    `Displacement(m)`   = c(0, 0.01, 0.02),
    `Power(W)`          = c(0, 10, 20),
    rsi                 = "NA"
  )
  utils::modifyList(base, extra)
}

mock_conn <- function() {
  cfg <- HawkinConfig()
  auth <- HawkinAuth(config = cfg)
  auth@access_token <- "test-token"
  auth@expires_at <- as.POSIXct(Sys.time() + 3600)
  auth
}

test_that("get_forcetime populates each force column from its own field (no positional shift)", {
  skip_on_cran()

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- mock_conn()
  on.exit(.hawkin_env$active_conn <- old_conn)

  mock_resp <- structure(list(), class = "httr2_response")
  mockery::stub(get_forcetime, "httr2::req_perform", mock_resp)
  mockery::stub(get_forcetime, "httr2::resp_status", 200L)
  mockery::stub(get_forcetime, "httr2::resp_body_json", ft_payload())

  ft <- get_forcetime(testId = "test-123")

  # The regression: each column must hold its own field, not a shifted neighbour.
  expect_equal(ft@data$left_force_N, c(100, 101, 102))
  expect_equal(ft@data$right_force_N, c(200, 201, 202))

  # Remaining series land on their own labels.
  expect_equal(ft@data$time_s, c(0.001, 0.002, 0.003))
  expect_equal(ft@data$combined_force_N, c(300, 302, 304))
  expect_equal(ft@data$power_W, c(0, 10, 20))
})

test_that("get_forcetime reads testType id, not a neighbouring field", {
  skip_on_cran()

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- mock_conn()
  on.exit(.hawkin_env$active_conn <- old_conn)

  mock_resp <- structure(list(), class = "httr2_response")
  mockery::stub(get_forcetime, "httr2::req_perform", mock_resp)
  mockery::stub(get_forcetime, "httr2::resp_status", 200L)
  mockery::stub(get_forcetime, "httr2::resp_body_json", ft_payload())

  ft <- get_forcetime(testId = "test-123")

  expect_equal(ft@testType_id, "tt-id-123")
  expect_equal(ft@testType_canonical, "7nNduHeM5zETPjHxvm7s")
})

test_that("get_forcetime maps tri-axial columns by name when present", {
  skip_on_cran()

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- mock_conn()
  on.exit(.hawkin_env$active_conn <- old_conn)

  payload <- ft_payload(list(
    `XLeftForce(N)`    = c(11, 12, 13),
    `XRightForce(N)`   = c(21, 22, 23),
    `YLeftForce(N)`    = c(31, 32, 33),
    `YRightForce(N)`   = c(41, 42, 43),
    `XLeftMoments(Nm)` = c(51, 52, 53)
  ))

  mock_resp <- structure(list(), class = "httr2_response")
  mockery::stub(get_forcetime, "httr2::req_perform", mock_resp)
  mockery::stub(get_forcetime, "httr2::resp_status", 200L)
  mockery::stub(get_forcetime, "httr2::resp_body_json", payload)

  ft <- get_forcetime(testId = "test-123")

  expect_equal(ft@data$x_left_force_N, c(11, 12, 13))
  expect_equal(ft@data$x_right_force_N, c(21, 22, 23))
  expect_equal(ft@data$y_left_force_N, c(31, 32, 33))
  expect_equal(ft@data$y_right_force_N, c(41, 42, 43))
  expect_equal(ft@data$x_left_moments, c(51, 52, 53))

  # Core columns are unaffected by the extra fields.
  expect_equal(ft@data$left_force_N, c(100, 101, 102))
  expect_equal(ft@data$right_force_N, c(200, 201, 202))
})

test_that("get_forcetime omits absent tri-axial columns without shifting others", {
  skip_on_cran()

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- mock_conn()
  on.exit(.hawkin_env$active_conn <- old_conn)

  mock_resp <- structure(list(), class = "httr2_response")
  mockery::stub(get_forcetime, "httr2::req_perform", mock_resp)
  mockery::stub(get_forcetime, "httr2::resp_status", 200L)
  mockery::stub(get_forcetime, "httr2::resp_body_json", ft_payload())

  ft <- get_forcetime(testId = "test-123")

  expect_false("x_left_force_N" %in% names(ft@data))
  expect_equal(
    names(ft@data),
    c("time_s", "left_force_N", "right_force_N", "combined_force_N",
      "velocity_m_s", "displacement_m", "power_W")
  )
})
