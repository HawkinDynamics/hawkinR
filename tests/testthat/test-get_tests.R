# Tests for get_tests() function
# Validates parameter handling, pagination logic, and error conditions.
# All API calls are mocked — no network access required.

test_that("get_tests errors without active connection", {
  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- NULL
  on.exit(.hawkin_env$active_conn <- old_conn)

  expect_error(get_tests(from = "2024-01-01"), "No active connection")
})

test_that("get_tests handles chunk_size parameter without error (deprecated)", {
  skip_on_cran()

  # Set up a valid mock connection
  cfg <- HawkinConfig()
  auth <- HawkinAuth(config = cfg)
  auth@access_token <- "test-token"
  auth@expires_at <- as.POSIXct(Sys.time() + 3600)

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- auth
  on.exit(.hawkin_env$active_conn <- old_conn)

  # Mock httr2 to avoid actual API call — return empty result
  mock_resp <- structure(list(), class = "httr2_response")
  mockery::stub(get_tests, "httr2::req_perform", mock_resp)
  mockery::stub(get_tests, "httr2::resp_status", 200L)
  mockery::stub(get_tests, "httr2::resp_body_json", list(
    data = data.frame(), count = 0, lastSyncTime = 0, lastTestTime = 0,
    hasMore = FALSE, nextCursor = NULL
  ))

  # chunk_size is deprecated but should not cause an error
  # (logger::log_warn is used, which doesn't trigger R's warning() mechanism)
  result <- get_tests(from = "2024-01-01", chunk_size = 30)
  expect_true(is.data.frame(result))
})

test_that("get_tests does not require a 'from' date (pagination fetches all history)", {
  skip_on_cran()

  cfg <- HawkinConfig()
  auth <- HawkinAuth(config = cfg)
  auth@access_token <- "test-token"
  auth@expires_at <- as.POSIXct(Sys.time() + 3600)

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- auth
  on.exit(.hawkin_env$active_conn <- old_conn)

  mock_resp <- structure(list(), class = "httr2_response")
  mockery::stub(get_tests, "httr2::req_perform", mock_resp)
  mockery::stub(get_tests, "httr2::resp_status", 200L)
  mockery::stub(get_tests, "httr2::resp_body_json", list(
    data = data.frame(), count = 0, lastSyncTime = 0, lastTestTime = 0,
    hasMore = FALSE, nextCursor = NULL
  ))

  # No 'from' supplied — should NOT prompt or error, and should return a data frame
  expect_no_error(result <- get_tests())
  expect_true(is.data.frame(result))
})

test_that("get_tests loops through every page until nextCursor is null", {
  skip_on_cran()

  cfg <- HawkinConfig()
  auth <- HawkinAuth(config = cfg)
  auth@access_token <- "test-token"
  auth@expires_at <- as.POSIXct(Sys.time() + 3600)

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- auth
  on.exit(.hawkin_env$active_conn <- old_conn)

  mock_resp <- structure(list(), class = "httr2_response")
  mockery::stub(get_tests, "httr2::req_perform", mock_resp)
  mockery::stub(get_tests, "httr2::resp_status", 200L)

  page1 <- list(
    data = data.frame(id = c("t1", "t2"), timestamp = c(1, 2)),
    count = 2, lastSyncTime = 0, lastTestTime = 0, hasMore = TRUE, nextCursor = "page2"
  )
  page2 <- list(
    data = data.frame(id = "t3", timestamp = 3),
    count = 1, lastSyncTime = 0, lastTestTime = 0, hasMore = FALSE, nextCursor = NULL
  )
  mockery::stub(get_tests, "httr2::resp_body_json", mockery::mock(page1, page2))

  result <- get_tests(from = "2024-01-01")
  expect_equal(nrow(result), 3)
  expect_setequal(result$id, c("t1", "t2", "t3"))
})

test_that("get_tests stops if the pagination cursor fails to advance", {
  skip_on_cran()

  cfg <- HawkinConfig()
  auth <- HawkinAuth(config = cfg)
  auth@access_token <- "test-token"
  auth@expires_at <- as.POSIXct(Sys.time() + 3600)

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- auth
  on.exit(.hawkin_env$active_conn <- old_conn)

  mock_resp <- structure(list(), class = "httr2_response")
  mockery::stub(get_tests, "httr2::req_perform", mock_resp)
  mockery::stub(get_tests, "httr2::resp_status", 200L)

  # Every page returns the SAME non-null cursor (would loop forever unguarded).
  stuck <- list(
    data = data.frame(id = "t1", timestamp = 1),
    count = 1, lastSyncTime = 0, lastTestTime = 0, hasMore = TRUE, nextCursor = "stuck"
  )
  mockery::stub(get_tests, "httr2::resp_body_json", stuck)

  expect_no_error(result <- get_tests(from = "2024-01-01"))
  expect_true(is.data.frame(result))
})

test_that("get_tests rejects a malformed 'from' value", {
  cfg <- HawkinConfig()
  auth <- HawkinAuth(config = cfg)
  auth@access_token <- "test-token"
  auth@expires_at <- as.POSIXct(Sys.time() + 3600)

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- auth
  on.exit(.hawkin_env$active_conn <- old_conn)

  expect_error(get_tests(from = "not-a-date"), "Invalid 'from'")
})
