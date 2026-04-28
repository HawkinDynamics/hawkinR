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

test_that("get_tests requires from date in non-interactive mode", {
  cfg <- HawkinConfig()
  auth <- HawkinAuth(config = cfg)
  auth@access_token <- "test-token"
  auth@expires_at <- as.POSIXct(Sys.time() + 3600)

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- auth
  on.exit(.hawkin_env$active_conn <- old_conn)

  expect_error(get_tests(), "requires a 'from' date")
})
