# Tests for connection management: set_active_conn, get_active_conn, hd_connect
# These tests validate internal state management without network calls.

test_that("get_active_conn errors when no connection is set", {
  # Ensure no active connection
  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- NULL
  on.exit(.hawkin_env$active_conn <- old_conn)

  expect_error(
    hawkinR:::get_active_conn(),
    "No active connection"
  )
})

test_that("set_active_conn stores and get_active_conn retrieves", {
  cfg <- HawkinConfig()
  auth <- HawkinAuth(config = cfg)
  auth@access_token <- "test-token"
  auth@expires_at <- as.POSIXct(Sys.time() + 3600)

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  on.exit(.hawkin_env$active_conn <- old_conn)

  hawkinR:::set_active_conn(auth)
  retrieved <- hawkinR:::get_active_conn()

  expect_equal(retrieved@access_token, "test-token")
  expect_equal(retrieved@config@profile, "default")
})

test_that("set_active_conn rejects non-HawkinAuth objects", {
  expect_error(
    hawkinR:::set_active_conn(list(token = "abc")),
    "HawkinAuth"
  )
})

test_that("hd_connect errors without stored credentials", {
  skip_on_cran()
  # Attempting to connect with a non-existent profile should fail
  expect_error(
    hd_connect(profile = "nonexistent_test_profile_xyz"),
    "credentials|authenticate|failed"
  )
})
