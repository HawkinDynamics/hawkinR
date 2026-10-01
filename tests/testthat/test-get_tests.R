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

test_that("get_tests errors if the pagination cursor fails to advance", {
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

  # Returning the rows fetched so far would be a truncated export.
  expect_error(get_tests(from = "2024-01-01"), "cursor did not advance on page 2")
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

# ---- useNulls / rounding / nestMetrics (API v1.16) ------------------------

with_mock_conn <- function(code) {
  cfg <- HawkinConfig()
  auth <- HawkinAuth(config = cfg)
  auth@access_token <- "test-token"
  auth@expires_at <- as.POSIXct(Sys.time() + 3600)

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- auth
  on.exit(.hawkin_env$active_conn <- old_conn)

  force(code)
}

empty_page <- list(
  data = data.frame(), count = 0, lastSyncTime = 0, lastTestTime = 0,
  hasMore = FALSE, nextCursor = NULL
)

test_that("get_tests emits useNulls/rounding/nestMetrics only for non-default values", {
  skip_on_cran()

  with_mock_conn({
    captured <- NULL
    mock_resp <- structure(list(), class = "httr2_response")
    mockery::stub(get_tests, "httr2::req_url_query", function(req, ...) {
      captured <<- rlang::list2(...)
      req
    })
    mockery::stub(get_tests, "httr2::req_perform", mock_resp)
    mockery::stub(get_tests, "httr2::resp_status", 200L)
    mockery::stub(get_tests, "httr2::resp_body_json", empty_page)

    get_tests(from = "2024-01-01")
    expect_false(any(c("useNulls", "rounding", "nestMetrics") %in% names(captured)))

    get_tests(from = "2024-01-01", useNulls = TRUE, rounding = FALSE, nestMetrics = FALSE)
    expect_false(any(c("useNulls", "rounding", "nestMetrics") %in% names(captured)))

    get_tests(from = "2024-01-01", useNulls = FALSE, rounding = TRUE, nestMetrics = TRUE)
    expect_equal(captured$useNulls, "false")
    expect_equal(captured$rounding, "true")
    expect_equal(captured$nestMetrics, "true")
  })
})

test_that("get_tests keeps 'N/A' strings when useNulls = FALSE", {
  skip_on_cran()

  with_mock_conn({
    mock_resp <- structure(list(), class = "httr2_response")
    mockery::stub(get_tests, "httr2::req_perform", mock_resp)
    mockery::stub(get_tests, "httr2::resp_status", 200L)

    page <- list(
      data = data.frame(
        id = c("t1", "t2"), timestamp = c(1, 2), segment = c("CMJ:1", "CMJ:2"),
        `Jump Height(m)` = c("N/A", "0.41"), check.names = FALSE
      ),
      count = 2, lastSyncTime = 0, lastTestTime = 0, hasMore = FALSE, nextCursor = NULL
    )
    mockery::stub(get_tests, "httr2::resp_body_json", page)

    result <- get_tests(from = "2024-01-01", useNulls = FALSE)
    expect_equal(nrow(result), 2)
    expect_true("jump_height_m" %in% names(result))
    expect_equal(result$jump_height_m, c("N/A", "0.41"))
  })
})

test_that("get_tests returns a long table when nestMetrics = TRUE", {
  skip_on_cran()

  with_mock_conn({
    mock_resp <- structure(list(), class = "httr2_response")
    mockery::stub(get_tests, "httr2::req_perform", mock_resp)
    mockery::stub(get_tests, "httr2::resp_status", 200L)

    # Shape produced by httr2::resp_body_json(simplifyVector = TRUE) for the
    # nested response: `metrics` is a list column holding one data frame per
    # test (or an empty list when the test has no numeric metrics).
    df <- data.frame(id = c("t1", "t2"), timestamp = c(1, 2), segment = c("CMJ:1", "CMJ:2"))
    df$metrics <- list(
      data.frame(
        metricId = c("jumpHeight", "weight"),
        metricLabel = c("Jump Height", "System Weight"),
        metricUnits = c("m", "N"),
        metricValue = c(0.4123, 800.5)
      ),
      list()
    )
    page <- list(data = df, count = 2, lastSyncTime = 0, lastTestTime = 0,
                 hasMore = FALSE, nextCursor = NULL)
    mockery::stub(get_tests, "httr2::resp_body_json", page)

    result <- get_tests(from = "2024-01-01", nestMetrics = TRUE)

    expect_equal(nrow(result), 3)
    expect_true(all(c("metric_id", "metric_label", "metric_units", "metric_value") %in% names(result)))
    expect_false("metrics" %in% names(result))

    t1 <- result[result$id == "t1", ]
    expect_equal(t1$metric_id, c("jumpHeight", "weight"))
    expect_equal(t1$metric_units, c("m", "N"))
    expect_equal(t1$metric_value, c(0.4123, 800.5))
    expect_equal(t1$segment, c("CMJ:1", "CMJ:1"))

    t2 <- result[result$id == "t2", ]
    expect_equal(nrow(t2), 1)
    expect_true(is.na(t2$metric_id))
  })
})

# ---- Incomplete exports are errors, not partial results (CSE-145) ---------

two_page_first <- list(
  data = data.frame(id = c("t1", "t2"), timestamp = c(1, 2)),
  count = 2, lastSyncTime = 0, lastTestTime = 0, hasMore = TRUE, nextCursor = "page2"
)

test_that("get_tests errors instead of returning partial data when a later page returns non-200", {
  skip_on_cran()

  with_mock_conn({
    mock_resp <- structure(list(), class = "httr2_response")
    mockery::stub(get_tests, "httr2::req_perform", mock_resp)
    mockery::stub(get_tests, "httr2::resp_status", mockery::mock(200L, 500L))
    mockery::stub(get_tests, "httr2::resp_body_json", two_page_first)

    expect_error(get_tests(from = "2024-01-01"), "Error 500.*page 2.*incomplete")
  })
})

test_that("get_tests reports 401 and other statuses with the failing page", {
  skip_on_cran()

  with_mock_conn({
    mock_resp <- structure(list(), class = "httr2_response")
    mockery::stub(get_tests, "httr2::req_perform", mock_resp)
    mockery::stub(get_tests, "httr2::resp_status", 401L)
    err <- tryCatch(get_tests(from = "2024-01-01"), error = identity)
    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "Error 401.*page 1")
    # Nothing was fetched before page 1, so there is no partial export to explain.
    expect_no_match(conditionMessage(err), "incomplete")
  })

  with_mock_conn({
    mock_resp <- structure(list(), class = "httr2_response")
    mockery::stub(get_tests, "httr2::req_perform", mock_resp)
    mockery::stub(get_tests, "httr2::resp_status", mockery::mock(200L, 503L))
    mockery::stub(get_tests, "httr2::resp_body_json", two_page_first)
    expect_error(
      get_tests(from = "2024-01-01"), "Unexpected HTTP status: 503 (page 2)", fixed = TRUE
    )
  })
})

test_that("get_tests errors instead of returning partial data when a later request fails", {
  skip_on_cran()

  with_mock_conn({
    mock_resp <- structure(list(), class = "httr2_response")
    mockery::stub(
      get_tests, "httr2::req_perform",
      mockery::mock(mock_resp, stop("Could not resolve host"))
    )
    mockery::stub(get_tests, "httr2::resp_status", 200L)
    mockery::stub(get_tests, "httr2::resp_body_json", two_page_first)

    expect_error(
      get_tests(from = "2024-01-01"),
      "Request failed on page 2: Could not resolve host.*incomplete"
    )
  })
})

test_that("get_tests errors with the failing page when a 200 body cannot be parsed", {
  skip_on_cran()

  with_mock_conn({
    mock_resp <- structure(list(), class = "httr2_response")
    mockery::stub(get_tests, "httr2::req_perform", mock_resp)
    mockery::stub(get_tests, "httr2::resp_status", 200L)
    mockery::stub(
      get_tests, "httr2::resp_body_json",
      mockery::mock(two_page_first, stop("lexical error: invalid char in json text"))
    )

    expect_error(
      get_tests(from = "2024-01-01"),
      "Could not parse the response on page 2: lexical error.*incomplete"
    )
  })
})

test_that("get_tests retries rate-limit and gateway errors only", {
  skip_on_cran()

  with_mock_conn({
    captured <- NULL
    mockery::stub(get_tests, "httr2::req_retry", function(req, ...) {
      captured <<- list(...)
      req
    })
    # force(req) so the lazily-evaluated request pipeline (and req_retry) runs.
    mockery::stub(get_tests, "httr2::req_perform", function(req, ...) {
      force(req)
      httr2::response(200)
    })
    mockery::stub(get_tests, "httr2::resp_body_json", empty_page)

    get_tests(from = "2024-01-01")

    expect_equal(captured$max_tries, 3)
    for (status in c(429, 502, 503, 504)) {
      expect_true(captured$is_transient(httr2::response(status)), label = status)
    }
    for (status in c(200, 400, 401, 404, 500)) {
      expect_false(captured$is_transient(httr2::response(status)), label = status)
    }
  })
})
