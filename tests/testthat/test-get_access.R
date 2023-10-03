# No refresh token provided
test_that("get_access token not provided", {
  expect_error(get_access())
})


# Wrong refresh Token provided
test_that("get_access with wrong token", {
  expect_error(get_access("ojpswjfspij"))
})

# Refresh token not character
test_that("get_access token not chr", {
  expect_error(get_access(ijfaoif9a0))
})

# Successful execution
test_that("get_access with valid token", {
  result <- get_access("uI1J4T.f6AYZqUI3XUeuEUaTZdIsCKtl8KML")
  t <- lubridate::with_tz(
    lubridate::as_datetime(
      base::as.numeric(Sys.getenv("accessToken_expiration")),
      tz = "UTC"
    ),
    tzone = base::Sys.timezone()
  )
  msg <- paste(
    "Success! Your access token was recieved and stored for use by other hawkinR functions. Your token will expire at", t)
  expect_equal(result, msg)
})

