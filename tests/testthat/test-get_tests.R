# Successful execution
test_that("get_tests with valid access token and default parameters", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTc3NDc5NTMsImV4cCI6MTY5Nzc1MTU1M30.64Zs5iQScatKvnvtYdjFeoIEw0rditm-xTBNN5NGR9A", accessToken_expiration = 1697751553)
  result <- get_tests()
  # Validate the structure and values of the result data frame
  expect_s3_class(result, "data.frame")
  # Add additional checks to validate the content of the data frame.
})

# Access token expiration
test_that("get_tests with expired access token", {
  # Set up access token and expiration environment variables with an expired token
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTc3NDc5NTMsImV4cCI6MTY5Nzc1MTU1M30.64Zs5iQScatKvnvtYdjFeoIEw0rditm-xTBNN5NGR9A", accessToken_expiration = 169595000)
  expect_error(get_tests())
})

# Access token not available
test_that("get_tests with no access token", {
  # Remove access token environment variable
  Sys.unsetenv("accessToken")
  expect_error(get_tests())
})

# invalid from type
test_that("get_tests with invalid from", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTc3NDc5NTMsImV4cCI6MTY5Nzc1MTU1M30.64Zs5iQScatKvnvtYdjFeoIEw0rditm-xTBNN5NGR9A", accessToken_expiration = 1697751553)
  expect_error(get_tests(from = "fromDate"))
})

# invalid to type
test_that("get_tests with invalid to", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTc3NDc5NTMsImV4cCI6MTY5Nzc1MTU1M30.64Zs5iQScatKvnvtYdjFeoIEw0rditm-xTBNN5NGR9A", accessToken_expiration = 1697751553)
  expect_error(get_tests(to = "fromDate"))
})
