# Successful execution
test_that("get_teams with valid access token", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTc3NDc5NTMsImV4cCI6MTY5Nzc1MTU1M30.64Zs5iQScatKvnvtYdjFeoIEw0rditm-xTBNN5NGR9A", accessToken_expiration = 1697751553)
  result <- get_teams()
  # Validate the structure and values of the result data frame
  expect_s3_class(result, "data.frame")
  # Add additional checks to validate the content of the data frame.
})

# Validate output structure
test_that("get_teams output structure", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTc3NDc5NTMsImV4cCI6MTY5Nzc1MTU1M30.64Zs5iQScatKvnvtYdjFeoIEw0rditm-xTBNN5NGR9A", accessToken_expiration = 1697751553)
  result <- get_teams()
  # Check the structure of the data frame
  expect_equal(colnames(result), c("id", "name"))
})

# Access token expiration
test_that("get_teams with expired access token", {
  # Set up access token and expiration environment variables with an expired token
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTc3NDc5NTMsImV4cCI6MTY5Nzc1MTU1M30.64Zs5iQScatKvnvtYdjFeoIEw0rditm-xTBNN5NGR9A", accessToken_expiration =169595000)
  expect_error(get_teams())
})

# Access token not available
test_that("get_teams with no access token", {
  # Remove access token environment variable
  Sys.unsetenv("accessToken")
  expect_error(get_teams())
})

