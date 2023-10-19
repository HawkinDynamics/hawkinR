# Successful Execution
test_that("get_groups with valid access token", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTc3NDc5NTMsImV4cCI6MTY5Nzc1MTU1M30.64Zs5iQScatKvnvtYdjFeoIEw0rditm-xTBNN5NGR9A", accessToken_expiration = 1697751553)
  result <- get_testTypes()
  # Validate the structure and values of the result data frame
  expect_s3_class(result, "data.frame")
  # Add additional checks to validate the content of the data frame.
})

# Validate output structure
test_that("get_groups output structure", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTc3NDc5NTMsImV4cCI6MTY5Nzc1MTU1M30.64Zs5iQScatKvnvtYdjFeoIEw0rditm-xTBNN5NGR9A", accessToken_expiration = 1697751553)
  result <- get_testTypes()
  # Check the structure of the data frame
  expect_equal(colnames(result), c("name", "id"))
})

# Access Token Expiration
test_that("get_groups with expired access token", {
  # Set up access token and expiration environment variables with an expired token
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTc3NDc5NTMsImV4cCI6MTY5Nzc1MTU1M30.64Zs5iQScatKvnvtYdjFeoIEw0rditm-xTBNN5NGR9A", accessToken_expiration =169595000)
  expect_error(get_testTypes())
})

# Access Token not available
test_that("get_groups with no access token", {
  # Remove access token environment variable
  Sys.unsetenv("accessToken")
  expect_error(get_testTypes())
})

