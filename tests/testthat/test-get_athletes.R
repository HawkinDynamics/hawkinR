# Successful Execution
test_that("get_athletes success", {

  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA", accessToken_expiration = 1695965923)

  result <- get_athletes()

  # Validate the structure and values of the result data frame
  expect_s3_class(result, "data.frame")
})

# Access Token Expired
test_that("get_athletes with expired access token", {
  # Set up access token and expiration environment variables with an expired token
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA", accessToken_expiration = 169595000)
  expect_error(get_athletes())
})

# Access token not available
test_that("get_athletes with no access token", {
  # Remove access token environment variable
  Sys.unsetenv("accessToken")
  expect_error(get_athletes())
})

# Invalid token response
test_that("get_athletes with invalid token response", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "invalid_access_token", accessToken_expiration = 1695965923)
  expect_error(get_athletes())
})
