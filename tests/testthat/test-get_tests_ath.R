# Successful Execution
test_that("get_tests with valid access token and default parameters", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA", accessToken_expiration = 1695965923)
  result <- get_tests_ath(athleteId = "CpLBaqEBQPOs6SKqfLS6")
  # Validate the structure and values of the result data frame
  expect_s3_class(result, "data.frame")
  # Add additional checks to validate the content of the data frame.
})

# Access token expired
test_that("get_tests_ath with expired access token", {
  # Set up access token and expiration environment variables with an expired token
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA", accessToken_expiration = 1695960000)
  expect_error(get_tests_ath(athleteId = "CpLBaqEBQPOs6SKqfLS6"))
})

# Access token not available
test_that("get_tests_ath with no access token", {
  # Remove access token environment variable
  Sys.unsetenv("accessToken")
  expect_error(get_tests_ath(athleteId = "CpLBaqEBQPOs6SKqfLS6"))
})

# Invalid athleteId
test_that("get_tests_ath with invalid atheteId", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA", accessToken_expiration = 1695965923)
  expect_error(get_tests_ath(athleteId = "invalid_athId"))
})

# invalid from type
test_that("get_tests_ath with invalid from", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA", accessToken_expiration = 1695965923)
  expect_error(get_tests_ath(athleteId = "CpLBaqEBQPOs6SKqfLS6", from = "fromDate"))
})

# invalid to type
test_that("get_tests_ath with invalid to", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA", accessToken_expiration = 1695965923)
  expect_error(get_tests_ath(athleteId = "CpLBaqEBQPOs6SKqfLS6", to = "fromDate"))
})
