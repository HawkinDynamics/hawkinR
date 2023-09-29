# Successful execution
test_that("get_tests_group with valid access token and default parameters", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA", accessToken_expiration = 1695965923)
  result <- get_tests_group(groupId = "egjaTWU21wUq21J3XVZN")
  # Validate the structure and values of the result data frame
  expect_s3_class(result, "data.frame")
  # Add additional checks to validate the content of the data frame.
})

# Access token expiration
test_that("get_tests_group with expired access token", {
  # Set up access token and expiration environment variables with an expired token
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA", accessToken_expiration = 169595000)
  expect_error(get_tests_group(groupId = "egjaTWU21wUq21J3XVZN"))
})

# Access token not available
test_that("get_tests_group with no access token", {
  # Remove access token environment variable
  Sys.unsetenv("accessToken")
  expect_error(get_tests_group(groupId = "egjaTWU21wUq21J3XVZN"))
})

# Incorrect groupId
test_that("get_tests_group with expired access token", {
  # Set up access token and expiration environment variables with an expired token
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA", accessToken_expiration = 1695965923)
  expect_error(get_tests_group(groupId = "96baa7ef1443c7a219702eb22e3"))
})

# invalid from type
test_that("get_tests_group with invalid from", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA", accessToken_expiration = 1695965923)
  expect_error(get_tests_group(groupId = "egjaTWU21wUq21J3XVZN", from = "fromDate"))
})

# invalid to type
test_that("get_tests_group with invalid to", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA", accessToken_expiration = 1695965923)
  expect_error(get_tests_group(groupId = "egjaTWU21wUq21J3XVZN", to = "fromDate"))
})


