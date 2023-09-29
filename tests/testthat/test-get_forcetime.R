# Successful execution
test_that("get_forcetime with valid access token and testId", {
  # Set up access token and expiration environment variables
  Sys.setenv(
    accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA",
    accessToken_expiration = 1695965923
  )
  result <- get_forcetime("X0vGP2xyb1wx8qjaGRFa")
  # Validate the structure and values of the result data frame
  expect_s3_class(result, "data.frame")
  # Add additional checks to validate the content of the data frame.
})

# Access Token Expiration
test_that("get_forcetime with expired access token", {
  # Set up access token and expiration environment variables with an expired token
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA"
, accessToken_expiration = 169595000)
  expect_error(get_forcetime("X0vGP2xyb1wx8qjaGRFa"))
})

# Access Token not available
test_that("get_forcetime with no access token", {
  # Remove access token environment variable
  Sys.unsetenv("accessToken")
  expect_error(get_forcetime("X0vGP2xyb1wx8qjaGRFa"))
})

# Invalid testId param
test_that("get_forcetime with invalid testId", {
  # Set up access token and expiration environment variables
  Sys.setenv(
    accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTU5NjIzMjMsImV4cCI6MTY5NTk2NTkyM30.B0scvSbwrsGTPnCii24M7otdJcDr-Fc63HG6vTiluRA",
    accessToken_expiration = 1695965923
    )
  expect_error(get_forcetime(12345))
})




