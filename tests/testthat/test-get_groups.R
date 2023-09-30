# Successful Execution
test_that("get_groups with valid access token", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTYwNTgxMzUsImV4cCI6MTY5NjA2MTczNX0.C1YfTKZprFQDVTswDdlDvmVHba807okSTrL12Gs0F98", accessToken_expiration = 1696061735)
  result <- get_groups()
  # Validate the structure and values of the result data frame
  expect_s3_class(result, "data.frame")
  # Add additional checks to validate the content of the data frame.
})

# Validate output structure
test_that("get_groups output structure", {
  # Set up access token and expiration environment variables
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTYwNTgxMzUsImV4cCI6MTY5NjA2MTczNX0.C1YfTKZprFQDVTswDdlDvmVHba807okSTrL12Gs0F98", accessToken_expiration = 1696061735)
  result <- get_groups()
  # Check the structure of the data frame
  expect_equal(colnames(result), c("id", "name"))
})

# Access Token Expiration
test_that("get_groups with expired access token", {
  # Set up access token and expiration environment variables with an expired token
  Sys.setenv(accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkYXRhIjp7Im9yZ0lkIjoiRFBNYjZlazJtZ1VOVmNnOHNpU3Fwbkl2RTJpMiIsInNjb3BlcyI6Ilt0ZXN0cy5yZWFkXSJ9LCJpYXQiOjE2OTYwNTgxMzUsImV4cCI6MTY5NjA2MTczNX0.C1YfTKZprFQDVTswDdlDvmVHba807okSTrL12Gs0F98", accessToken_expiration =169595000)
  expect_error(get_groups())
})

# Access Token not available
test_that("get_groups with no access token", {
  # Remove access token environment variable
  Sys.unsetenv("accessToken")
  expect_error(get_groups())
})

