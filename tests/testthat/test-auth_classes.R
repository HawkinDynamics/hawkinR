# Tests for S7 authentication classes: HawkinConfig and HawkinAuth
# These tests validate class construction, property defaults, and validators
# without making any network calls.

test_that("HawkinConfig creates with correct defaults", {
  cfg <- HawkinConfig()
  expect_equal(cfg@profile, "default")
  expect_equal(cfg@org_id, "v1")
  expect_equal(cfg@environment, "development")
  expect_equal(cfg@log_level, "INFO")
})

test_that("HawkinConfig accepts custom values", {
  cfg <- HawkinConfig(
    profile = "prod",
    org_id = "custom-org",
    environment = "production",
    log_level = "DEBUG"
  )
  expect_equal(cfg@profile, "prod")
  expect_equal(cfg@org_id, "custom-org")
  expect_equal(cfg@environment, "production")
  expect_equal(cfg@log_level, "DEBUG")
})

test_that("HawkinConfig rejects invalid environment", {
  expect_error(
    HawkinConfig(environment = "staging"),
    "development.*production"
  )
})

test_that("HawkinAuth computes correct base_url per region", {
  cfg <- HawkinConfig()

  auth_us <- HawkinAuth(config = cfg, region = "Americas")
  expect_equal(auth_us@base_url, "https://cloud.hawkindynamics.com/api")

  auth_eu <- HawkinAuth(config = cfg, region = "Europe")
  expect_equal(auth_eu@base_url, "https://eu.cloud.hawkindynamics.com/api")

  auth_apac <- HawkinAuth(config = cfg, region = "APAC")
  expect_equal(auth_apac@base_url, "https://apac.cloud.hawkindynamics.com/api")

  # Also accepts full name

  auth_apac2 <- HawkinAuth(config = cfg, region = "Asia/Pacific")
  expect_equal(auth_apac2@base_url, "https://apac.cloud.hawkindynamics.com/api")
})

test_that("HawkinAuth rejects invalid region", {
  cfg <- HawkinConfig()
  auth <- HawkinAuth(config = cfg, region = "Antarctica")
  expect_error(auth@base_url, "Invalid region")
})

test_that("HawkinAuth rejects non-POSIXct expires_at", {
  cfg <- HawkinConfig()
  expect_error(
    HawkinAuth(config = cfg, expires_at = "not-a-date"),
    "POSIXct"
  )
})

test_that("HawkinAuth accepts NULL access_token and expires_at", {
  cfg <- HawkinConfig()
  auth <- HawkinAuth(config = cfg)
  expect_null(auth@access_token)
  expect_null(auth@expires_at)
})
