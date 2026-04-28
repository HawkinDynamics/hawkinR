# =============================================================================
# hawkinR v2 — Stress Test against Dev API
# =============================================================================
#
# Real end-to-end test of every public API function against
# https://cloud.dev.hawkindynamics.com/api. Intended for pre-release validation;
# not included in the CRAN tarball (this file lives under dev/ which is in
# .Rbuildignore).
#
# SETUP (run these once, before sourcing this file):
#
#   # 1. Set the dev refresh token (this value never lives in the committed script).
#   Sys.setenv(HAWKIN_KEY_DEV = "<your dev refresh token>")
#
#   # 2. Point the package at the dev API via the internal env var override.
#   Sys.setenv(HAWKINR_API_BASE_URL_OVERRIDE = "https://cloud.dev.hawkindynamics.com/api")
#
# THEN, from the repo root:
#
#   setwd("packages/hawkinR")
#   devtools::load_all(".")
#   source("dev/stress_test_dev.R")
#
# OUTPUT:
#   - Pass/fail summary printed to console
#   - Detailed log written to dev/stress_test_dev_<timestamp>.log
# =============================================================================

# --- Preflight checks --------------------------------------------------------

if (!nzchar(Sys.getenv("HAWKIN_KEY_DEV"))) {
  stop(
    "HAWKIN_KEY_DEV env var is not set. See the setup block at the top of ",
    "this file for instructions.",
    call. = FALSE
  )
}
if (!nzchar(Sys.getenv("HAWKINR_API_BASE_URL_OVERRIDE"))) {
  message("[setup] HAWKINR_API_BASE_URL_OVERRIDE not set — pointing at dev.")
  Sys.setenv(
    HAWKINR_API_BASE_URL_OVERRIDE = "https://cloud.dev.hawkindynamics.com/api"
  )
}

# --- Results table + helper --------------------------------------------------

.stress_results <- data.frame(
  step     = character(),
  status   = character(),
  rows     = integer(),
  elapsed  = numeric(),
  message  = character(),
  stringsAsFactors = FALSE
)

run_step <- function(name, expr) {
  cat(sprintf("\n=== %s ===\n", name))
  t0 <- Sys.time()
  status <- "PASS"
  rows <- NA_integer_
  msg <- ""
  res <- NULL
  tryCatch({
    res <- force(expr)
    if (is.data.frame(res)) {
      rows <- nrow(res)
    } else if (is.list(res) && !is.null(names(res))) {
      rows <- length(res)
    } else if (is.vector(res)) {
      rows <- length(res)
    }
    cat(sprintf("  -> OK (rows=%s)\n", format(rows)))
  }, error = function(e) {
    status <<- "FAIL"
    msg <<- conditionMessage(e)
    cat(sprintf("  -> FAIL: %s\n", msg))
  })
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3)
  .stress_results[nrow(.stress_results) + 1L, ] <<- list(
    name, status, rows, elapsed, msg
  )
  invisible(res)
}

# Variant of run_step for cases where an error IS the expected outcome.
expect_error_step <- function(name, expr, pattern = NULL) {
  cat(sprintf("\n=== %s (expect error) ===\n", name))
  t0 <- Sys.time()
  status <- "FAIL"
  msg <- "No error was thrown, but one was expected."
  tryCatch({
    force(expr)
  }, error = function(e) {
    caught <- conditionMessage(e)
    if (is.null(pattern) || grepl(pattern, caught, fixed = FALSE)) {
      status <<- "PASS"
      msg <<- paste0("Caught expected error: ", caught)
      cat(sprintf("  -> OK (caught: %s)\n", caught))
    } else {
      msg <<- paste0("Caught wrong error: ", caught)
      cat(sprintf("  -> FAIL: wrong error: %s\n", caught))
    }
  })
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3)
  .stress_results[nrow(.stress_results) + 1L, ] <<- list(
    name, status, NA_integer_, elapsed, msg
  )
  invisible(NULL)
}

# --- Connect -----------------------------------------------------------------

run_step("connect: hd_connect (DEV profile, prod env for env-var creds)", {
  hd_connect(
    profile     = "DEV",
    environment = "production",      # tells package to read HAWKIN_KEY_DEV
    region      = "Americas",        # ignored when override env var is set
    log_level   = "INFO"
  )
})

# Stop if we couldn't connect — downstream steps would all fail with the same
# message, polluting the summary.
if (tail(.stress_results$status, 1) == "FAIL") {
  stop("Could not authenticate. See connect step message.", call. = FALSE)
}

# --- Metadata endpoints ------------------------------------------------------

teams      <- run_step("metadata: get_teams",       get_teams())
groups     <- run_step("metadata: get_groups",      get_groups())
tags       <- run_step("metadata: get_tags",        get_tags())
test_types <- run_step("metadata: get_testTypes",   get_testTypes())
metrics    <- run_step("metadata: get_metrics",     get_metrics())
ath_active <- run_step("metadata: get_athletes (active only)", get_athletes())
ath_all    <- run_step(
  "metadata: get_athletes (includeInactive = TRUE)",
  get_athletes(includeInactive = TRUE)
)

# --- Tests: core pagination --------------------------------------------------
#
# The Hawkin dev database has thousands of tests going back 4+ years, and the
# API paginates at 1,000 tests per page. To meaningfully stress the cursor
# loop we need a window big enough to cross that threshold. Windows smaller
# than ~1 year often fit in a single page on this dataset.

today    <- Sys.Date()
d_small  <- format(today - 30,          "%Y-%m-%d")   # ~single page, sanity
d_medium <- format(today - 365,         "%Y-%m-%d")   # likely multi-page
d_large  <- format(today - (365 * 4),   "%Y-%m-%d")   # 4 years — multi-page guaranteed
d_epoch  <- "2015-01-01"                              # all-time, max stress

t_small  <- run_step("tests: last 30 days (single-page sanity)", get_tests(from = d_small))
t_medium <- run_step("tests: last 365 days",                      get_tests(from = d_medium))
t_large  <- run_step("tests: last 4 years (multi-page cursor)",   get_tests(from = d_large))
t_all    <- run_step("tests: all-time since 2015 (max pagination stress)",
                     get_tests(from = d_epoch))

# --- Tests: filters (from/to required) ---------------------------------------
#
# Filtered queries use the 4-year window so even sparsely-populated filters
# can return enough rows to cross the pagination threshold when applicable.

filter_from <- d_large
filter_to   <- format(today, "%Y-%m-%d")

if (is.data.frame(ath_active) && nrow(ath_active) > 0) {
  sample_athlete_id <- ath_active$id[1]
  run_step(
    "tests: filter by athleteId (single, 4 years)",
    get_tests(from = filter_from, to = filter_to, athleteId = sample_athlete_id)
  )
}

if (is.data.frame(teams) && nrow(teams) > 0) {
  run_step(
    "tests: filter by teamId (single, 4 years)",
    get_tests(from = filter_from, to = filter_to, teamId = teams$id[1])
  )
  if (nrow(teams) >= 2) {
    multi_team_ids <- head(teams$id, 10L)
    run_step(
      sprintf("tests: filter by teamId (multi, n=%d, 4 years)", length(multi_team_ids)),
      get_tests(
        from   = filter_from,
        to     = filter_to,
        teamId = multi_team_ids
      )
    )
  }
}

if (is.data.frame(groups) && nrow(groups) > 0) {
  run_step(
    "tests: filter by groupId (single, 4 years)",
    get_tests(from = filter_from, to = filter_to, groupId = groups$id[1])
  )
}

if (is.data.frame(test_types) && nrow(test_types) > 0) {
  run_step(
    "tests: filter by typeId (single, 4 years)",
    get_tests(from = filter_from, to = filter_to, typeId = test_types$canonicalId[1])
  )
}

# --- Tests: sync mode --------------------------------------------------------
#
# Use a 1-year syncFrom so we get a meaningful result set. With syncFrom
# covering everything created/updated since then, large orgs easily exceed
# the 1,000 test page limit and exercise cursor pagination in sync mode too.

sync_from_epoch <- as.character(as.integer(as.POSIXct(d_medium, tz = "UTC")))
run_step(
  "tests: sync mode (syncFrom 1 year ago, multi-page)",
  get_tests(sync = TRUE, from = sync_from_epoch)
)

# --- Tests: includeInactive toggle ------------------------------------------

run_step(
  "tests: includeInactive = TRUE (1 year)",
  get_tests(from = d_medium, includeInactive = TRUE)
)

# --- Forcetime ---------------------------------------------------------------

# Pick a sample test ID from the smallest successful fetch (most recent data).
sample_test_id <- NULL
for (cand in list(t_small, t_medium, t_large, t_all)) {
  if (is.data.frame(cand) && nrow(cand) > 0) {
    if ("id" %in% names(cand)) {
      sample_test_id <- cand$id[1]
      break
    }
  }
}

if (!is.null(sample_test_id)) {
  run_step(
    sprintf("forcetime: single (id=%s)", sample_test_id),
    get_forcetime(testId = sample_test_id)
  )
}

# Bulk: pick up to 10 IDs from the 30-day window
if (is.data.frame(t_small) && nrow(t_small) > 0 && "id" %in% names(t_small)) {
  bulk_ids <- head(t_small$id, 10L)
  run_step(
    sprintf("forcetime: bulk (n=%d)", length(bulk_ids)),
    get_forcetime_bulk(test_ids = bulk_ids)
  )
}

# --- Token refresh forced ----------------------------------------------------

run_step("token-refresh: force expiry and re-call", {
  conn <- hawkinR:::get_active_conn()
  conn@expires_at <- Sys.time() - 1L
  hawkinR:::set_active_conn(conn)
  # This call should trigger a transparent refresh before its own request.
  get_teams()
})

# --- Error paths -------------------------------------------------------------

expect_error_step(
  "error: get_forcetime with bogus ID",
  get_forcetime(testId = "does-not-exist-xxxxxxxxxxxxxxxxxxxxxxxxx"),
  pattern = NULL
)

expect_error_step(
  "error: get_tests with invalid 'from' in non-interactive branch",
  {
    # Pass a clearly invalid date; function should reject it.
    get_tests(from = "not-a-date")
  },
  pattern = "Invalid|from|date"
)

# --- Bulk athletes (safe, clearly-labeled test record) ----------------------

stress_name <- sprintf(
  "STRESSTEST_%s",
  format(Sys.time(), "%Y%m%d_%H%M%S")
)

created_id <- NULL
run_step(
  sprintf("athletes: create_athletes (%s)", stress_name),
  {
    payload <- data.frame(
      name   = stress_name,
      active = TRUE,
      stringsAsFactors = FALSE
    )
    res <- create_athletes(athleteData = payload)
    # create_athletes returns NULL on success; re-fetch athletes to get the ID.
    roster <- get_athletes(includeInactive = TRUE)
    hit <- roster[roster$name == stress_name, , drop = FALSE]
    if (nrow(hit) > 0) created_id <<- hit$id[1]
    res
  }
)

if (!is.null(created_id)) {
  run_step(
    sprintf("athletes: update_athletes (%s -> active=FALSE)", stress_name),
    {
      payload <- data.frame(
        id     = created_id,
        name   = stress_name,
        active = FALSE,
        stringsAsFactors = FALSE
      )
      update_athletes(athleteData = payload)
    }
  )
}

# --- Summary -----------------------------------------------------------------

cat("\n\n================== STRESS TEST SUMMARY ==================\n")
print(.stress_results, row.names = FALSE)
cat("---------------------------------------------------------\n")
n_pass <- sum(.stress_results$status == "PASS")
n_fail <- sum(.stress_results$status == "FAIL")
cat(sprintf(
  "Result: %d / %d PASS (%d FAIL). Total time: %.1fs.\n",
  n_pass, nrow(.stress_results), n_fail, sum(.stress_results$elapsed, na.rm = TRUE)
))
cat("=========================================================\n\n")

# Write full log ---------------------------------------------------------------

log_dir <- "dev"
if (!dir.exists(log_dir)) log_dir <- "."
log_path <- file.path(
  log_dir,
  sprintf("stress_test_dev_%s.log", format(Sys.time(), "%Y%m%d_%H%M%S"))
)
tryCatch({
  write.csv(.stress_results, log_path, row.names = FALSE)
  cat(sprintf("Detailed log written to: %s\n", log_path))
}, error = function(e) {
  cat(sprintf("Could not write log to %s: %s\n", log_path, conditionMessage(e)))
})

invisible(.stress_results)
