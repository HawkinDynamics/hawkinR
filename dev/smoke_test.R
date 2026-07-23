# =============================================================================
# hawkinR — End-to-End Smoke Test
# =============================================================================
#
# PURPOSE
#   Exercise every public function in hawkinR once, in the order a real user
#   would call them, against a live Hawkin Dynamics API. This is a *smoke test*
#   (one representative call per scenario, pass/fail recorded) rather than the
#   pagination/stress harness in dev/stress_test_dev.R.
#
#   It confirms: auth + connection, offline metadata, org metadata, test
#   queries (every filter), force-time (single + bulk + export), the optional
#   write path (create/update athletes), expected error paths, and transparent
#   token refresh.
#
# -----------------------------------------------------------------------------
# HOW TO RUN
# -----------------------------------------------------------------------------
#   1. Put this file at packages/hawkinR/dev/smoke_test.R  (dev/ is in
#      .Rbuildignore, so it is never shipped to CRAN).
#   2. Add your token in the CONFIG block below (or set it as an env var —
#      see the two supported modes).
#   3. From the repo root:
#         setwd("packages/hawkinR")
#         devtools::load_all(".")
#         source("dev/smoke_test.R")
#
# -----------------------------------------------------------------------------
# TWO WAYS TO SUPPLY THE TOKEN
# -----------------------------------------------------------------------------
#   MODE A — "development" (keyring):
#     The package reads your refresh token from the OS credential store
#     (Windows Credential Manager / macOS Keychain) via the keyring package.
#     If you paste a token below, this script will store it there for you using
#     hd_auth_store(). NOTE: hd_auth_store() requires an INTERACTIVE session
#     (RStudio is fine; `Rscript` is not), because it guards credential entry.
#
#   MODE B — "production" (environment variable):
#     The package reads the token from HAWKIN_KEY_<PROFILE> (profile upper-cased).
#     This works headless (Rscript, CI). If you paste a token below, the script
#     sets that env var for the current session. This is the most portable mode
#     and is what dev/stress_test_dev.R uses.
#
#   If you leave HD_TOKEN = "" the script assumes the credential already exists
#   (keyring entry for MODE A, or HAWKIN_KEY_<PROFILE> already exported for
#   MODE B) and just connects.
#
# -----------------------------------------------------------------------------
# SECURITY NOTE
# -----------------------------------------------------------------------------
#   Pasting a token into HD_TOKEN can leave it in your .Rhistory. Prefer setting
#   it as an environment variable in .Renviron and leaving HD_TOKEN = "". This
#   file lives under dev/ and should never be committed with a real token.
# =============================================================================


# =============================================================================
# 1. CONFIG  —  EDIT THIS BLOCK
# =============================================================================

# --- Token ---------------------------------------------------------------
# Paste your refresh / integration token here, OR leave "" and supply it via
# keyring (MODE A) or the HAWKIN_KEY_<PROFILE> env var (MODE B) ahead of time.
HD_TOKEN <- ""

# --- Connection settings -------------------------------------------------
HD_PROFILE     <- "smoke"        # profile name (keyring username / env-var suffix)
HD_REGION      <- "Americas"     # "Americas", "Europe", or "APAC"
HD_ENVIRONMENT <- "production"   # "development" (keyring) or "production" (env var)
HD_ORG_ID      <- "v1"           # org id used in API paths; "v1" for standard users
HD_LOG_LEVEL   <- "INFO"         # "TRACE","DEBUG","INFO","WARN","ERROR"

# --- Optional: target a non-prod API -------------------------------------
# Set to the dev/staging base URL to hit that environment instead of the
# region default, or leave "" to use the regional production endpoint.
# (Uses the package's internal HAWKINR_API_BASE_URL_OVERRIDE hook.)
HD_BASE_URL_OVERRIDE <- ""       # e.g. "https://cloud.dev.hawkindynamics.com/api"

# --- Date windows for test queries ---------------------------------------
# Kept modest so the smoke test stays fast. Widen if your org is sparse.
HD_FROM_RECENT <- format(Sys.Date() - 30,  "%Y-%m-%d")   # ~last month
HD_FROM_WIDE   <- format(Sys.Date() - 365, "%Y-%m-%d")   # ~last year
HD_TO          <- format(Sys.Date(),       "%Y-%m-%d")   # today

# --- Safety switches -----------------------------------------------------
# Write operations CREATE a real athlete in your org (and then deactivate it).
# Leave FALSE for a read-only smoke test. Set TRUE only when you accept that a
# clearly-labelled STRESSTEST_* athlete will be created in the target org.
RUN_WRITE_TESTS <- FALSE

# If TRUE and we stored a keyring credential during MODE A this run, remove it
# again at the end. Leave FALSE to keep your stored credential.
RESET_CREDENTIALS_ON_EXIT <- FALSE

# Limit how many force-time trials the bulk steps pull, to keep things quick.
HD_BULK_LIMIT <- 5L


# =============================================================================
# 2. TEST HARNESS  (do not edit)
# =============================================================================

# We keep all run state in a private environment so the helpers can append to
# the results table with <<- without leaking globals.
.smoke <- new.env(parent = emptyenv())
.smoke$results <- data.frame(
  step      = character(),
  status    = character(),   # PASS | FAIL | SKIP
  rows      = integer(),     # rows (data.frame) / length (list/vector)
  elapsed_s = numeric(),
  detail    = character(),
  stringsAsFactors = FALSE
)
.smoke$stored_keyring <- FALSE  # did we write a keyring entry this run?

# Best-effort size of a result for at-a-glance sanity checking.
.count <- function(x) {
  if (is.null(x)) return(NA_integer_)
  if (is.data.frame(x)) return(nrow(x))
  if (is.list(x))       return(length(x))
  if (is.atomic(x))     return(length(x))
  NA_integer_
}

.record <- function(step, status, rows = NA_integer_, elapsed = NA_real_, detail = "") {
  .smoke$results[nrow(.smoke$results) + 1L, ] <-
    list(step, status, as.integer(rows), as.numeric(elapsed), detail)
}

# Standard step: ANY error => FAIL. Returns the expression's value (invisibly)
# so later steps can reuse it (e.g. grab an athlete id from the roster).
step <- function(name, expr) {
  cat(sprintf("\n--- %s\n", name))
  t0     <- Sys.time()
  status <- "PASS"
  detail <- ""
  res <- tryCatch(
    force(expr),
    error = function(e) { status <<- "FAIL"; detail <<- conditionMessage(e); NULL }
  )
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3)
  rows    <- if (status == "PASS") .count(res) else NA_integer_
  if (status == "PASS") cat(sprintf("    PASS  (n = %s, %ss)\n", format(rows), elapsed))
  else                  cat(sprintf("    FAIL  %s\n", detail))
  .record(name, status, rows, elapsed, detail)
  invisible(res)
}

# Inverted step: an error is the EXPECTED outcome. Optionally require the error
# message to match `pattern` (regex). Useful for 404s, validation errors, etc.
step_expect_error <- function(name, expr, pattern = NULL) {
  cat(sprintf("\n--- %s  (expect error)\n", name))
  t0     <- Sys.time()
  status <- "FAIL"
  detail <- "no error was raised, but one was expected"
  tryCatch(
    force(expr),
    error = function(e) {
      msg <- conditionMessage(e)
      if (is.null(pattern) || grepl(pattern, msg, perl = TRUE)) {
        status <<- "PASS"; detail <<- paste0("caught: ", msg)
      } else {
        status <<- "FAIL"; detail <<- paste0("wrong error: ", msg)
      }
    }
  )
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3)
  if (status == "PASS") cat(sprintf("    PASS  (%s)\n", detail))
  else                  cat(sprintf("    FAIL  %s\n", detail))
  .record(name, status, NA_integer_, elapsed, detail)
  invisible(NULL)
}

# Explicitly skipped step (records why, so the summary is honest).
step_skip <- function(name, reason) {
  cat(sprintf("\n--- %s\n    SKIP  %s\n", name, reason))
  .record(name, "SKIP", NA_integer_, NA_real_, reason)
  invisible(NULL)
}

# Convenience: was the most recently recorded step a PASS?
.last_passed <- function() {
  n <- nrow(.smoke$results)
  n > 0 && .smoke$results$status[n] == "PASS"
}


# =============================================================================
# 3. PRE-FLIGHT
# =============================================================================

cat("\n=========================================================\n")
cat(  "  hawkinR smoke test\n")
cat(sprintf("  profile=%s  region=%s  env=%s  org=%s\n",
            HD_PROFILE, HD_REGION, HD_ENVIRONMENT, HD_ORG_ID))
cat(sprintf("  write-tests=%s  bulk-limit=%d\n", RUN_WRITE_TESTS, HD_BULK_LIMIT))
cat(  "=========================================================\n")

if (!requireNamespace("hawkinR", quietly = TRUE) && !exists("hd_connect")) {
  stop("hawkinR is not loaded. Run devtools::load_all('.') from packages/hawkinR first.",
       call. = FALSE)
}

# Apply the optional base-URL override before connecting.
if (nzchar(HD_BASE_URL_OVERRIDE)) {
  Sys.setenv(HAWKINR_API_BASE_URL_OVERRIDE = HD_BASE_URL_OVERRIDE)
  cat(sprintf("[setup] API base URL override -> %s\n", HD_BASE_URL_OVERRIDE))
}

# Initialise logging to both console and a timestamped file in dev/ (falls back
# to the working directory if dev/ doesn't exist).
log_dir  <- if (dir.exists("dev")) "dev" else "."
log_path <- file.path(log_dir, sprintf("smoke_test_%s.log",
                                        format(Sys.time(), "%Y%m%d_%H%M%S")))
initialize_logger(
  log_output           = "both",
  log_threshold_stdout = HD_LOG_LEVEL,
  log_file             = log_path,
  log_threshold_file   = "TRACE"   # capture everything to the file for triage
)


# =============================================================================
# 4. AUTH + CONNECT
# =============================================================================

# Resolve the token into whichever storage the chosen environment reads from,
# then open the connection. Token handling differs by mode (see header).
connected <- step("connect: store credential (if needed) + hd_connect", {

  if (nzchar(HD_TOKEN)) {
    if (identical(HD_ENVIRONMENT, "development")) {
      # MODE A: write the token into the OS keychain. Requires interactive R.
      if (!interactive()) {
        stop("MODE A (development) needs an interactive session to store the ",
             "token via hd_auth_store(). Use HD_ENVIRONMENT='production' for ",
             "headless runs, or pre-store the credential.", call. = FALSE)
      }
      hd_auth_store(profile = HD_PROFILE, token = HD_TOKEN)
      .smoke$stored_keyring <- TRUE
    } else {
      # MODE B: expose HAWKIN_KEY_<PROFILE> for this session only.
      env_var <- paste0("HAWKIN_KEY_", toupper(HD_PROFILE))
      do.call(Sys.setenv, stats::setNames(list(HD_TOKEN), env_var))
    }
  }

  # The actual handshake: exchanges the refresh token for an access token and
  # sets the result as the package-level "active connection".
  hd_connect(
    profile     = HD_PROFILE,
    org_id      = HD_ORG_ID,
    environment = HD_ENVIRONMENT,
    region      = HD_REGION,
    log_level   = HD_LOG_LEVEL
  )
})

# Did we connect? If not, every network step downstream would fail identically,
# so we record them as SKIP instead of drowning the summary in repeats.
CONNECTED <- .last_passed()
if (!CONNECTED) {
  cat("\n[!] Could not authenticate — network steps will be skipped.\n")
  cat("    Check the token, region, environment, and base-URL override.\n")
}

# Inspect the live connection object (read-only) to confirm internal state.
if (CONNECTED) {
  step("connect: inspect active connection state", {
    conn <- hawkinR:::get_active_conn()
    stopifnot(!is.null(conn@access_token))
    cat(sprintf("    base_url=%s\n    profile=%s  region=%s\n    token expires=%s\n",
                conn@base_url, conn@config@profile, conn@region,
                format(conn@expires_at)))
    conn
  })
}


# =============================================================================
# 5. OFFLINE METADATA  (no network — should pass even without a connection)
# =============================================================================
# These exercise the static lookup tables (test-type map + MetricDictionary)
# and are a good early sanity check that the package loaded correctly.

step("offline: get_testTypes()", get_testTypes())

step("offline: get_metrics() — all",        get_metrics())
step("offline: get_metrics('CMJ')",          get_metrics(testType = "CMJ"))
step("offline: get_metrics('Isometric Test')", get_metrics(testType = "Isometric Test"))


# =============================================================================
# 6. ORG METADATA  (network)
# =============================================================================

if (CONNECTED) {
  teams      <- step("org: get_teams()",   get_teams())
  groups     <- step("org: get_groups()",  get_groups())
  tags       <- step("org: get_tags()",    get_tags())
  ath_active <- step("org: get_athletes() — active only",            get_athletes())
  ath_all    <- step("org: get_athletes(includeInactive = TRUE)",    get_athletes(includeInactive = TRUE))

  # Sanity relationship: the "all" roster should be >= the active-only roster.
  step("org: includeInactive returns >= active-only", {
    stopifnot(is.data.frame(ath_active), is.data.frame(ath_all))
    if (nrow(ath_all) < nrow(ath_active)) {
      stop(sprintf("all=%d < active=%d (unexpected)", nrow(ath_all), nrow(ath_active)),
           call. = FALSE)
    }
    TRUE
  })
} else {
  teams <- groups <- tags <- ath_active <- ath_all <- NULL
  step_skip("org: metadata endpoints", "no connection")
}


# =============================================================================
# 7. TEST QUERIES  (network)  —  every filter path
# =============================================================================

if (CONNECTED) {

  # --- Date windows -------------------------------------------------------
  tests_recent <- step("tests: from recent window",
                       get_tests(from = HD_FROM_RECENT))
  step("tests: from/to bounded window",
       get_tests(from = HD_FROM_RECENT, to = HD_TO))

  # --- Filter by test type (canonical id, then abbreviation) --------------
  cmj_id <- {
    tt <- tryCatch(get_testTypes(), error = function(e) NULL)
    if (is.data.frame(tt) && "Countermovement Jump" %in% tt$name)
      tt$canonicalId[tt$name == "Countermovement Jump"][1] else NA_character_
  }
  if (!is.na(cmj_id)) {
    step("tests: filter by typeId (canonical CMJ id)",
         get_tests(from = HD_FROM_WIDE, to = HD_TO, typeId = cmj_id))
  }
  step("tests: filter by typeId (abbreviation 'CMJ')",
       get_tests(from = HD_FROM_WIDE, to = HD_TO, typeId = "CMJ"))

  # --- Filter by athlete --------------------------------------------------
  if (is.data.frame(ath_active) && nrow(ath_active) > 0) {
    step("tests: filter by athleteId (single)",
         get_tests(from = HD_FROM_WIDE, to = HD_TO, athleteId = ath_active$id[1]))
  } else {
    step_skip("tests: filter by athleteId", "no athletes in roster")
  }

  # --- Filter by team (single + multi up to 10) ---------------------------
  if (is.data.frame(teams) && nrow(teams) > 0) {
    step("tests: filter by teamId (single)",
         get_tests(from = HD_FROM_WIDE, to = HD_TO, teamId = teams$id[1]))
    if (nrow(teams) >= 2) {
      step("tests: filter by teamId (multi, vector)",
           get_tests(from = HD_FROM_WIDE, to = HD_TO, teamId = head(teams$id, 10L)))
    }
  } else {
    step_skip("tests: filter by teamId", "no teams in org")
  }

  # --- Filter by group ----------------------------------------------------
  if (is.data.frame(groups) && nrow(groups) > 0) {
    step("tests: filter by groupId (single)",
         get_tests(from = HD_FROM_WIDE, to = HD_TO, groupId = groups$id[1]))
  } else {
    step_skip("tests: filter by groupId", "no groups in org")
  }

  # --- Sync mode (pulls by modified/uploaded time, not test time) ---------
  step("tests: sync mode (syncFrom recent)",
       get_tests(from = HD_FROM_WIDE, sync = TRUE))

  # --- includeInactive (server-side toggle, API v1.13+) -------------------
  step("tests: includeInactive = TRUE",
       get_tests(from = HD_FROM_RECENT, includeInactive = TRUE))

  # --- includeEid (equipment id column) -----------------------------------
  step("tests: includeEid = TRUE",
       get_tests(from = HD_FROM_RECENT, includeEid = TRUE))

  # --- Deprecated chunk_size: must NOT error, only log a warning ----------
  step("tests: deprecated chunk_size is ignored (no error)",
       get_tests(from = HD_FROM_RECENT, chunk_size = 30))

} else {
  tests_recent <- NULL
  step_skip("tests: all query scenarios", "no connection")
}


# =============================================================================
# 8. FORCE-TIME  (network)  —  single, bulk, and export
# =============================================================================

if (CONNECTED) {

  # Choose a representative test id. Prefer the recent window; fall back to a
  # wider pull if the recent window came back empty.
  sample_test_id <- NULL
  ids_source <- NULL
  if (is.data.frame(tests_recent) && nrow(tests_recent) > 0 && "id" %in% names(tests_recent)) {
    ids_source     <- tests_recent
    sample_test_id <- tests_recent$id[1]
  } else {
    wide <- tryCatch(get_tests(from = HD_FROM_WIDE, to = HD_TO),
                     error = function(e) NULL)
    if (is.data.frame(wide) && nrow(wide) > 0 && "id" %in% names(wide)) {
      ids_source     <- wide
      sample_test_id <- wide$id[1]
    }
  }

  if (!is.null(sample_test_id)) {

    # --- Single force-time trial (returns a HawkinForceTime S7 object) -----
    step(sprintf("forcetime: single trial (%s)", sample_test_id), {
      ft <- get_forcetime(testId = sample_test_id)
      cat(sprintf("    athlete=%s  type=%s  rate=%dHz  samples=%d\n",
                  ft@athlete_name, ft@testType_name,
                  ft@test_sampling_rate, nrow(ft@data)))
      ft
    })

    # --- Bulk from an explicit vector of ids (in-memory list) -------------
    bulk_ids <- head(ids_source$id, HD_BULK_LIMIT)
    step(sprintf("forcetime: bulk from id vector (n=%d, in-memory)", length(bulk_ids)),
         get_forcetime_bulk(test_ids = bulk_ids))

    # --- Bulk accepting a data frame (v2 feature: get_tests() output) ------
    # get_forcetime_bulk() detects the `id` column and pulls those trials.
    step("forcetime: bulk from data frame (id column auto-detected)",
         get_forcetime_bulk(test_ids = head(ids_source, HD_BULK_LIMIT)))

    # --- Bulk with NO ids: delegates to get_tests(...) for targets --------
    step("forcetime: bulk via get_tests delegation (typeId + from)",
         get_forcetime_bulk(typeId = "CMJ", from = HD_FROM_RECENT))

    # --- Export to CSV (+ auto metadata_manifest.csv) ----------------------
    csv_dir <- file.path(tempdir(), "hawkinR_smoke_csv")
    step(sprintf("forcetime: bulk export -> CSV (%s)", csv_dir), {
      get_forcetime_bulk(
        test_ids    = bulk_ids,
        export      = TRUE,
        export_dir  = csv_dir,
        format      = "csv",
        file_naming = c("athlete_name", "testType_name", "test_id")
      )
      files <- list.files(csv_dir)
      cat(sprintf("    wrote %d file(s): %s\n",
                  length(files), paste(head(files, 4), collapse = ", ")))
      stopifnot(any(grepl("^metadata_manifest", files)))
      files
    })

    # --- Export to RDS -----------------------------------------------------
    rds_dir <- file.path(tempdir(), "hawkinR_smoke_rds")
    step(sprintf("forcetime: bulk export -> RDS (%s)", rds_dir),
         get_forcetime_bulk(
           test_ids    = bulk_ids,
           export      = TRUE,
           export_dir  = rds_dir,
           format      = "rds",
           file_naming = c("test_id")
         ))

    # --- Export with de-identification (PII stripped from name + filename) -
    deid_dir <- file.path(tempdir(), "hawkinR_smoke_deid")
    step(sprintf("forcetime: bulk export -> CSV, de-identified (%s)", deid_dir),
         get_forcetime_bulk(
           test_ids    = bulk_ids,
           export      = TRUE,
           export_dir  = deid_dir,
           format      = "csv",
           deidentify  = TRUE,
           file_naming = c("athlete_name", "test_id")  # name becomes "De-identified"
         ))

    # --- Parquet export (only if arrow is installed) -----------------------
    if (requireNamespace("arrow", quietly = TRUE)) {
      pq_dir <- file.path(tempdir(), "hawkinR_smoke_parquet")
      step(sprintf("forcetime: bulk export -> Parquet (%s)", pq_dir),
           get_forcetime_bulk(
             test_ids    = bulk_ids,
             export      = TRUE,
             export_dir  = pq_dir,
             format      = "parquet",
             file_naming = c("test_id")
           ))
    } else {
      step_skip("forcetime: bulk export -> Parquet", "arrow package not installed")
    }

  } else {
    step_skip("forcetime: all scenarios", "no test trials available to sample")
  }

} else {
  step_skip("forcetime: all scenarios", "no connection")
}


# =============================================================================
# 8b. CENTER OF PRESSURE  (network)  —  Free Run tests only
# =============================================================================
# COP data is exclusive to the Free Run test type. Locate a Free Run trial
# dynamically (skip cleanly if the org has none), then exercise get_cop():
# the HawkinCOP object, the seven-column data frame, and the element-wise
# nullable COP series (NA where no weight is on a plate).

if (CONNECTED) {

  free_run_id <- {
    frt <- tryCatch(
      get_tests(from = HD_FROM_WIDE, to = HD_TO, typeId = "Free Run"),
      error = function(e) NULL
    )
    if (is.data.frame(frt) && nrow(frt) > 0 && "id" %in% names(frt)) {
      frt$id[1]
    } else {
      NA_character_
    }
  }

  if (!is.na(free_run_id)) {

    # --- Single COP trial (returns a HawkinCOP S7 object) -----------------
    step(sprintf("cop: single Free Run trial (%s)", free_run_id), {
      cop <- get_cop(testId = free_run_id)
      cat(sprintf("    athlete=%s  type=%s  rate=%sHz  samples=%d  eid=%s\n",
                  cop@athlete_name, cop@testType_name,
                  format(cop@test_sampling_rate), nrow(cop@data),
                  if (is.null(cop@eid)) "NA" else cop@eid))
      cat(sprintf("    columns: %s\n", paste(names(cop@data), collapse = ", ")))
      cop
    })

    # --- Data frame shape: 7 COP series present, time axis never NA -------
    step("cop: data frame shape + nullable COP series", {
      cop      <- get_cop(testId = free_run_id)
      expected <- c("time_s", "cop_x", "cop_y", "left_cop_x",
                    "left_cop_y", "right_cop_x", "right_cop_y")
      missing  <- setdiff(expected, names(cop@data))
      if (length(missing) > 0) {
        stop(sprintf("missing COP columns: %s", paste(missing, collapse = ", ")),
             call. = FALSE)
      }
      if (nrow(cop@data) == 0)    stop("COP data frame is empty", call. = FALSE)
      if (anyNA(cop@data$time_s)) stop("time_s should never contain NA", call. = FALSE)
      cop@data
    })

  } else {
    step_skip("cop: single Free Run trial", "no Free Run tests in org")
    step_skip("cop: data frame shape + nullable COP series", "no Free Run tests in org")
  }

} else {
  step_skip("cop: all scenarios", "no connection")
}


# =============================================================================
# 9. WRITE OPERATIONS  (network, OPT-IN)  —  create + update athletes
# =============================================================================
# These mutate your org. Guarded by RUN_WRITE_TESTS. A uniquely-named
# STRESSTEST_* athlete is created, then immediately deactivated. There is no
# delete endpoint in the package, so cleanup = deactivation.

if (CONNECTED && isTRUE(RUN_WRITE_TESTS)) {

  smoke_name <- sprintf("SMOKETEST_%s", format(Sys.time(), "%Y%m%d_%H%M%S"))
  created_id <- NULL

  step(sprintf("write: create_athletes (%s)", smoke_name), {
    payload <- data.frame(name = smoke_name, active = TRUE, stringsAsFactors = FALSE)
    res <- create_athletes(athleteData = payload)
    # create_athletes() returns invisible(TRUE) on full success; re-fetch to
    # recover the new athlete's id for the update step.
    roster <- get_athletes(includeInactive = TRUE)
    hit <- roster[roster$name == smoke_name, , drop = FALSE]
    if (nrow(hit) > 0) created_id <<- hit$id[1]
    res
  })

  if (!is.null(created_id)) {
    step(sprintf("write: update_athletes (%s -> active = FALSE)", smoke_name),
         update_athletes(athleteData = data.frame(
           id     = created_id,
           name   = smoke_name,
           active = FALSE,
           stringsAsFactors = FALSE
         )))
  } else {
    step_skip("write: update_athletes", "could not resolve created athlete id")
  }

} else {
  step_skip("write: create/update athletes",
            if (!CONNECTED) "no connection" else "RUN_WRITE_TESTS is FALSE")
}


# =============================================================================
# 10. EXPECTED-ERROR PATHS  (network + offline)
# =============================================================================
# These should FAIL the API/validation call — and that failure is the PASS.

# Offline: an unknown test type must be rejected by get_metrics().
step_expect_error("error: get_metrics() rejects unknown testType",
                  get_metrics(testType = "NotARealTestType"),
                  pattern = "Invalid testType")

if (CONNECTED) {
  # A bogus trial id should surface as a 404 / not-found.
  step_expect_error("error: get_forcetime() with bogus id",
                    get_forcetime(testId = "does-not-exist-000000000000000000000000"),
                    pattern = "404|Not Found|No test data")

  # A bogus COP id should surface as a 404 / not-found.
  step_expect_error("error: get_cop() with bogus id",
                    get_cop(testId = "does-not-exist-000000000000000000000000"),
                    pattern = "404|Not Found|Resource Not Found|No COP")

  # COP is Free Run only: requesting it for a non-Free-Run test must 404.
  non_free_run_id <- {
    cmj <- tryCatch(get_tests(from = HD_FROM_WIDE, to = HD_TO, typeId = "CMJ"),
                    error = function(e) NULL)
    if (is.data.frame(cmj) && nrow(cmj) > 0 && "id" %in% names(cmj))
      cmj$id[1] else NA_character_
  }
  if (!is.na(non_free_run_id)) {
    step_expect_error("error: get_cop() on a non-Free-Run test (CMJ)",
                      get_cop(testId = non_free_run_id),
                      pattern = "404|Not Found|Resource Not Found|exclusive to Free Run")
  } else {
    step_skip("error: get_cop() on a non-Free-Run test", "no CMJ tests in org")
  }

  # get_tests() without a `from` is only a clean error in NON-interactive mode;
  # interactively it prompts via readline(), which would hang an automated run.
  if (!interactive()) {
    step_expect_error("error: get_tests() requires from (non-interactive)",
                      get_tests(),
                      pattern = "requires a 'from' date")
  } else {
    step_skip("error: get_tests() requires from",
              "interactive session would prompt via readline() — skipped to avoid hanging")
  }
} else {
  step_skip("error: network error paths", "no connection")
}


# =============================================================================
# 11. TOKEN REFRESH  (network)
# =============================================================================
# Force the access token to look expired, then make a call. The package should
# transparently re-authenticate (token_remaining < 300s threshold) before the
# request, and the call should succeed.

if (CONNECTED) {
  step("auth: forced token refresh on next call", {
    conn <- hawkinR:::get_active_conn()
    conn@expires_at <- Sys.time() - 1L          # pretend it just expired
    hawkinR:::set_active_conn(conn)
    get_teams()                                  # should trigger a refresh first
  })
} else {
  step_skip("auth: forced token refresh", "no connection")
}


# =============================================================================
# 12. CLEANUP
# =============================================================================
# Optionally remove the keyring credential we created this run (MODE A only).

if (isTRUE(RESET_CREDENTIALS_ON_EXIT) && isTRUE(.smoke$stored_keyring)) {
  step(sprintf("cleanup: hd_auth_reset('%s')", HD_PROFILE),
       { hd_auth_reset(profile = HD_PROFILE); TRUE })
} else {
  step_skip("cleanup: hd_auth_reset",
            if (!.smoke$stored_keyring) "no keyring credential stored this run"
            else "RESET_CREDENTIALS_ON_EXIT is FALSE")
}


# =============================================================================
# 13. SUMMARY
# =============================================================================

res <- .smoke$results
n_pass <- sum(res$status == "PASS")
n_fail <- sum(res$status == "FAIL")
n_skip <- sum(res$status == "SKIP")

cat("\n\n================== SMOKE TEST SUMMARY ==================\n")
print(res, row.names = FALSE)
cat("-------------------------------------------------------\n")
cat(sprintf("PASS: %d   FAIL: %d   SKIP: %d   (of %d steps)\n",
            n_pass, n_fail, n_skip, nrow(res)))
cat(sprintf("Total time: %.1fs\n", sum(res$elapsed_s, na.rm = TRUE)))
if (n_fail > 0) {
  cat("\nFailures:\n")
  fails <- res[res$status == "FAIL", c("step", "detail")]
  for (i in seq_len(nrow(fails))) {
    cat(sprintf("  - %s\n      %s\n", fails$step[i], fails$detail[i]))
  }
}
cat("=======================================================\n")

# Persist the results table next to the trace log.
results_csv <- sub("\\.log$", "_results.csv", log_path)
tryCatch({
  utils::write.csv(res, results_csv, row.names = FALSE)
  cat(sprintf("\nResults written to: %s\n", results_csv))
  cat(sprintf("Full trace log:     %s\n", log_path))
}, error = function(e) {
  cat(sprintf("\nCould not write results CSV: %s\n", conditionMessage(e)))
})

invisible(.smoke$results)