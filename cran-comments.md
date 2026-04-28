# CRAN submission — hawkinR 2.0.0

## Release summary

This is the first CRAN submission of hawkinR. The package provides a
secure, configurable R interface to the Hawkin Dynamics force-platform
REST API for retrieving test results, athlete records, and
organization metadata.

Version 2.0.0 is a complete rewrite of an internal 1.x codebase:

- Profile-based authentication with OS keychain (via `keyring`) for
  local development and environment-variable credentials for
  production deployment.
- S7 classes (`HawkinConfig`, `HawkinAuth`) for configuration and
  connection state.
- Cursor-based pagination for large test queries, with automatic
  access-token refresh.
- Region-aware routing (Americas / Europe / APAC).
- Structured logging via the `logger` package.
- Full roxygen2 documentation, five vignettes, and a testthat suite
  (mocked — no network calls during CRAN checks).

## Test environments

- Local: Windows 11 Pro (x86_64, build 26200), R 4.4.3 (2025-02-28 ucrt)
- GitHub Actions (`r-lib/actions/check-r-package`) on:
  - ubuntu-latest, R-release
  - ubuntu-latest, R-devel
  - ubuntu-latest, R-oldrel-1
  - macos-latest, R-release
  - windows-latest, R-release

## R CMD check results

0 errors | 0 warnings | 0 notes

On the local Windows build one additional note appears:
`* checking for future file timestamps ... NOTE — unable to verify current time`.
This is a local clock-skew diagnostic on the developer machine and does
not appear on CRAN's build servers or in any of the GitHub Actions
matrix jobs.

Expected note on first submission: "New submission".

## Downstream dependencies

None — this is a first submission and no reverse dependencies exist.

## Notes for CRAN reviewers

- **No network calls during R CMD check.** All tests that would
  contact the Hawkin API are mocked via the `mockery` package.
  Credential-dependent tests are wrapped in `skip_on_cran()` and
  `skip_if_not()` guards.
- **All examples use `\dontrun{}`** because they require a live
  refresh token issued by a Hawkin Dynamics customer account, which
  cannot be bundled with the package.
- **Vignettes use `eval = FALSE`** for the same reason — no API calls
  are made during vignette rendering.
- **Interactive prompts** (`keyring::key_set()` inside
  `hd_auth_store()`, `readline()` inside `get_tests()`) are guarded
  by `interactive()` checks and documented as requiring an interactive
  R session; they will not block non-interactive CRAN checks.
- **Package-level environment** (`.hawkin_env` created in
  `.onLoad()`) holds the active authenticated connection across
  function calls. No user state is persisted outside the R session.
- **`initialize_logger()` is user-invoked and opt-in.** The package
  does not write any log file on load, attach, or by default. The
  function's default is `log_output = "stdout"` (console only); a
  log file is created only when the user explicitly calls
  `initialize_logger(log_output = "file")` or `"both"`. The file
  appender and its file handle are not constructed unless one of
  those modes is selected.
- **Startup banner uses `.onAttach()` + `packageStartupMessage()`.**
  Users can suppress it with `suppressPackageStartupMessages(library(hawkinR))`.
  `.onLoad()` only sets up the internal connection environment
  and configures the logger; it prints nothing.
