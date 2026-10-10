# CRAN submission — hawkinR 2.2.0

## Release summary

This is a minor release. The current CRAN version is 2.0.1 (published
2026-07-21). Version 2.1.0 was tagged in our repository but never submitted
to CRAN, so this submission contains the changes from both 2.1.0 and 2.2.0.
See NEWS.md for details.

Two of the changes are data-correctness fixes:

- `get_tests()` could silently return incomplete data. When any page after
  the first failed (a non-200 status, a network error, or a pagination
  cursor that did not advance), it logged a warning and returned only the
  pages fetched so far, which looked like a complete result. It now stops
  with an error naming the status and page. Transient failures (HTTP 429,
  502, 503, 504) are retried with backoff first.
- `update_athletes()` always sent an `external` field, empty when the data
  frame had no custom-property columns. The API replaces external properties
  on update, so an update that only changed, for example, `name` could delete
  every custom property on those athletes. `external` is now sent only when
  there are custom values. Multiple team or group IDs were also sent nested
  one level too deep; they are now sent as a flat array.

New features:

- `get_tests()` gains `useNulls`, `rounding` and `nestMetrics`, which map to
  response-shape query parameters of the Hawkin API.
- `create_athletes()` and `update_athletes()` write the athlete profile
  fields `position`, `sport`, `dob` and `height` as native API fields.

Regression tests cover each fix.

## Test environments

- Local: Windows 11 Enterprise (x86_64), R 4.4.3 (2025-02-28 ucrt)
- mac builder (macOS 26.6, Apple M1, R 4.6.1, arm64): Status OK, no errors, warnings or notes; PDF manual OK
- win-builder: R-devel (2026-09-30 r90605 ucrt) and R-release (4.6.1), x86_64-w64-mingw32

## R CMD check results

Local `R CMD check --as-cran`: 0 errors | 0 warnings | 1 note

```
* checking for future file timestamps ... NOTE
  unable to verify current time
```

This note comes from the local environment's time-server lookup, not from
the package.

win-builder R-devel and R-release: Status OK (0 errors | 0 warnings | 0 notes),
including the CRAN incoming feasibility check.

mac builder (R 4.6.1, arm64): Status OK (0 errors | 0 warnings | 0 notes).

## Downstream dependencies

None. There are no reverse dependencies on CRAN.

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
- **Package-level environment** (`.hawkin_env`, defined at the top
  level of `R/auth_system.R` and initialized in `.onLoad()`) holds the
  active authenticated connection across function calls. It is created
  with `new.env(parent = emptyenv())` and its contents are mutated by
  reference; the package never assigns into the global environment.
  No user state is persisted outside the R session.
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
