# Change Log

## hawkinR v2.0.1

### Bug Fixes
* **CRITICAL:** `get_forcetime()` built its data frame by indexing the API response positionally rather than by field name. When the response elements did not line up with the assumed positions, the force-time series were populated from the wrong fields, leaving column values misaligned relative to their labels (for example, a force series appearing under the `time_s` column). Because the shape and length of the data were unaffected, the result looked valid and no error was raised, so downstream analysis could be silently incorrect. Columns are now selected by their API field names (`Time(s)`, `LeftForce(N)`, `RightForce(N)`, ...), so each series is populated from the correct vector regardless of field order or omitted optional fields. The tri-axial force and moment columns are handled the same way.
* `get_forcetime()` now reads `testType_id` from the named `testType$id` field instead of a positional lookup.
* Added regression tests covering the force-time column mapping (each column populated from its own field, tri-axial fields present and absent, and `testType_id`), which previously had no test coverage.

## hawkinR v2.0.0

### Breaking Changes
* `get_tests()` now uses cursor-based API pagination instead of time-window chunking. The `chunk_size` argument is deprecated and ignored.
* `includeInactive` filtering in `get_tests()` is now server-side (API v1.13). Behavior is unchanged but the parameter is sent to the API directly rather than filtering client-side.
* `HawkinForceTime` property renamed: `testType_canoncical` -> `testType_canonical` (typo fix).

### New Features
* `get_cop()` — retrieve raw Center of Pressure (COP) time-series data for a test, returning a new `HawkinCOP` object with the six COP series (`cop_x`, `cop_y`, `left_cop_x`, `left_cop_y`, `right_cop_x`, `right_cop_y`). Exclusive to the Free Run test type (other types return a 404). COP values are kept as `NA` for samples with no weight on a given plate.

### Bug Fixes
* **CRITICAL:** `update_athletes()` now correctly calls `UpdateAthleteJSON()` instead of `AddAthleteJSON()`. Previously, update payloads were missing the required `id` field.
* `create_athletes()` and `update_athletes()` now return the failure data frame when operations partially fail, as documented.
* Fixed `stop(logger::log_error(...))` antipattern in `create_athletes()`, `update_athletes()`, and `UpdateAthleteJSON()`.
* Fixed malformed `@examples` syntax in `create_athletes()` and `update_athletes()` documentation.
* Removed duplicate token refresh block in `get_forcetime()`.
* `get_forcetime()` no longer returns `NULL` for tests whose API response omits an `rsi` value. A `return(NULL)` inside a `tryCatch({})` block was escaping the whole function instead of the block, so any non-RSI test (and every `get_forcetime_bulk()` item) silently came back empty.
* Removed dead code in `TestTypePrep()`.
* `AthletePrep()` now selects athlete columns by name and unnests external (custom) properties robustly, handling both uniform sub-data-frames and list-columns whose keys vary across athletes. Athletes missing a given key receive `NA` instead of triggering an error in `get_athletes()` / `get_tests()`.
* Token expiry checks are now `NA`-safe. A missing or non-`POSIXct` expiration returns a clear prompt to run `hd_connect()` rather than `missing value where TRUE/FALSE needed`, via the internal `token_seconds_remaining()` helper used across all data functions.

### Enhancements
* `get_athletes()` now returns the API v1.14 athlete profile fields (`image`, `position`, `dob`, `sport`, `height`, `lastTestedOn`) when present in the response.
* `get_tests()` now returns the athlete profile fields (`athlete_image`, `athlete_position`, `athlete_dob`, `athlete_sport`, `athlete_height`, `athlete_lastTestedOn`) when present.
* `get_forcetime_bulk()` now accepts a data frame with an `id` column (e.g., output of `get_tests()`) in addition to a character vector of test IDs.
* `get_forcetime_bulk()` now supports `.rda` export format.
* Standardized token refresh threshold to 300 seconds across all functions.

### Documentation
* Updated `hd_auth_store()` with security note about `.Rhistory` risk when passing tokens as arguments.
* Updated `update_athletes()` `@return` documentation to say "updated" instead of "created".
* CRAN compliance fixes: DESCRIPTION title, import spacing, roxygen completeness.

## hawkinR v1.1.5

* fix to R/utils/ParamValidation duplicated date format check.

## hawkinR v1.1.4

* Addition of `org_name` to `get_access()` parameters.

* Improved functionality to `get_tests()` to accept character strings in the format "YYYY-MM-DD" for `from` and `to`

* Addition of 'Metric Library' returned from `get_metrics()` 

* Changes to `get_tests()` metric headers. Improved consistency of naming reflective of the metric library

* Improved functionality for `get_forcetime()` to return all test types

## hawkinR v1.1.3

* Bug fix: removes errors thrown for tests returned without tags

## hawkinR v1.1.2

* Bug fix: addition of TruStrength test types to typeId validation methods

## hawkinR v1.1.1

* fixes (or removes) log suppression:  surface more useful logging to end-users by removing log suppression present in v1.1.0 and earlier

## hawkinR v1.1.0

* Update of `get_tests()` functionality.

* Depreciation of `get_tests_...` functions.

* 'teamId' and 'groupId' parameters now accept lists and atomic vectors to go along with single text strings

* Addition of logs functionality and customization

* Updated documentation

## hawkinR v1.0.5.02

* fix to `DateTimeParam` utility function

* updated test files

## hawkinR v1.0.5.01

* Bug fix to get_tests_teams / groups `to` parameter

## hawkinR v1.0.5.1

* Bug fixes to `get_tests...`. Corrected pad_and_condense function in utils.R

## hawkinR v1.0.5

* Bug fixes to `get_tests_type`. Now handles test trials with and without tags specified

* Changes to `get_tests_type`. 'typeId' parameter now accepts strings containing either canonical test Id, test type name, or test type abbreviation.  See docs for abbreviations.

* Addition of `get_tags` function. Create data frame of tags within your system, including: id, name, description.

* Addition of utils.R file with utility functions to minimize code and increase readability.

## hawkinR v1.0.4

* changes to API endpoint (inclusion of test tag details). All `get_tests_...` function outputs now
include tag Ids, names, and description columns with the prefix 'test_type_tag_'. These can be
found after 'test_type_canonicalId' column.

## hawkinR v1.0.3.2

* bug fixes to all `get_tests_...` functions to handle NULL externalIds

## hawkinR v1.0.3.1

* bug fixes to get_atheletes function to handle NULL externalIds

## hawkinR v1.0.3

* Updates to all `get_tests_...` functions to include filter parameter for 'active' tests. Ability 
to include or exclude disabled trials.

* Updates to all `get_tests_...` functions to include 'sync' parameter. Ability 
to utilize 'syncFrom' and 'syncTo' in all queries. This allows for more efficient queries and 
faster download times when used frequently or to maintain an external database.

* Bug fixes to externalId field

## hawkinR v1.0.2

* updated `get_athlete output` to include externalId

## hawkinR v1.0.1

* bug fixes to `get_tests_team` and `get_tests_group`

* Changes to column name outputs for `get_tests...` calls.

* bug fixes to `get_forcetime`

## hawkinR 1.0.0

* Initial CRAN submission.
