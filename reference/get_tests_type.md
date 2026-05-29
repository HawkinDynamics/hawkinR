# Get Test Trials By Test Type

**Deprecated**: Use `get_tests` instead, which has been expanded to
handle all requests.

Get only tests of the specified type for an account.

## Usage

``` r
get_tests_type(typeId, from = NULL, to = NULL, sync = FALSE, includeInactive = FALSE)
```

## Arguments

- typeId:

  Supply a value of type string. Must be canonical test Id, test type
  name, or test name abbreviation.

  Names \| Abbreviations: "Countermovement Jump" \| "CMJ", "Squat Jump"
  \| "SJ", "Isometric Test" \| "ISO", "Drop Jump" \| "DJ", "Free Run" \|
  "FREE", "CMJ Rebound" \| "CMJR", "Multi Rebound" \| "MR", "Weigh In"
  \| "WI", "Drop Landing" \| "DL", "TS Free Run" \| "TSFR", "TS
  Isometric Test" \| "TSISO"

- from:

  Optionally supply a time frame **start** value. Accepts either:

  - A Unix timestamp as an `integer` (e.g., `1689958617`), or

  - A date as a `character` string in `"YYYY-MM-DD"` format (e.g.,
    `"2023-08-01"`).

  If not supplied, all available tests from the earliest record will be
  returned. Use this parameter for bulk exports or to define a starting
  point for data retrieval.

- to:

  Optionally supply a time frame **end** value. Accepts either:

  - A Unix timestamp as an `integer` (e.g., `1691207356`), or

  - A date as a `character` string in `"YYYY-MM-DD"` format (e.g.,
    `"2023-08-10"`).

  If not supplied, all available tests up to the latest record will be
  returned, or up to the `from` parameter if specified. Use this
  parameter to limit the range of historical data retrieved.

- sync:

  The result set will include updated and newly created tests. This
  parameter is best suited to keep your database in sync with the Hawkin
  database. If you do not supply this value you will receive every test.

- includeInactive:

  There was a change to the default API configuration to reflect the
  majority of users API configuration. Inactive tests or tests where
  `active = false` are returned in these configuration. Be default,
  `includeInactive` is set to `FALSE`. To return all tests, including
  disabled trials, set `includeInactive` to `TRUE`.

## Value

Response will be a data frame containing the trials within the time
range (if specified).

|  |  |  |
|----|----|----|
| **Column Name** | **Type** | **Description** |
| **id** | *str* | Test trial unique ID |
| **active** | *logi* | The trial is active and not disabled |
| **timestamp** | *int* | UNIX time stamp of trial |
| **segment** | *chr* | Description of the test type and trial number of the session (testType:trialNo) |
| **test_type_id** | *chr* | Id of the test type of the trial |
| **test_type_name** | *chr* | Name of the test type of the trial |
| **test_type_canonicalId** | *chr* | Canonical Id of the test type of the trial |
| **test_type_tag_ids** | *chr* | String of Ids associated with tags used during the test trial |
| **test_type_tag_names** | *chr* | String of names of tags used during the test trial |
| **test_type_tag_desc** | *chr* | String of descriptions of tags used during the test trial |
| **athlete_id** | *chr* | Unique Id of the athlete |
| **athlete_name** | *chr* | Athlete given name |
| **athlete_active** | *logi* | The athlete is active |
| **athlete_teams** | *list* | List containing Ids of each team the athlete is on |
| **athlete_groups** | *list* | List containing Ids of each group the athlete is in |

Only metrics of the given test type are included as the remaining
variables.

## See also

`get_tests`

## Examples

``` r
if (FALSE) { # \dontrun{
# This is an example of how the function would be called.


## Call for all CMJ tests
df_cmj <- get_tests_type(typeId = "7nNduHeM5zETPjHxvm7s")


## Call for Free Run tests within a  specific time frame
df_free <- get_tests_type(typeId = "5pRSUQVSJVnxijpPMck3", from = 1689958617, to = 1691207356)


## Call for Squat Jump tests since a specific date
df_sjSince <- get_tests_type("QEG7m7DhYsD6BrcQ8pic", from = 1689958617)

} # }
```
