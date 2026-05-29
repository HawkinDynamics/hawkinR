# Get All Tests or Sync Tests

Get the tests for an account. You can specify a time frame from, or to,
which the tests should come (or be synced).

## Usage

``` r
get_tests(
 from = NULL,
 to = NULL,
 sync = FALSE,
 athleteId = NULL,
 typeId = NULL,
 teamId = NULL,
 groupId = NULL,
 includeInactive = FALSE
 )
```

## Arguments

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

- athleteId:

  Supply an athlete’s id to receive tests for a specific athlete

- typeId:

  Supply a value of type string. Must be canonical test Id, test type
  name, or test name abbreviation.

  Names \| Abbreviations: "Countermovement Jump" \| "CMJ", "Squat Jump"
  \| "SJ", "Isometric Test" \| "ISO", "Drop Jump" \| "DJ", "Free Run" \|
  "FREE", "CMJ Rebound" \| "CMJR", "Multi Rebound" \| "MR", "Weigh In"
  \| "WI", "Drop Landing" \| "DL", "TS Free Run" \| "TSFR", "TS
  Isometric Test" \| "TSISO"

- teamId:

  Supply a team’s ID, a list of team IDs, or a string of a comma
  separated team id’s to receive tests from the specified teams. A
  maximum of 10 teams can be fetched at once.

- groupId:

  Supply a group’s ID, a list of group IDs, or a string of a comma
  separated group id’s to receive tests from the specified groups. A
  maximum of 10 groups can be fetched at once.

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
| **segment** | *chr* | Description of the test type and trial |
| number of the session (testType:trialNo) |  |  |
| **test_type_id** | *chr* | Id of the test type of the trial |
| **test_type_name** | *chr* | Name of the test type of the trial |
| **test_type_canonicalId** | *chr* | Canonical Id of the test type of the trial |
| **test_type_tag_ids** | *chr* | String of Ids associated with tags used during |
| the test trial |  |  |
| **test_type_tag_names** | *chr* | String of names of tags used during the |
| test trial |  |  |
| **test_type_tag_desc** | *chr* | String of descriptions of tags used during |
| the test trial |  |  |
| **athlete_id** | *chr* | Unique Id of the athlete |
| **athlete_name** | *chr* | Athlete given name |
| **athlete_active** | *logi* | The athlete is active |
| **athlete_teams** | *list* | List containing Ids of each team the athlete is on |
| **athlete_groups** | *list* | List containing Ids of each group the athlete is in |
| **athlete_image** | *chr* | URL to athlete's profile image |
| **athlete_position** | *chr* | Athlete's position |
| **athlete_dob** | *chr* | Athlete's date of birth |
| **athlete_sport** | *chr* | Athlete's sport |
| **athlete_height** | *chr* | Athlete's height |

All metrics from each test type are included as the remaining variables
unless `typeId` is provided, then only the metrics of that test type
will be returned. If a trial does not have data for a variable it is
returned NA.

## Examples
