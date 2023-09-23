---
output: html_document
knitr:
  eval: FALSE
---

# hawkinR

<!-- badges: start -->

<!-- badges: end -->

hawkinR provides simple functionality with Hawkin Dynamics API. These functions are for use with 'Hawkin Dynamics Beta API' version 1.8-beta. You must be an Hawkin Dynamics user with active integration account to utilize functions within the package.

This API is designed to get data out of your Hawkin Dynamics database into your own database. It is not designed to be accessed from client applications directly. There is a limit on the amount of data that can be returned in a single request. As your database grows, it will be necessary to use the from and to parameters to limit the size of the responses. Responses that exceed the memory limit will fail. It is advised that you design your client to handle this from the beginning. A recommended pattern would be to have two methods of fetching data. A scheduled pull that uses the from and to parameters to constrain the returned data to only tests that have occurred since the last fetch e.g. every day or every 5 minutes. And then a pull that fetches the entire database since you began testing that is only executed when necessary. A recommended way of doing this is to generate the from and to parameters for each month since you started and send a request for each either in parallel or sequentially.

## Installation

You can install the development version of hawkinR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("HawkinDynamics/hawkinR")
```

## Functions

This package was meant to help execute requests to the Hawkin Dynamics API with a single line of code. There are 11 functions to help execute 4 primary objectives:

#### Get Access

-   `get_access()` - Use the Refresh Token generated <https://cloud.hawkindynamics.com/integrations> to get a valid Access Token. Only the organization administrator account has the ability to generate API tokens. Use this function to initiate access to your data in the cloud. All other hawkinR functions will depend on the values returned from this function.

#### Get Test Types

-   `get_testTypes()` - Get the test type names and ids for all the test types in the system. Response will be a data frame containing the tests that are in the HD system.

#### Get Organization Data

-   `get_athletes()` - Get the athletes for an account. Inactive players will only be included if \`inactive\` parameter is set to TRUE. Response will be a data frame containing the athletes that match this query.

-   `get_teams()` - Get the team names and ids for all the teams in the org. Response will be a data frame containing the teams that are in the organization.

-   `get_groups()` - Get the group names and ids for all the groups in the org. Response will be a data frame containing the groups that are in the organization.

#### Get Test Data

-   `get_forcetime()` - Get the force-time data for a specific test by id. This includes both left, right and combined force data at 1000hz (per millisecond). Calculated velocity, displacement, and power at each time interval will also be included.

-   `get_tests()` - Get the tests for an account. You can specify a time frame from, or to, which the tests should come (or be synced). Response will be a data frame containing the trials within the time range (if specified).

-   `get_tests_type()` - Get only tests of the specified type for an account. Response will be a data frame containing the trials of the specified type and within the time range (if specified).

-   `get_tests_ath()` - Get only tests of the specified athlete for an account. Response will be a data frame containing the trials from the specified team and within the time range (if specified).

-   `get_tests_team()` - Get only tests of the specified team for an account. Response will be a data frame containing the trials from the specified team and within the time range (if specified).

-   `get_tests_group()` - Get only tests of the specified group for an account. Response will be a data frame containing the trials from the specified team and within the time range (if specified).

## Examples

This is a basic example which shows you how to solve a common problem:

``` r

library(hawkinR) 
## basic example code

# Get access token. When successful, access token is stored for use in the session.
get_access("refreshToken", region = "Americas")

# Sync tests since a time point.
df_SyncFrom <- get_tests(from = 1689958617, sync = TRUE)

# Get tests by athlete in time frame.
dfFromTo <- get_tests_ath(athleteId = "athleteId", from = 1689958617, to = 1691207356)
```
