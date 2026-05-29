# Get Teams

Get the team names and ids for all the teams in the org.

## Usage

``` r
get_teams()
```

## Value

Response will be a data frame containing the teams that are in the
organization. Each team has the following variables:

|                 |          |                   |
|-----------------|----------|-------------------|
| **Column Name** | **Type** | **Description**   |
| **id**          | *chr*    | team's unique ID  |
| **name**        | *chr*    | team's given name |

## Examples

``` r
if (FALSE) { # \dontrun{
# This is an example of how the function would be called.

df_teams <- get_teams()

} # }
```
