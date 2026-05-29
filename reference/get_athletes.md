# Get Athletes

Get the athletes for an account. Inactive players will only be included
if `includeInactive` parameter is set to TRUE.

## Usage

``` r
get_athletes(includeInactive = FALSE)
```

## Arguments

- includeInactive:

  FALSE by default to exclude inactive players in database. Set to TRUE
  if you want inactive players included in the return.

## Value

Response will be a data frame containing the athletes that match this
query. Each athlete includes the following variables:

|  |  |  |
|----|----|----|
| **Column Name** | **Type** | **Description** |
| **id** | *chr* | athlete's unique ID |
| **name** | *chr* | athlete's given name (First Last) |
| **active** | *bool* | athlete is active (TRUE) |
| **teams** | *chr* | team ids separated by "," |
| **groups** | *chr* | group ids separated by "," |
| **image** | *chr* | URL to athlete's profile image |
| **position** | *chr* | athlete's position |
| **dob** | *chr* | athlete's date of birth |
| **sport** | *chr* | athlete's sport |
| **height** | *chr* | athlete's height |
| **lastTestedOn** | *chr* | date of athlete's last test |
| **external** | *chr* | external properties will have a column of their name with the appropriate values for the athlete of `NA` if it does not apply |

## Examples

``` r
if (FALSE) { # \dontrun{
# This is an example of how the function would be called. If you only wish to call active players,
# you don't need to provide any parameters.

df_athletes <- get_athletes()

# If you want to include all athletes, including inactive athletes, include the optional
# `includeInactive` parameter.

df_wInactive <- get_athletes( includeInactive = TRUE)

} # }
```
