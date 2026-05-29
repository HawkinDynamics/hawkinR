# Get Force-Time Data

Get the force-time data for a specific test by id. This includes both
left, right and combined force data at 1000hz (per millisecond).
Calculated velocity, displacement, and power at each time interval will
also be included.

## Usage

``` r
get_forcetime(testId)
```

## Arguments

- testId:

  Give the unique test id of the trial you want to be called.

## Value

Response will be a data frame containing the following:

|  |  |  |
|----|----|----|
| **Column Name** | **Type** | **Description** |
| **time_s** | *int* | Elapsed time in seconds, starting from end of identified quiet phase |
| **force_right** | *int* | Force recorded from the RIGHT platform coinciding with time point from `time_s`, measured in Newtons (N) |
| **force_Left** | *int* | Force recorded from the LEFT platform coinciding with time point from `time_s`, measured in Newtons (N) |
| **force_combined** | *int* | Sum of forces from LEFT and RIGHT, coinciding with time point from `time_s`, measured in Newtons (N) |
| **velocity_m.s** | *int* | Calculated velocity of center of mass at time interval, measured in meters per second (m/s) |
| **displacement_m** | *int* | Calculated displacement of center of mass at time interval, measured in meters (m) |
| **power_w** | *int* | Calculated power of mass at time interval, measured in watts (W) |

## Examples

``` r
if (FALSE) { # \dontrun{
# This is an example of how the function would be called.

df_ft <- get_forcetime(testId = `stringId`)

} # }
```
