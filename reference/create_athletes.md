# Create Athletes

Create a new athlete or athletes for an account. Bulk create up to 500
athletes at a time.

## Usage

``` r
create_athletes(athleteData)
```

## Arguments

- athleteData:

  A data frame of the athletes to be created. The data frame must follow
  the schema:

## Value

If successful, a confirmation message will be printed with the number of
successful athletes created. If there are failures, a data frame
containing the athletes that failed to be created will be returned with
columns:

|                 |          |                                   |
|-----------------|----------|-----------------------------------|
| **Column Name** | **Type** | **Description**                   |
| **reason**      | *chr*    | Reason for failed creation        |
| **name**        | *chr*    | Athlete's given name (First Last) |

## Details

The data frame passed as the argument must use the following schema:

|  |  |  |  |
|----|----|----|----|
| **Column Name** | **Type** | **Inclusion** | **Description** |
| **name** | *chr* | **REQUIRED** | athlete's given name (First Last) |
| **image** | *chr* | *optional* | URL path to image. `default = null` |
| **active** | *logi* | *optional* | athlete is active (TRUE). `default = null` |
| **teams** | *list* | *optional* | a single team id as a string or list of team ids. `default = [defaultTeamId]` |
| **groups** | *list* | *optional* | a single group id as a string or list of group ids. `default = []` |
| **external property** | *chr* | *optional* | External properties can be added by adding any additional columns of equal length. The name of the column will become the external property name, and the row value will become the external property value. Use "lowercase" or "snake_case" styles for column names. |

## Examples
