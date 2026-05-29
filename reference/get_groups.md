# Get Groups

Get the group names and ids for all the groups in the org.

## Usage

``` r
get_groups()
```

## Value

Response will be a data frame containing the groups that are in the
organization. Each group has the following variables:

|                 |          |                    |
|-----------------|----------|--------------------|
| **Column Name** | **Type** | **Description**    |
| **id**          | *chr*    | group's unique ID  |
| **name**        | *chr*    | group's given name |

## Examples

``` r
if (FALSE) { # \dontrun{
# This is an example of how the function would be called.

df_groups <- get_groups()

} # }
```
