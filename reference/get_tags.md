# Get Tags

Get the tag names and ids for all the tags in the system.

## Usage

``` r
get_tags()
```

## Value

Response will be a data frame containing the tags that are in the
organization. Each tag has the following variables:

|                 |          |                                     |
|-----------------|----------|-------------------------------------|
| **Column Name** | **Type** | **Description**                     |
| **id**          | *chr*    | tag's unique ID                     |
| **name**        | *chr*    | tag's given name                    |
| **description** | *chr*    | description of tag provided by user |

## Examples

``` r
if (FALSE) { # \dontrun{
# This is an example of how the function would be called.

df_tags <- get_tags()

} # }
```
