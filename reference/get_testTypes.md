# Get Test Types

Get the test type names and ids for all the test types in the system.

## Usage

``` r
get_testTypes()
```

## Value

Response will be a data frame containing the tests that are in the HD
system. Each test type includes the following variables:

|                  |          |                              |
|------------------|----------|------------------------------|
| **Column Name**  | **Type** | **Description**              |
| **canonicalId**  | *chr*    | test's unique canonical ID   |
| **name**         | *chr*    | test's given name            |
| **abbreviation** | *chr*    | test given name abbreviation |

## Examples

``` r
if (FALSE) { # \dontrun{
# This is an example of how the function would be called.

df_tests <- get_testTypes()

} # }
```
