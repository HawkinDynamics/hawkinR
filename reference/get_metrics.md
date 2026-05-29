# Get Test Metrics

Get the metrics and ids for all the metrics in the system.

## Usage

``` r
get_metrics(testType = "all")
```

## Arguments

- testType:

  Supply a value of type string. Must be canonical test Id, test type
  name, or test name abbreviation.

  Names \| Abbreviations: "Countermovement Jump" \| "CMJ", "Squat Jump"
  \| "SJ", "Isometric Test" \| "ISO", "Drop Jump" \| "DJ", "Free Run" \|
  "FREE", "CMJ Rebound" \| "CMJR", "Multi Rebound" \| "MR", "Weigh In"
  \| "WI", "Drop Landing" \| "DL", "TS Isometric Test" \| "TSISO", "TS
  Free Run" \| "TSFREE"

## Value

Response will be a data frame containing the tests metrics that are in
the HD system. The parameter `testType` can be used to filter and return
only metrics of the specified type.

The returned data frame will follow the following schema:

|  |  |  |
|----|----|----|
| **Column Name** | **Type** | **Description** |
| **canonicalTestTypeID** | *chr* | Canonical Test Id |
| **testTypeName** | *chr* | Given Test Name |
| **id** | *chr* | camelCase Test Name |
| **label** | *chr* | Outward facing label or title |
| **label_unit** | *chr* | Outward facing label or title w/ unit of measure |
| **header** | *chr* | header of data frame output |
| **units** | *chr* | Unit of measure (if any) |
| **description** | *chr* | Description or definition of metric |

## Examples

``` r
if (FALSE) { # \dontrun{
# This is an example of how the function would be called.

df_testsMetrics <- get_metrics(testType = "CMJ")

} # }
```
