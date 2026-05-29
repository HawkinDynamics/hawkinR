# Expand Comma-Separated Values To Nested Lists

This function takes a data frame of athlete test output, which contains
comma-separated strings (test tag name, test tag id, test tag
description, athlete team, and athlete group), and expands them into
nested lists.

## Usage

``` r
dfTests_expand(arg_df)
```

## Arguments

- arg_df:

  A data frame containing flattened athlete test data.

## Value

A data frame where the specified columns have been converted to nested
lists.
