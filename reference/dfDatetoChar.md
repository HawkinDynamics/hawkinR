# Convert Date-Time formats to Character Strings

This function takes a data frame of test trials and searches for any
columns with a date class. Then it will convert them to a character
class.

## Usage

``` r
dfDatetoChar(arg_df)
```

## Arguments

- arg_df:

  A data frame containing flattened athlete test data.

## Value

A data frame where the specified columns have been converted to nested
lists.
