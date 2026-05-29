# Flatten Nested Lists in Athlete Test Output

This function takes a data frame of athlete test output, which may
contain nested lists or tables, and flattens them into a simple data
frame. It works specifically on the columns that contain lists or other
complex structures.

## Usage

``` r
dfTests_flat(arg_df)
```

## Arguments

- arg_df:

  A data frame containing athlete test data, including columns with
  nested lists or tables.

## Value

A data frame where nested lists or tables have been flattened, making it
easier to manipulate.
