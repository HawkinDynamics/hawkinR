# Check Authentication Validity

Takes the current token expiration and validates it. A missing, empty,
or non-numeric expiration (which coerces to `NA`) is treated as invalid
rather than being compared against `NA` — the latter raises "missing
value where TRUE/FALSE needed".

## Usage

``` r
check_token_validity()
```

## Value

`TRUE` if the stored token is still valid, otherwise `FALSE` (returned
invisibly).
