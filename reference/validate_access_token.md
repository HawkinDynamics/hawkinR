# Retrieve and Validate Stored Access Token

Reads the access token and expiration stored by
[`get_access()`](https://hawkindynamics.github.io/hawkinR/reference/get_access.md),
validates both, and returns the token. A missing, empty, or non-numeric
(`NA`) expiration is treated as an expired token — prompting
re-authentication — rather than being allowed to propagate `NA` into the
calling `if` condition (which raises "missing value where TRUE/FALSE
needed").

## Usage

``` r
validate_access_token(fn_name = "hawkinR")
```

## Arguments

- fn_name:

  Calling function label, used for log context.

## Value

The access token (character). Stops with an instructive error when the
token is missing or expired.
