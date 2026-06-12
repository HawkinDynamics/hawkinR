# Monitor Authentication Validity

Looped checks of access token validation. Polling continues only while
the token is valid; once it expires (or was never set) the loop stops,
and the next `get_*()` call prompts re-authentication. The loop also
stops on any unexpected error to avoid an endless 5-second error loop in
the console.

## Usage

``` r
monitor_token()
```
