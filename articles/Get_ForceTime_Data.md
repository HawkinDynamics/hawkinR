# Accessing Force-Time Data of A Test

There are many cases in which a user would want to access the raw data
from a test trial. Just like the rest of your data, you can access the
same data used to create the force plot in your app and cloud. With just
a couple of lines of code, you can call in the force-time data from any
test in your system.

------------------------------------------------------------------------

## 1. Get Access

Like always, the first step to getting your data is initializing your
session. Simply load the hawkinR package and run the `get_access`
function.

``` r

# load the hawkinR package
library(hawkinR)

# initialize your current session
get_access(refreshToken = 'your_integration_key', region = 'your_region')
```

## 2. Get Tests

For this example, I am going to call for only my tests. To do this I am
going to need to get my unique ID first. So that I can include that in
the `get_tests_ath` function.

``` r

# create a data frame of my athlete info 
roster <- get_athletes()

# My Athlete ID
myID <- roster$id[roster$name == "Lauren Green"]

# call your tests
myTests <- get_tests_ath(athleteId = myID)
```

| id | segment | testType.canonicalId | athlete.name |
|:---|:---|:---|:---|
| 9Ytz9g1erMXm3SByTyEd | Countermovement Jump:1 | 7nNduHeM5zETPjHxvm7s | Lauren Green |
| G1yfTWvTj3hAveQYz5wH | Weigh In:1 | ubeWMPN1lJFbuQbAM97s | Lauren Green |
| YB35oOBAGHNQew0WziDt | Drop Landing:1 | rKgI4y3ItTAzUekTUpvR | Lauren Green |
| 2RnV4tM3J6IW2qYKgqg2 | Drop Landing:2 | rKgI4y3ItTAzUekTUpvR | Lauren Green |
| qNIZaBguZefAyar4oUtu | Drop Jump:1 | gyBETpRXpdr63Ab2E0V8 | Lauren Green |

## 3. Get Test ID

Now that I have a list of the tests I want to evaluate, I can call for
the force-time data I want by using the tests unique ID.

Every test has it’s own unique identifier. This is all that’s needed to
get your test data. The test Id is found in the first column of any of
the `get_tests` function returns.

In this example, I am calling the first test of my data frame, which is
a CMJ trial.

``` r

# Get the ID of the test
myTestID <- myTests$id[1]

# Get the force-time data
myFT <- get_forcetime(testId = myTestID)
```

| time_s | force_right | force_left | force_combined | velocity_m_s | displacement_m | power_w |
|---:|---:|---:|---:|---:|---:|---:|
| 0.001 | 622 | 564 | 1186 | 0 | 0 | 0 |
| 0.002 | 622 | 564 | 1186 | 0 | 0 | 0 |
| 0.003 | 622 | 564 | 1186 | 0 | 0 | 0 |
| 0.004 | 623 | 564 | 1187 | 0 | 0 | 0 |
| 0.005 | 622 | 564 | 1186 | 0 | 0 | 0 |
| 0.006 | 622 | 565 | 1187 | 0 | 0 | 0 |
| 0.007 | 623 | 566 | 1189 | 0 | 0 | 0 |
| 0.008 | 622 | 566 | 1188 | 0 | 0 | 0 |
| 0.009 | 621 | 566 | 1187 | 0 | 0 | 0 |
| 0.010 | 620 | 566 | 1186 | 0 | 0 | 0 |
| 0.011 | 620 | 567 | 1187 | 0 | 0 | 0 |
| 0.012 | 619 | 567 | 1186 | 0 | 0 | 0 |
| 0.013 | 619 | 568 | 1187 | 0 | 0 | 0 |
| 0.014 | 619 | 568 | 1187 | 0 | 0 | 0 |
| 0.015 | 618 | 568 | 1186 | 0 | 0 | 0 |
| 0.016 | 618 | 568 | 1186 | 0 | 0 | 0 |
| 0.017 | 619 | 568 | 1187 | 0 | 0 | 0 |
| 0.018 | 620 | 568 | 1188 | 0 | 0 | 0 |
| 0.019 | 620 | 569 | 1189 | 0 | 0 | 0 |
| 0.020 | 620 | 568 | 1188 | 0 | 0 | 0 |

And just like that we have our force-time data. You can do this to with
any of your test types.
