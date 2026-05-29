# Construct Athlete df

Take athlete section from data frame and prep for final data frame. Uses
the same v1.14 schema as get_athletes() — id/name/active/teams/
groups/image are explicit core columns; position/dob/sport/height/
lastTestedOn are optional and only appear when at least one athlete has
the field populated; external is a nested object unnested into separate
columns. Everything is prefixed with "athlete\_" so test rows can
namespace athlete data alongside metrics.

## Usage

``` r
AthletePrep(arg_df)
```

## Arguments

- arg_df:

  Data frame to be evaluated.

## Value

data frame of athlete information, all columns prefixed "athlete\_"
