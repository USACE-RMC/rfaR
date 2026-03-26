# Jay McGraw Dam Monthly Stage Duration Curves

Monthly stage duration curves from Water Year 1980-2024.

## Usage

``` r
jmd_stage_duration
```

## Format

A data frame with 45 rows and 13 columns:

- Probability:

  Probability of exceedance

- January:

  Stage for January (FT-NAVD88)

- February:

  Stage for February (FT-NAVD88)

- March:

  Stage for March (FT-NAVD88)

- April:

  Stage for April (FT-NAVD88)

- May:

  Stage for May (FT-NAVD88)

- June:

  Stage for June (FT-NAVD88)

- July:

  Stage for July (FT-NAVD88)

- August:

  Stage for August (FT-NAVD88)

- September:

  Stage for September (FT-NAVD88)

- October:

  Stage for October (FT-NAVD88)

- November:

  Stage for November (FT-NAVD88)

- December:

  Stage for December (FT-NAVD88)

## Examples

``` r
head(jmd_stage_duration)
#>   Probability January February   March   April     May    June    July  August
#> 1       0.999 3798.15  3802.57 3805.61 3798.95 3795.34 3794.81 3792.28 3791.99
#> 2       0.998 3798.39  3802.72 3805.73 3799.04 3795.65 3794.83 3792.32 3792.04
#> 3       0.997 3798.61  3802.93 3805.85 3799.20 3795.98 3794.85 3792.33 3792.05
#> 4       0.996 3798.85  3803.11 3805.98 3799.31 3796.27 3794.86 3792.34 3792.12
#> 5       0.995 3799.06  3803.28 3806.11 3799.39 3796.61 3794.86 3792.41 3792.16
#> 6       0.994 3799.19  3803.42 3806.22 3799.44 3796.89 3794.88 3792.64 3792.18
#>   September October November December
#> 1   3793.03 3790.46  3791.74  3794.61
#> 2   3793.07 3790.47  3791.95  3794.72
#> 3   3793.08 3790.48  3792.12  3794.89
#> 4   3793.12 3790.52  3792.25  3794.98
#> 5   3793.18 3790.53  3792.38  3795.06
#> 6   3793.21 3790.54  3792.48  3795.16
```
