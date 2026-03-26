# JMD Flood Seasonality

Results of flood seasonality analysis from the full John McGraw Dam
period of record. Determined using: threshold flow of 5,000 cfs,
critical duration of 2 days, max events per year of 5, minimum days
between events of 7.

## Usage

``` r
jmd_seasonality
```

## Format

A data frame with 12 rows and 4 columns:

- month:

  Month (1-12)

- frequency:

  Event frequency

- relative_frequency:

  Relative frequency

- cume_rel_frequency:

  Cumulative relative frequency

## Examples

``` r
print(jmd_seasonality)
#>        month frequency relative_frequency cume_rel_frequency
#> 1    January         0              0.000              0.000
#> 2   February         0              0.000              0.000
#> 3      March         0              0.000              0.000
#> 4      April         1              0.018              0.018
#> 5        May        14              0.246              0.263
#> 6       June        17              0.298              0.561
#> 7       July        12              0.211              0.772
#> 8     August        11              0.193              0.965
#> 9  September         2              0.035              1.000
#> 10   October         0              0.000              1.000
#> 11  November         0              0.000              1.000
#> 12  December         0              0.000              1.000
```
