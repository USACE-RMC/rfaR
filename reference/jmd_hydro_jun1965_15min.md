# Jay McGraw Dam June 1965 Hydrograph (15 minute intervals)

Inflow hydrograph shape from June 1965 flood event (from HEC-HMS).

## Usage

``` r
jmd_hydro_jun1965_15min
```

## Format

A data frame with 481 rows and 4 columns:

- Ordinate:

  Timeseries Ordinate

- Date:

  Date mm/dd/yyyy

- Time:

  Time in 00:00

- Flow:

  Inflow (cfs)

## Examples

``` r
head(jmd_hydro_jun1965_15min)
#>   Ordinate      Date Time Flow
#> 1        1 6/17/1965 7:00    0
#> 2        2 6/17/1965 7:15   24
#> 3        3 6/17/1965 7:30   49
#> 4        4 6/17/1965 7:45   73
#> 5        5 6/17/1965 8:00  692
#> 6        6 6/17/1965 8:15 1339
# \donttest{
plot(jmd_hydro_jun1965_15min$Ordinate, jmd_hydro_jun1965_15min$Flow,
     xlab = "Ordinate", ylab = "Inflow (cfs)",
     type = "l")

# }
```
