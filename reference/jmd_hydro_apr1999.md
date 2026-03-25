# Jay McGraw Dam April 1999 Hydrograph

Inflow hydrograph shape from April 1999 flood event.

## Usage

``` r
jmd_hydro_apr1999
```

## Format

A data frame with 241 rows and 4 columns:

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
head(jmd_hydro_apr1999)
#>   Ordinate      Date Time Flow
#> 1        1 4/30/1999 0:00  213
#> 2        2 4/30/1999 1:00  239
#> 3        3 4/30/1999 2:00  262
#> 4        4 4/30/1999 3:00  280
#> 5        5 4/30/1999 4:00  300
#> 6        6 4/30/1999 5:00  310
# \donttest{
plot(jmd_hydro_apr1999$Ordinate, jmd_hydro_apr1999$Flow,
     xlab = "Ordinate", ylab = "Inflow (cfs)",
          type = "l")

# }
```
