# Jay McGraw Dam June 1921 Hydrograph

Inflow hydrograph shape from June 1921 flood event.

## Usage

``` r
jmd_hydro_jun1921
```

## Format

A data frame with 169 rows and 4 columns:

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
head(jmd_hydro_jun1921)
#>   Ordinate     Date Time Flow
#> 1        1 6/4/1921 0:00  500
#> 2        2 6/4/1921 1:00  542
#> 3        3 6/4/1921 2:00  583
#> 4        4 6/4/1921 3:00  625
#> 5        5 6/4/1921 4:00  667
#> 6        6 6/4/1921 5:00  708
# \donttest{
plot(jmd_hydro_jun1921$Ordinate, jmd_hydro_jun1921$Flow,
     xlab = "Ordinate", ylab = "Inflow (cfs)",
     type = "l")

# }
```
