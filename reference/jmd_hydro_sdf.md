# JMD SDF Hydrograph

Inflow hydrograph shape for Spillway Design Flood (SDF).

## Usage

``` r
jmd_hydro_sdf
```

## Format

A data frame with 337 rows and 2 columns:

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
head(jmd_hydro_sdf)
#>   Ordinate     Date Time  Flow
#> 1        1 1/1/1900 0:00 20000
#> 2        2 1/1/1900 1:00 20000
#> 3        3 1/1/1900 2:00 20000
#> 4        4 1/1/1900 3:00 20000
#> 5        5 1/1/1900 4:00 20000
#> 6        6 1/1/1900 5:00 20000
# \donttest{
plot(jmd_hydro_sdf$Ordinate, jmd_hydro_sdf$Flow,
     xlab = "Ordinate", ylab = "Inflow (cfs)",
     type = "l")

# }
```
