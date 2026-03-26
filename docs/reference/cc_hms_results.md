# Cherry Cricket Dam HEC-HMS Routing Results

Routing results from HEC-HMS using the Cherry Cricket reservoir
model/geometry, inflow hydrograph, and initial reservoir elevation.

## Usage

``` r
cc_hms_results
```

## Format

A data frame with 5 columns:

- time_hr:

  Time in hours (integer)

- inflow_cfs:

  Inflow in cubic-feet per second (CFS)

- elevation_ft:

  Routed reservoir stage/elevation data in FT-NAVD88

- storage_acft:

  Routed reservoir storage (ACRE-FT) corresponding to reservoir
  elevation (elev_ft).

- outflow_cfs:

  Routed discharge/outflow (CFS) corresponding to reservoir elevation
  (elev_ft).

## Examples

``` r
head(cc_hms_results)
#>   time_hr inflow_cfs elevation_ft storage_acft outflow_cfs
#> 1       0         15     5565.000     28347.00    750.0000
#> 2       1         15     5564.952     28286.42    745.9841
#> 3       2         15     5564.904     28226.18    741.9902
#> 4       3         15     5564.856     28166.26    738.0181
#> 5       4         15     5564.809     28106.67    734.0677
#> 6       5         15     5564.762     28047.40    730.1389
```
