# Cherry Cricket Dam Reservoir Model

Elevation-storage-discharge relationship for Cherry Cricket Reservoir.
Reservoir models must be monotonic.

## Usage

``` r
cc_resmodel
```

## Format

A data frame with 3 columns:

- elev_ft:

  Reservoir stage/elevation data (FT-NAVD88)

- stor_acft:

  Cumulative reservoir storage (ACRE-FT) corresponding to reservoir
  elevation (elev_ft).

- outflow_cfs:

  Cumulative discharge/outflow (CFS) corresponding to reservoir
  elevation (elev_ft).

## Examples

``` r
sapply(cc_resmodel,class)
#>     elev_ft   stor_acft outflow_cfs 
#>   "integer"   "integer"   "numeric" 
head(cc_resmodel)
#>   elev_ft stor_acft outflow_cfs
#> 1    5524         0           0
#> 2    5525        17           0
#> 3    5526       122           0
#> 4    5527       258           0
#> 5    5528       426           0
#> 6    5529       626           0
```
