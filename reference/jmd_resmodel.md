# JMD Reservoir Model

Elevation-storage-discharge relationship for John McGraw Reservoir.
Reservoir models must be monotonic.

## Usage

``` r
jmd_resmodel
```

## Format

A data frame with 3 columns:

- stage_ft:

  Reservoir stage/elevation data (FT-NAVD88)

- stor_acft:

  Cumulative reservoir storage (ACRE-FT) corresponding to reservoir
  elevation (elev_ft).

- discharge_cfs:

  Cumulative discharge/outflow (CFS) corresponding to reservoir
  elevation (elev_ft).

## Examples

``` r
sapply(jmd_resmodel,class)
#>      stage_ft     stor_acft discharge_cfs 
#>     "numeric"     "numeric"     "numeric" 
head(jmd_resmodel)
#>   stage_ft stor_acft discharge_cfs
#> 1   3784.8         0             0
#> 2   3785.8        10             0
#> 3   3786.8        48             0
#> 4   3787.8       182             0
#> 5   3788.8       446             0
#> 6   3789.8       790             0
plot(jmd_resmodel[[1]], jmd_resmodel[[2]],
     xlab = "Elevation (ft)", ylab = "Storage (acre-ft)",
     type = "l")
```
