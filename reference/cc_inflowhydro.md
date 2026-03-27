# Cherry Cricket Dam Inflow Hydrograph

Inflow hydrograph for an example flood event at Cherry Cricket Dam.

## Usage

``` r
cc_inflowhydro
```

## Format

A data frame with 2 columns:

- time_hr:

  Time in hours (integer)

- inflow_cfs:

  Inflow in cubic-feet per second (CFS)

## Examples

``` r
sapply(cc_inflowhydro,class)
#>    time_hr inflow_cfs 
#>  "integer"  "integer" 
head(cc_inflowhydro)
#>   time_hr inflow_cfs
#> 1       0         15
#> 2       1         15
#> 3       2         15
#> 4       3         15
#> 5       4         15
#> 6       5         15
```
