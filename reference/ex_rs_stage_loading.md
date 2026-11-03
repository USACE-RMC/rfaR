# Rejection Sampling - Example Data

Hydrologic Loading (stage-frequency) data that can be used in the
PMF-stage rejection sampling process.

## Usage

``` r
ex_rs_stage_loading
```

## Format

A data frame with 89 rows and 2 columns:

- aep:

  Annual Exceedance Probabilities

- stage:

  Expected stages corresponding to AEPs

## See also

[`rejection_sampling_stage()`](https://usace-rmc.github.io/rfaR/reference/rejection_sampling_stage.md)

## Examples

``` r
head(ex_rs_stage_loading)
#>    aep   stage
#> 1 0.98 1570.38
#> 2 0.97 1573.56
#> 3 0.96 1575.17
#> 4 0.95 1576.35
#> 5 0.94 1577.64
#> 6 0.93 1579.00
```
