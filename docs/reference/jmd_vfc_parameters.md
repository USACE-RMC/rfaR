# JMD VFC Parameters

LP3 distribution parameters of volume-frequency curve results from
RMC-BestFit 2.0.

## Usage

``` r
jmd_vfc_parameters
```

## Format

A data frame with 1 row and 5 columns:

- mean_log:

  Mean of log-transformed values

- sd_log:

  Standard deviation of log-transformed values

- skew_log:

  Skewness of log-transformed values

- erl:

  Estimated effective record length

- duration:

  Inflow-volume duration in days

## Examples

``` r
head(jmd_vfc_parameters)
#>   mean_log sd_log skew_log erl duration
#> 1   3.5504 0.3718   0.7555 600        2
```
