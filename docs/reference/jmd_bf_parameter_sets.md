# Jay McGraw Dam BestFit Parameter Sets

LP3 distribution parameters sets of volume-frequency curve results from
RMC-BestFit 2.0.

## Usage

``` r
jmd_bf_parameter_sets
```

## Format

A data frame with 10000 rows and 3 columns:

- mean_log:

  Mean of log-transformed values

- sd_log:

  Standard deviation of log-transformed values

- skew_log:

  Skewness of log-transformed values

- log_likelihood:

  Log-likelihood of parameter set

## Examples

``` r
jmd_bf_parameter_sets[sample(nrow(jmd_bf_parameter_sets), 5), ]
#>      mean_log    sd_log  skew_log log_likelihood
#> 8214 3.527913 0.3463826 0.7973568      -1093.541
#> 5192 3.559503 0.3910620 0.8534204      -1093.236
#> 1025 3.531917 0.3775757 0.8278522      -1092.808
#> 8831 3.565623 0.3914104 0.7965407      -1093.045
#> 2510 3.628081 0.3991306 0.4527677      -1095.041
```
