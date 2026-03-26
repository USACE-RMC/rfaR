# Jay McGraw Dam RFA Results - Full Uncertainty

Jay McGraw Dam, full uncertainty stage-frequency results from RFA.

## Usage

``` r
jmd_rfa_full
```

## Format

A data frame with 10000 rows and 3 columns:

- AEP:

  Annual Exceedance Probabilities

- Upper:

  Upper 95% CI of realizations

- Lower:

  Lower 5% CI of realizations

- Expected:

  Expected stages corresponding to AEPs

- Median:

  Median stages corresponding to AEPs

## Examples

``` r
head(jmd_rfa_full)
#>    AEP   Upper   Lower Expected  Median
#> 1 0.98 3804.28 3802.80  3803.47 3803.47
#> 2 0.97 3805.42 3804.04  3804.67 3804.68
#> 3 0.96 3806.31 3804.94  3805.59 3805.60
#> 4 0.95 3807.02 3805.72  3806.36 3806.37
#> 5 0.94 3807.62 3806.41  3807.02 3807.01
#> 6 0.93 3808.18 3807.02  3807.58 3807.57
```
