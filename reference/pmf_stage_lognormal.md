# Parameterize a Three-Parameter Lognormal PMF for Stage

Computes the parameters of a three-parameter lognormal distribution for
use as a probabilistic maximum stage (PMF) in rejection sampling. The
shift parameter defines the lower bound of the distribution.

## Usage

``` r
pmf_stage_lognormal(pmf_shift, pmf_mean, pmf_sigma = 0.5)
```

## Arguments

- pmf_shift:

  Numeric. Lower bound (shift) of the lognormal distribution. Must be
  greater than `pmf_mean`.

- pmf_mean:

  Numeric. Mean of the PMF stage distribution. Must be less than
  `pmf_shift`.

- pmf_sigma:

  Numeric. Standard deviation on the log scale. Defaults to `0.5`.

## Value

A named list with the following elements:

- pmf_shift:

  Lower bound of the distribution.

- pmf_mean:

  Mean of the distribution.

- pmf_sigma:

  Standard deviation on the log scale.

- pmf_mu:

  Derived location parameter on the log scale.

- pmf_p05:

  5th percentile of the PMF stage distribution.

- pmf_p95:

  95th percentile of the PMF stage distribution.

## Examples

``` r
pmf <- pmf_stage_lognormal(pmf_shift = 1200, pmf_mean = 1150, pmf_sigma = 0.5)
#> Warning: NaNs produced
pmf$pmf_p05
#> [1] NaN
pmf$pmf_p95
#> [1] NaN
```
