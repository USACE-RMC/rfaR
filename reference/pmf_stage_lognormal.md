# Parameterize a Three-Parameter Lognormal PMF for Stage

Computes the parameters of a three-parameter lognormal distribution for
use as a probabilistic maximum stage (PMF) in rejection sampling. The
shift parameter defines the lower or upper bound of the distribution
(typically, the lower value). The function supports two modes: (1)
supplying a best estimate and assuming sigma, or (2) supplying a best
estimate, low, and high to solve for sigma numerically.

## Usage

``` r
pmf_stage_lognormal(
  pmf_shift,
  pmf_best,
  pmf_sigma = NULL,
  pmf_low = NULL,
  pmf_high = NULL
)
```

## Arguments

- pmf_shift:

  Numeric. Hard lower bound (shift) of the lognormal distribution. Must
  be less than all of `pmf_best`, `pmf_low`, and `pmf_high`.

- pmf_best:

  Numeric. Best estimate (mean) of the PMF stage distribution.

- pmf_sigma:

  Numeric. Assumed standard deviation on the log scale. Required if
  `pmf_low` and `pmf_high` are not supplied. Defaults to `NULL`.
  Suggested value = 0.5.

- pmf_low:

  Numeric. Low estimate of PMF stage (assumed 5th percentile). Optional.
  If supplied, `pmf_high` must also be supplied.

- pmf_high:

  Numeric. High estimate of PMF stage (assumed 95th percentile).
  Optional. If supplied, `pmf_low` must also be supplied.

## Value

A named list with the following elements:

- pmf_shift:

  Hard lower bound of the distribution.

- pmf_best:

  Best estimate of PMF stage.

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
# Mode 1: assume sigma
pmf <- pmf_stage_lognormal(pmf_shift = 239, pmf_best = 241.9, pmf_sigma = 0.5)

# Mode 2: solve for sigma from low/high estimates
pmf <- pmf_stage_lognormal(pmf_shift = 239, pmf_best = 241.9,
                            pmf_low = 239, pmf_high = 245)
```
