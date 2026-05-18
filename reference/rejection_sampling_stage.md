# Rejection Sampling of Stage Bounded by a Probabilistic Maximum Stage

Draws `n_samples` stage values from a stage-frequency curve, rejecting
any draws that exceed a probabilistic maximum stage (PMF) sampled from a
three-parameter lognormal distribution. Accepted samples are ranked and
assigned Weibull plotting positions for use in frequency analysis.

## Usage

``` r
rejection_sampling_stage(
  pmf_stage_LN,
  stage_freq_df = NULL,
  aep = NULL,
  stage = NULL,
  n_samples = 1e+07
)
```

## Arguments

- pmf_stage_LN:

  A named list returned by
  [`pmf_stage_lognormal`](https://usace-rmc.github.io/rfaR/reference/pmf_stage_lognormal.md)
  containing the lognormal PMF parameters.

- stage_freq_df:

  A data frame with AEP in column 1 and stage in column 2. Mutually
  exclusive with `aep` and `stage`.

- aep:

  Numeric vector of annual exceedance probabilities. Must be supplied
  with `stage`. Mutually exclusive with `stage_freq_df`.

- stage:

  Numeric vector of stages corresponding to `aep`. Must be supplied with
  `aep`. Mutually exclusive with `stage_freq_df`.

- n_samples:

  Integer. Number of samples to draw. Defaults to `1e7`.

## Value

A data frame of thinned accepted samples with columns for z-score and
stage, suitable for plotting a probabilistically bounded stage-frequency
curve. Output is produced by
[`thin_samples()`](https://usace-rmc.github.io/rfaR/reference/thin_samples.md).

## See also

[`pmf_stage_lognormal`](https://usace-rmc.github.io/rfaR/reference/pmf_stage_lognormal.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pmf_ln <- pmf_stage_lognormal(pmf_shift = 239, pmf_best = 241.9, pmf_sigma = 0.5)

result <- rejection_sampling_stage(
  pmf_stage_LN  = pmf_ln,
  stage_freq_df = my_stage_freq_df,
  n_samples     = 1e7
)
} # }
```
