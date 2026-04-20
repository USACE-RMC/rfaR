# Flow Frequency Sampler (Expected Only)

Generates a stratified matrix of flow values by pairing each z-ordinate
with a different parameter set from the posterior distribution. This
collapses the nested Monte Carlo structure into a single pass,
simultaneously sampling natural variability (via stratified z-ordinates)
and knowledge uncertainty (via varying parameters). Used internally by
[`rfa_simulate()`](https://usace-rmc.github.io/rfaR/reference/rfa_simulate.md)
for expected-only mode.

## Usage

``` r
flow_frequency_sampler_expected(
  bestfit_params,
  freq_dist = "LP3",
  strat_dist = "ev1",
  Nbin = NULL,
  Mevent = NULL
)
```

## Arguments

- bestfit_params:

  Data frame of distribution parameters from RMC-BestFit. Must have
  `Nbin * Mevent` rows (one parameter set per z-ordinate). For LP3:
  columns are mean (log), sd (log), skew (log). For GEV: columns are
  location, scale, shape.

- freq_dist:

  Character. Distribution type. Either `"LP3"` (default) or `"GEV"`.

- strat_dist:

  Character. Probability space for stratification bins. Passed to
  [`stratified_sampler()`](https://usace-rmc.github.io/rfaR/reference/stratified_sampler.md).
  One of `"ev1"` (default), `"normal"`, or `"uniform"`. See
  [`stratified_sampler()`](https://usace-rmc.github.io/rfaR/reference/stratified_sampler.md)
  for details.

- Nbin:

  Integer. Number of stratified bins. Default is `50`.

- Mevent:

  Integer. Number of events per bin. Default is `200`.

## Value

A list containing:

- flow:

  Matrix of sampled flow values `[Mevent x Nbin]`

- nbins:

  Number of stratified bins

- mevents:

  Number of events per bin

- weights:

  Probability weights for each bin from
  [`stratified_sampler()`](https://usace-rmc.github.io/rfaR/reference/stratified_sampler.md)

## See also

[`stratified_sampler()`](https://usace-rmc.github.io/rfaR/reference/stratified_sampler.md),
[`qp3()`](https://usace-rmc.github.io/rfaR/reference/qp3.md),
[`flow_frequency_sampler()`](https://usace-rmc.github.io/rfaR/reference/flow_frequency_sampler.md),
[`rfa_simulate()`](https://usace-rmc.github.io/rfaR/reference/rfa_simulate.md)

## Examples

``` r
# Using a pre-loaded parameter set (all 10,000 parameter sets)
result <- flow_frequency_sampler_expected(jmd_bf_parameter_sets,
                                          Nbin = 20, Mevent = 500)
dim(result$flow)  # 500 x 20
#> [1] 500  20

# Using bootstrapped parameter samples
jmd_samples <- bootstrap_vfc(
  c(jmd_vfc_parameters$mean_log,
    jmd_vfc_parameters$sd_log,
    jmd_vfc_parameters$skew_log),
  dist = "LP3",
  ERL  = jmd_vfc_parameters$erl)

jmd_result <- flow_frequency_sampler_expected(
  jmd_samples$params,
  freq_dist = "LP3",
  Nbin      = 20,
  Mevent    = 500)
```
