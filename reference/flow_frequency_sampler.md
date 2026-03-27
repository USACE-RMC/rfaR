# Flow Frequency Sampler

Generates a stratified matrix of flow values from a single set of
frequency distribution parameters using stratified Monte Carlo sampling.
Used internally by
[`rfa_simulate()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/rfa_simulate.md)
for median-only and full uncertainty modes.

## Usage

``` r
flow_frequency_sampler(
  bestfit_params,
  freq_dist = "LP3",
  strat_dist = "ev1",
  Nbin = NULL,
  Mevent = NULL
)
```

## Arguments

- bestfit_params:

  Numeric vector of length 3 containing distribution parameters. For
  LP3: `c(mean_log, sd_log, skew_log)`. For GEV:
  `c(location, scale, shape)`.

- freq_dist:

  Character. Distribution type. Either `"LP3"` (default) or `"GEV"`.

- strat_dist:

  Character. Probability space for stratification bins. Passed to
  [`stratified_sampler()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/stratified_sampler.md).
  One of `"ev1"` (default), `"normal"`, or `"uniform"`. See
  [`stratified_sampler()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/stratified_sampler.md)
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
  [`stratified_sampler()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/stratified_sampler.md)

## See also

[`stratified_sampler()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/stratified_sampler.md),
[`qp3()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/qp3.md),
[`rfa_simulate()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/rfa_simulate.md)

## Examples

``` r
# Single parameter set (posterior mode)
params <- c(4.85, 0.39, -0.15)
result <- flow_frequency_sampler(params, freq_dist = "LP3",
                                 Nbin = 20, Mevent = 500)
dim(result$flow)  # 500 x 20
#> [1] 500  20
hist(result$flow)
```
