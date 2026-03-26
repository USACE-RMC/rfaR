# Compute Stage-Frequency Curve

Converts a matrix of routed peak stages into a stage-frequency curve
using the law of total probability across stratified bins. For each
candidate stage, the conditional exceedance probability within each bin
is weighted by the bin probability and summed to produce the
unconditional annual exceedance probability (AEP).

## Usage

``` r
stage_frequency_curve(peakStage, weights, stage_bins = 1000)
```

## Arguments

- peakStage:

  Matrix of peak stages `[Mevents x Nbins]` from routing.

- weights:

  Numeric vector of bin weights from
  [`stratified_sampler()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/stratified_sampler.md).
  Must have length equal to `ncol(peakStage)`.

## Value

A data frame with columns:

- stage:

  Evaluated stage values

- AEP:

  Annual exceedance probability at each stage

## See also

[`stratified_sampler()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/stratified_sampler.md),
[`flow_frequency_sampler()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/flow_frequency_sampler.md),
[`rfa_simulate()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/rfa_simulate.md)
