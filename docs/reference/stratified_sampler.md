# Stratified Sampler for Monte Carlo Simulation

Creates stratified bins in standard normal space for use in stratified
Monte Carlo sampling. Stratification improves sampling efficiency by
ensuring adequate coverage of rare events.

## Usage

``` r
stratified_sampler(
  minAEP = 1e-08,
  maxAEP = 0.99,
  dist = "ev1",
  Nbins = NULL,
  Mevents = NULL
)
```

## Arguments

- minAEP:

  Minimum annual exceedance probability. Default is `1E-8`.

- maxAEP:

  Maximum annual exceedance probability. Default is `0.99`.

- dist:

  Character. Probability space for stratification. One of:

  "EV1"

  :   Extreme Value Type I (Gumbel) space. Recommended for flood
      frequency analysis. Allocates more bins to rare events through the
      transformation `-log(-log(1-AEP))`, improving tail estimation
      efficiency.

  "Normal"

  :   Standard normal (z-score) space. Uniform bins in z-space. Use when
      the underlying phenomenon is normally distributed.

  "Uniform"

  :   Uniform probability space. Equal probability width per bin.
      Generally inefficient for rare event estimation.

  Default is "EV1". Invalid values trigger a warning and default to
  "EV1".

- Nbins:

  Number of stratified bins. Default is `20`.

- Mevents:

  Number of events per bin. Default is `500`.

## Value

A list containing:

- normOrd:

  Vector of standard normal ordinates spanning all bins

- Weights:

  Vector of probability weights for each bin

- Zlower:

  Vector of lower bounds for each bin (standard normal)

- Zupper:

  Vector of upper bounds for each bin (standard normal)

- Nbins:

  Number of bins

- Mevents:

  Number of events per bin

## Details

The function divides the probability space into bins using the specified
transformation. EV1 transformation is recommended for heavy-tailed
distributions common in flood frequency analysis, as it naturally
allocates more sampling effort to rare events critical for dam safety
assessments.

## Examples

``` r
# Default stratification
strat <- stratified_sampler()

# Custom bins and events
strat <- stratified_sampler(minAEP = 1E-6, maxAEP = 0.5, dist = "EV1", Nbins = 10, Mevents = 100)
```
