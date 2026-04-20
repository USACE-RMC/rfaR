# Stratified Sampling - Example Data

Example stratification of probability space into 20 bins using three
distribution transformations: Uniform, Normal, and Extreme Value Type I
(EV1/Gumbel). Demonstrates how different transformations allocate
sampling effort across the probability range, with EV1 concentrating
more bins in the rare event tail critical for dam safety analysis.

## Usage

``` r
example_stratified
```

## Format

A data frame with 60 rows and 5 columns:

- distribution:

  Stratification distribution type: "uniform", "normal", or "ev1"

- bin:

  Bin number (1-20), ordered from most frequent to most rare events

- lower:

  Lower bound of the bin in the distribution's transformed space
  (probability for Uniform, z-score for Normal, Gumbel reduced variate
  for EV1)

- upper:

  Upper bound of the bin in the distribution's transformed space

- weight:

  Probability weight of the bin, representing the fraction of the total
  probability captured by each bin. Weights sum to approximately 1
  within each distribution.

## See also

[`stratified_sampler()`](https://usace-rmc.github.io/rfaR/reference/stratified_sampler.md)

## Examples

``` r
example_stratified[sample(nrow(example_stratified), 5), ]
#>    distribution bin      lower      upper      weight
#> 33       normal  13 2.43666160 2.83357905 0.005110276
#> 5       uniform   5 0.79200000 0.74250000 0.049499999
#> 10      uniform  10 0.54450000 0.49500001 0.049500000
#> 20      uniform  20 0.04950001 0.00000001 0.049500010
#> 16      uniform  16 0.24750001 0.19800001 0.049500000
```
