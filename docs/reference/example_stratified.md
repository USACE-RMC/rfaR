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

[`stratified_sampler()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/stratified_sampler.md)

## Examples

``` r
example_stratified[sample(nrow(example_stratified), 5), ]
#>    distribution bin     lower     upper      weight
#> 35       normal  15  3.230497  3.627414 0.000474740
#> 3       uniform   3  0.891000  0.841500 0.049499999
#> 33       normal  13  2.436662  2.833579 0.005110276
#> 56          ev1  16 13.433716 14.431109 0.000000925
#> 22       normal   2 -1.929430 -1.532513 0.035859225
```
