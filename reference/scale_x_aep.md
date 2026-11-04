# AEP-Labelled Normal-Variate X Scale

Creates a continuous ggplot2 x-axis positioned on the standard-normal
variate scale while displaying labels as annual exceedance probabilities
(AEPs). This is useful for stage-frequency, flow-frequency, and similar
conceptual plots where values are modeled or transformed in
normal-variate space but should be interpreted as probabilities.

## Usage

``` r
scale_x_aep(
  aep_breaks = c(0.99, 0.9, 0.5, 0.1, 0.01, 0.001, 1e-04, 1e-05, 1e-06, 1e-07, 1e-08,
    1e-09, 1e-10),
  minor_aep = 2:9
)
```

## Arguments

- aep_breaks:

  Numeric vector of annual exceedance probabilities to use as major axis
  breaks. Values must be greater than 0 and less than 1. Defaults range
  from 0.99 through \\10^{-10}\\.

- minor_aep:

  Numeric vector of multipliers used to generate minor AEP breaks within
  each decade. The default, `2:9`, produces minor breaks such as 0.2
  through 0.9, 0.02 through 0.09, and so on.

## Value

A ggplot2 continuous x-scale object that can be added to a `ggplot` via
`+`.

## Details

Major breaks are specified as AEP values and transformed using
`qnorm(1 - AEP)`. Minor breaks are generated from the supplied
multipliers across AEP decades from \\10^{-1}\\ through \\10^{-10}\\.

## Examples

``` r
library(ggplot2)

example_data <- data.frame(
  AEP = c(0.99, 0.5, 0.1, 0.01, 0.001),
  Stage = c(10.2, 11.5, 14.0, 17.3, 20.1)
)

ggplot(example_data, aes(x = qnorm(1 - AEP), y = Stage)) +
  geom_line() +
  scale_x_aep() +
  labs(
    title = "Example Stage-Frequency Curve",
    x = "Annual Exceedance Probability",
    y = "Stage (ft)"
  )
```
