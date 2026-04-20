# Interpolate Stage from AEP

Given a stage-frequency curve, interpolates stage values at target AEPs.
Interpolation is performed in standard normal (z-variate) space for
better behavior across orders of magnitude in AEP.

## Usage

``` r
aep2stage(aep, stage, interp_aep)
```

## Arguments

- aep:

  Numeric vector of annual exceedance probabilities from the known
  curve.

- stage:

  Numeric vector of stage values corresponding to `aep`.

- interp_aep:

  Numeric vector of target AEPs to interpolate at.

## Value

Numeric vector of interpolated stage values (same length as
`interp_aep`).

## See also

[`stage2aep`](https://usace-rmc.github.io/rfaR/reference/stage2aep.md)
for the inverse operation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get stage at the 1% and 0.1% AEP
aep2stage(curve$AEP, curve$stage, c(0.01, 0.001))
} # }
```
