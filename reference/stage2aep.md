# Interpolate AEP from Stage

Given a stage-frequency curve, interpolates AEP values at target stages.
Interpolation is performed in standard normal (z-variate) space for
better behavior across orders of magnitude in AEP.

## Usage

``` r
stage2aep(aep, stage, interp_stage)
```

## Arguments

- aep:

  Numeric vector of annual exceedance probabilities from the known
  curve.

- stage:

  Numeric vector of stage values corresponding to `aep`.

- interp_stage:

  Numeric vector of target stage values to interpolate at.

## Value

Numeric vector of interpolated AEP values (same length as
`interp_stage`).

## See also

[`aep2stage`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/aep2stage.md)
for the inverse operation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get AEP at stage 3850 ft
stage2aep(curve$AEP, curve$stage, 3850)
} # }
```
