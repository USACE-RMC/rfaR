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

## Details

Stage-frequency curves typically flatten at very rare AEPs once the
reservoir reaches the reservoir model discharge capacity. This produces
tied stage values. [`approx()`](https://rdrr.io/r/stats/approxfun.html)
handles these by averaging the corresponding AEPs, which may emit a
"collapsing to unique 'x' values" warning. This is expected behavior.

## See also

[`aep2stage`](https://usace-rmc.github.io/rfaR/reference/aep2stage.md)
for the inverse operation.

## Examples

``` r
# Get AEP at stage 3862.0-ft and 3872-ft
stage2aep(jmd_rfa_expected$AEP, jmd_rfa_expected$Expected, c(3862.0, 3872))
#> Warning: collapsing to unique 'x' values
#> [1] 0.011166982 0.001746593
```
