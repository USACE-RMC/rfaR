# Interpolate AEPs at Target Stages Across Realizations

For each realization's stage-frequency curve, interpolates the AEP at
every target stage value. Interpolation is performed in standard normal
(z-variate) space.

## Usage

``` r
interpolate_aep_matrix(results_list, target_stages)
```

## Arguments

- results_list:

  List of realization data frames, each with columns `stage` and `AEP`.

- target_stages:

  Numeric vector of stage values to interpolate at.

## Value

Matrix of AEP values with rows = target stages and columns =
realizations.
