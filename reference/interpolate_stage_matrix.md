# Interpolate Stages at Target AEPs Across Realizations

For each realization's stage-frequency curve, interpolates the stage at
every target AEP value. Interpolation is performed in standard normal
(z-variate) space.

## Usage

``` r
interpolate_stage_matrix(results_list, target_aeps)
```

## Arguments

- results_list:

  List of realization data frames, each with columns `stage` and `AEP`.

- target_aeps:

  Numeric vector of AEP values to interpolate at.

## Value

Matrix of stage values with rows = target AEPs and columns =
realizations.
