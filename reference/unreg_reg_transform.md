# Create an Unregulated-to-Regulated Inflow Transformation Function

Constructs a function that transforms unregulated inflow values to
corresponding regulated inflow values using linear interpolation. By
default, input values outside the range of the supplied unregulated
inflows are returned unchanged.

## Usage

``` r
unreg_reg_transform(unreg_inflow, reg_inflow, extend = TRUE)
```

## Arguments

- unreg_inflow:

  Numeric vector of unregulated inflow values. Values should represent
  the independent variable for the interpolation.

- reg_inflow:

  Numeric vector of regulated inflow values corresponding element-wise
  to `unreg_inflow`.

- extend:

  Logical; if `TRUE` (default), values passed to the returned function
  that fall outside the observed range of `unreg_inflow` are returned
  unchanged. If `FALSE`, out-of-range values return `NA`.

## Value

A function accepting a numeric vector `x` and returning a numeric vector
of interpolated regulated inflow values.

## Details

The transformation uses
[`stats::approxfun()`](https://rdrr.io/r/stats/approxfun.html) with
`rule = 1`, which produces `NA` for values outside the interpolation
range. When `extend = TRUE`, those out-of-range values are replaced with
their original input values rather than extrapolated.

Missing values in `x` are preserved as `NA`. The minimum and maximum
interpolation bounds are calculated with missing values removed from
`unreg_inflow`.

## Examples

``` r
unreg_inflow <- c(0, 100, 200, 300)
reg_inflow <- c(0, 90, 170, 240)

transform_inflow <- unreg_reg_transform(unreg_inflow, reg_inflow)

# Interpolated value
transform_inflow(150)
#> [1] 130

# Out-of-range values are unchanged when extend = TRUE
transform_inflow(c(-10, 50, 350))
#> [1] -10  45 350

# Out-of-range values return NA when extend = FALSE
strict_transform <- unreg_reg_transform(
  unreg_inflow,
  reg_inflow,
  extend = FALSE
)
strict_transform(c(-10, 50, 350))
#> [1] NA 45 NA
```
