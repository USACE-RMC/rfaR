# Thin rejection sampling output for plotting

Thins a large sample of sorted z-scores and stages for plotting
efficiency. Retains 5,000 evenly-spaced points across the full range
plus the last 10,000 points for dense tail coverage.

## Usage

``` r
thin_samples(n, z_sorted, stage_sorted)
```

## Arguments

- n:

  Integer. Total number of samples.

- z_sorted:

  Numeric vector of sorted z-scores (ascending), length `n`.

- stage_sorted:

  Numeric vector of sorted stage values (ascending), length `n`.

## Value

A data frame with columns `z_aep` and `stage`.
