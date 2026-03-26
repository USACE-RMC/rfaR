# Pearson Type III Inverse CDF (Quantile Function)

Computes quantiles from a Pearson Type III distribution given
probabilities and distribution parameters (mean, standard deviation,
skewness).

## Usage

``` r
qp3(p, mu, sigma, gamma)
```

## Arguments

- p:

  Vector of probabilities (between 0 and 1).

- mu:

  Mean of the distribution.

- sigma:

  Standard deviation of the distribution.

- gamma:

  Skewness coefficient of the distribution.

## Value

Vector of quantiles corresponding to the input probabilities.

## Details

NOTE: This is the non-vectorized version of the function (it will be
used in a loop) Vectorized version is part of future development.

The Pearson Type III distribution is parameterized by mean (`mu`),
standard deviation (`sigma`), and skewness (`gamma`). These are
converted internally to location, scale, and shape parameters.

When skewness is near zero (`abs(gamma) < 1E-3`), the normal
distribution is used as an approximation.

## Examples

``` r
# Single quantile
qp3(0.99, mu = 10, sigma = 2, gamma = 0.5)
#> [1] 15.37144

# Multiple quantiles
qp3(c(0.5, 0.9, 0.99), mu = 10, sigma = 2, gamma = 0.5)
#> [1]  9.833965 12.646186 15.371443

# Use with JMD volume-frequency parameters
qp3(0.99,
    mu = jmd_vfc_parameters$mean_log,
    sigma = jmd_vfc_parameters$sd_log,
    gamma = jmd_vfc_parameters$skew_log)
#> [1] 4.61417
```
