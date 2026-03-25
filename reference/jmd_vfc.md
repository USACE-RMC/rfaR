# Jay McGraw Dam Volume-Frequency Curve

Tabular volume-frequency curve results from RMC-BestFit 2.0.

## Usage

``` r
jmd_vfc
```

## Format

A data frame with 25 rows and 5 columns:

- aep:

  Annual exceedance probability

- ci_95:

  95% credible interval

- ci_5:

  5% credible interval

- posterior_predictive:

  Posterior predictive value

- posterior_mode:

  Posterior mode value

## Examples

``` r
head(jmd_vfc)
#>     aep     ci_95     ci_5 posterior_predictive posterior_mode
#> 1 1e-06 6311066.1 772770.1            3146263.1      2540637.6
#> 2 2e-06 4492836.2 629905.9            2284549.7      1910347.4
#> 3 5e-06 2860344.7 479952.1            1498256.5      1305324.8
#> 4 1e-05 2023452.0 387635.0            1089633.2       975392.1
#> 5 2e-05 1429747.3 313505.6             792677.1       726562.5
#> 6 5e-05  900982.4 232158.9             520430.7       489589.7
```
