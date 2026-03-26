# MCD GEV Example

## Summary

This article steps through an example of `rfa_simulate` (expected only)
using a GEV inflow volume-frequency parameter set. The example will load
the required data and display the formatting currently required to
successful execute `rfa_simulate`. The example project is the Marcus
Camby Dam (MCD & `mcd_`).

## Analysis Set Up

The libraries loaded after `rfaR` are intended to make data analysis and
visualization easier.

All of the MCD data is stored in sub-directories within this parent
directory. Setting up the parent directory as an R-project can also make
searching for files/directories easier. An R-project will change the
working directory to the specified project location. For example,
setting `parent_dir` as the R-project directory would set the default
working directory (likely on the C: drive) to `parent_dir` (ex.
[`getwd()`](https://rdrr.io/r/base/getwd.html) would return
`"D:/0.RMC/Reefer/MC_Dam"`).

``` r
parent_dir <- "D:/0.RMC/Reefer/MC_Dam"
```

## Load Data

Currently, the preferred method of data entry for rfaR is copying data
from RMC-RFA into CSVs and saving them to a parent/project directory.
Future development will prioritize loading the data from RFA sqlite
files and less-rigid formatting requirements. The subsections below
provide an example of reading the data and the correct formatting. Data
will be presented as the R-console output as opposed to a formatted
table. Note that R-Studio refers to rows as `obs.` and columns as
`variables`.

### BestFit Parameter Sets

The BestFit parameter sets should be 10,000 rows and 3 columns. Each row
is a set of 3 parameters (for both GEV and LP3 parameter sets). Users
can include a 4th column, log-likelihood, if desired. All of the data is
numeric

### Seasonality

`rfaR` does not have a seasonality analysis module at this time. The
seasonality analysis from RMC-RFA should be copied as is from the
software. The seasonality data should have 12 rows and 4 columns. The
third column, `$relative_frequency` will be used in
[`rfa_simulate()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/rfa_simulate.md)
to sample the seasonality.

### Stage Timeseries

`rfaR` modified the starting stage sampling method from RMC-RFA.
Observed stages are sampled corresponding to the sampled month
(seasonality). Therefore, columns of date and time are required in the
stage timeseries data. Currently,
[`rfa_simulate()`](https://ideal-broccoli-1q9y47z.pages.github.io/reference/rfa_simulate.md)
can only handle date time data in separate columns, as **character**
data. Future development will allow the user to specify if the date &
time data is already a formatted as date
([`as.Date()`](https://rdrr.io/r/base/as.Date.html)) and/or POSIXct
([`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html)) objects.

The empirical stage frequency from RMC-RFA has been loaded below. This
data contains 2 columns of stage and corresponding plotting-position.

### Reservoir Model

The reservoir model is formatted identically to RMC-RFA. The model
should be three columns: stage, storage, and discharge and the stage
must be monotonic.

### Hydrograph Shapes

The hydrographs have been copied directly from RMC-RFA, including the
**Ordinate** column. The

## Set up Hydrograph Shapes

## Expected-Only Stage-Frequency Analysis

## Display Results

The results have been plotted below on a standard `ggplot2` template.
Note that the AEPs of both the results and emprical stage-frequency were
converted to z-variates for plotting purposes.
