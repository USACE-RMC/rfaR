# Hydrograph Setup for RFA Simulation

Prepares hydrograph data frames copied from RMC-RFA for use in RFA
simulation. Converts date/time columns and adds sequential hour and
hydrograph ID columns.

## Usage

``` r
hydrograph_setup(
  ...,
  critical_duration = NULL,
  routing_days = NULL,
  weights = NULL
)
```

## Arguments

- ...:

  Data frame representing an input hydrograph with columns: Ord, Date,
  Time, Flow (cfs). Copied directly from RMC-RFA. Date & Time should be
  `class() = "character"`

- critical_duration:

  Critical duration in days.

- routing_days:

  Desired length of routing simulation in days.

- weights:

  Optional numeric vector of sampling weights for each hydrograph. Must
  be the same length as the number of input hydrographs. Weights are
  normalized to probabilities internally. If NULL (default), all
  hydrographs are weighted equally.

## Value

A list of formatted hydrograph data frames, each containing:

- datetime:

  Date-time (POSIXct)

- hour:

  Hours from start of event

- inflow:

  Inflow (cfs)

- hydrograph_num:

  Hydrograph ID number for sampling

- obs_vol:

  Max n-day inflow volume (stored as an attribute of each dataframe)

- dt:

  Hydrograph timestep (delta time, dt) in hours (stored as an attribute
  of each dataframe)

The returned list also has a `probs` attribute containing the normalized
sampling probabilities derived from `weights`.

## Examples

``` r
# Setup with equal weights (default)
hydros <- hydrograph_setup(jmd_hydro_apr1999,
                           jmd_hydro_jun1965,
                           jmd_hydro_pmf,
                           critical_duration = 2,
                           routing_days = 10)

# Setup with custom weights (PMF 3x more likely to be sampled)
hydros <- hydrograph_setup(jmd_hydro_apr1999,
                           jmd_hydro_jun1965,
                           jmd_hydro_pmf,
                           critical_duration = 2,
                           routing_days = 10,
                           weights = c(1, 1, 3))

# view normalized probabilities
attr(hydros, "probs")
#> [1] 0.2 0.2 0.6
```
