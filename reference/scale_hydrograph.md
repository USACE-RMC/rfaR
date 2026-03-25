# Scale Hydrograph

Scales inflow hydrograph given a sampled inflow volume from the
volume-frequency curve. Scale factor is defined by the sampled inflow
volume and the maximum volume of the corresponding duration. If the
native hydrograph timestep differs from the target routing timestep,
resampling is applied prior to scaling: finer-to-coarser uses block
averaging; coarser-to-finer uses linear interpolation.

## Usage

``` r
scale_hydrograph(
  hydrograph_shape,
  observed_volume,
  sampled_volume,
  routing_dt = 1
)
```

## Arguments

- hydrograph_shape:

  Data frame with two columns: time (hours) and inflow (cfs). Must be in
  that order.

- observed_volume:

  Maximum n-day inflow volume from input hydrograph shape.

- sampled_volume:

  Sampled inflow volume from volume-frequency curve.

- routing_dt:

  Target routing timestep in hours. Supported values are `0.25`
  (15-min), `1` (1-hour, default), `6` (6-hour), and `24` (24-hour).

## Value

A data frame with two columns: `time_hrs` and `inflow_cfs`, at the
target routing timestep, with time starting at hour 0.

## Examples

``` r
# Example hydrograph. Requires pre-processing
hydro_example <- hydrograph_setup(jmd_hydro_jun1965_15min, critical_duration = 2, routing_days = 10)
hydrograph_shape <- hydro_example[[1]][, 2:3]

# Default 1-hour routing timestep
scaled <- scale_hydrograph(hydrograph_shape,
                           observed_volume = 50000,
                           sampled_volume = 55000)

# 15-min input hydrograph, 1-hour routing timestep
scaled <- scale_hydrograph(hydrograph_shape,
                           observed_volume = 50000,
                           sampled_volume = 55000,
                           routing_dt = 1)
```
