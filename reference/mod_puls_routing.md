# Modified Puls Reservoir Routing

Performs Modified Puls (level pool) routing of inflow hydrograph given a
defined reservoir geometry (Stage (ft), Storage (ac-ft), Discharge
(cfs)).

## Usage

``` r
mod_puls_routing(resmodel_df, inflow_df, initial_elev, full_results = FALSE)
```

## Arguments

- resmodel_df:

  Data frame with three columns: elevation/stage (ft), storage
  (acre-feet), and discharge (cfs). Must be in that order.

- inflow_df:

  Data frame with two columns: time (hours) and inflow (cfs). Must be in
  that order.

- initial_elev:

  Starting water surface elevation in feet.

- full_results:

  Logical. If `FALSE` (default), returns only peak stage and discharge.
  If `TRUE`, returns the complete routing result.

## Value

If `full_results = FALSE`, a named numeric vector with `peak_stage_ft`
and `peak_discharge_cfs`. If `full_results = TRUE`, a data frame with
columns: `time_hr`, `inflow_cfs`, `elevation_ft`, `storage_acft`, and
`outflow_cfs`.

## References

Chow, V.T. (1959). Open-Channel Hydraulics. McGraw-Hill.

## Examples

``` r
# Example hydrograph. Requires pre-processing
hydro_example <- hydrograph_setup(jmd_hydro_jun1965_15min, critical_duration = 2, routing_days = 10)
hydrograph_shape <- hydro_example[[1]][, 2:3]
scaled_hydrograph <- scale_hydrograph(hydrograph_shape,
                                      observed_volume = 50000,
                                      sampled_volume = 55000)

# Peak values only
mod_puls_routing(jmd_resmodel, scaled_hydrograph, initial_elev = 3830)
#>      peak_stage_ft peak_discharge_cfs 
#>           3860.329            500.000 

# Full routing table
jmd_full_routing <- mod_puls_routing(jmd_resmodel, scaled_hydrograph, initial_elev = 3830, full_results = TRUE)
head(jmd_full_routing)
#>   time_hr inflow_cfs elevation_ft storage_acft outflow_cfs
#> 1       0     40.150     3830.000     129736.8           0
#> 2       1   1301.025     3830.008     129792.2           0
#> 3       2   1484.725     3830.026     129907.3           0
#> 4       3   1453.650     3830.044     130028.8           0
#> 5       4   1376.650     3830.062     130145.7           0
#> 6       5   1305.975     3830.079     130256.6           0
```
