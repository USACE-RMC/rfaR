# RFA Stage-Frequency Simulation

Performs reservoir stage frequency analysis using Monte Carlo simulation
to develop stage-frequency relationships. Combines stratified
flow-frequency sampling, flood seasonality, hydrograph scaling, and
Modified Puls reservoir routing to estimate annual exceedance
probabilities of reservoir stage.

## Usage

``` r
rfa_simulate(
  sim_type = "expected",
  bestfit_params,
  dist = "LP3",
  stage_ts,
  seasonality,
  hydrographs,
  resmodel,
  Nbins = 50,
  events_per_bin = 200,
  routing_dt = 1,
  Ncores = NULL,
  sim_name = NULL,
  results_dir = NULL
)
```

## Arguments

- sim_type:

  Character string specifying the simulation mode. One of `"median"`,
  `"expected"`, or `"full"`. Default is `"expected"`.

- bestfit_params:

  Data frame or matrix of distribution parameters from RMC-BestFit MCMC
  output. Columns 1–3 are distribution parameters:

  - LP3: mean (log), standard deviation (log), skew (log)

  - GEV: location, scale, shape

  Column 4 is log-likelihood (used only in `"median"` mode to identify
  the posterior mode). For `"expected"` mode, must have at least
  `Nbins * events_per_bin` rows. For `"full"` mode, each row is treated
  as an independent realization.

- dist:

  Character string specifying the frequency distribution. Either `"LP3"`
  (Log-Pearson Type III, default) or `"GEV"` (Generalized Extreme
  Value).

- stage_ts:

  Data frame of historical reservoir stage with columns `date`
  (character in M/D/YYYY format) and `stage` (numeric, feet).

- seasonality:

  Numeric vector of length 12 giving monthly flood occurrence
  probabilities (relative frequencies). Used to sample the month of each
  simulated event, which determines the antecedent pool elevation.

- hydrographs:

  List of hydrograph data frames as returned by
  [`hydrograph_setup`](https://usace-rmc.github.io/rfaR/reference/hydrograph_setup.md).
  Each element has columns `datetime`, `hour`, `inflow`, and
  `hydrograph_num`, with attributes `"obs_vol"` (observed max n-day
  volume) and `"dt"` (timestep in hours). The list must have a `"probs"`
  attribute containing normalized sampling probabilities.

- resmodel:

  Data frame with three columns: elevation (ft), storage (acre-ft), and
  discharge (cfs). Defines the reservoir model for Modified Puls
  routing.

- Nbins:

  Integer. Number of stratified sampling bins in EV1 space. Default is
  50.

- events_per_bin:

  Integer. Number of events sampled per bin. Default is 200. Total
  simulations per realization = `Nbins * events_per_bin`.

- routing_dt:

  Numeric. Routing timestep in hours. Passed to
  [`scale_hydrograph`](https://usace-rmc.github.io/rfaR/reference/scale_hydrograph.md)
  to resample hydrographs before routing. Supported values are `0.25`
  (15-min), `1` (1-hour, default), `6` (6-hour), and `24` (24-hour).

- Ncores:

  Integer or `NULL`. Number of parallel workers for `"full"` mode. If
  `NULL` (default), automatically selects based on available cores
  (capped at 16). Ignored for `"median"` and `"expected"` modes.

- sim_name:

  Optional character string to label the simulation. Used in the output
  CSV filename. If `NULL` (default), defaults to `"sim"`. Spaces are
  replaced with underscores in the filename.

- results_dir:

  Optional path to the directory where results are saved. If `NULL`
  (default), creates an `rfaR_results` folder in the current working
  directory.

## Value

A list whose contents depend on `sim_type`:

For `"median"` and `"expected"`:

- stage_frequency:

  Data frame with columns `stage` and `AEP` from
  [`stage_frequency_curve`](https://usace-rmc.github.io/rfaR/reference/stage_frequency_curve.md).

- peakStage:

  Matrix of simulated peak stages (`events_per_bin` rows by `Nbins`
  columns).

- peakFlow:

  Matrix of simulated peak discharges (same dimensions).

- weights:

  Stratified sampling bin weights.

For `"full"`:

- stage_frequency:

  Data frame with columns `stage`, `expected`, `median`, `lower_05`, and
  `upper_95` on a common stage grid.

- all_curves:

  List of per-realization stage-frequency data frames (each with `stage`
  and `AEP` columns).

- aep_matrix:

  Matrix of interpolated AEP values (500 rows by `Nrealizations`
  columns) on the common stage grid.

- common_stage:

  Numeric vector of the common stage grid (length 500).

- Nrealizations:

  Number of parameter set realizations processed.

- Nsims_per_realiz:

  Number of simulations per realization.

## Details

Three simulation modes are available:

- `"median"`:

  Single realization using the most likely (posterior mode) parameter
  set from RMC-BestFit. Produces one stage-frequency curve with no
  uncertainty bounds. Useful for quick screening or debugging.

- `"expected"`:

  Single realization using expected value sampling, where each
  stratified flow sample is paired 1-to-1 with a different parameter set
  from the posterior distribution. Produces one stage-frequency curve
  representing the expected (mean) result integrated across parameter
  uncertainty.

- `"full"`:

  Nested Monte Carlo with full uncertainty quantification. The outer
  loop iterates over all parameter sets (one per row of
  `bestfit_params`), and each inner loop independently samples natural
  variability via stratified sampling, seasonality, starting pool, and
  hydrograph shape. Each realization produces an independent
  stage-frequency curve; curves are combined on a common stage grid to
  yield expected, median, and 5th/95th percentile confidence bounds. The
  outer loop is parallelized using the future framework.

For all modes, natural variability is captured through stratified
sampling in with `Nbins` bins and `events_per_bin` events per bin,
providing reliable coverage from common events down to approximately
1e-8 AEP.

The stratified sampling approach divides the AEP range (default 0.99 to
1e-8) into equal-width bins in EV1 (Gumbel reduced variate) space and
samples events uniformly within each bin. This ensures adequate
representation of rare flood events that would require orders of
magnitude more samples under crude Monte Carlo sampling.

Post-processing uses the law of total probability via
[`stage_frequency_curve`](https://usace-rmc.github.io/rfaR/reference/stage_frequency_curve.md):
for a grid of stage thresholds, the weighted exceedance fraction is
computed within each bin and summed across bins.

In `"full"` mode, each parallel worker independently samples all random
inputs (seasonality, starting pool, hydrograph shape, and stratified
z-variates) in addition to using its own parameter set. This ensures
full Monte Carlo independence across realizations. Each worker
post-processes its results into a stage-frequency curve immediately,
returning only the curve rather than the full peak stage matrix, keeping
memory usage manageable for large numbers of realizations.

Curves from individual realizations are combined by interpolating each
onto a common stage grid (spanning the global min/max across all
realizations) in log10(AEP) space, then computing summary statistics
across realizations at each stage threshold.

Results are automatically exported as a CSV file named
`{sim_name}_{sim_type}_{MM_DD_YY_HHMM}.csv` in the `results_dir`
directory. For example, a median simulation named "John McGraw Dam" run
on June 4, 2025 at 2:30 PM would produce
`John_McGraw_Dam_median_06_04_25_1430.csv`.

## See also

[`flow_frequency_sampler`](https://usace-rmc.github.io/rfaR/reference/flow_frequency_sampler.md),
[`flow_frequency_sampler_expected`](https://usace-rmc.github.io/rfaR/reference/flow_frequency_sampler_expected.md),
[`stratified_sampler`](https://usace-rmc.github.io/rfaR/reference/stratified_sampler.md),
[`scale_hydrograph`](https://usace-rmc.github.io/rfaR/reference/scale_hydrograph.md),
[`mod_puls_routing`](https://usace-rmc.github.io/rfaR/reference/mod_puls_routing.md),
[`stage_frequency_curve`](https://usace-rmc.github.io/rfaR/reference/stage_frequency_curve.md),
[`hydrograph_setup`](https://usace-rmc.github.io/rfaR/reference/hydrograph_setup.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# --- Setup ---
hydros <- hydrograph_setup(jmd_hydro_apr1999, jmd_hydro_jun1965,
                           jmd_hydro_may1955, jmd_hydro_pmf,
                           critical_duration = 2, routing_days = 10)

# --- Expected only (default) ---
results_exp <- rfa_simulate(
  sim_type       = "expected",
  bestfit_params = jmd_vfc_parameters,
  stage_ts       = jmd_wy1980_stage,
  seasonality    = jmd_seasonality$relative_frequency,
  hydrographs    = hydros,
  resmodel       = jmd_resmodel,
  sim_name       = "jmd"
)

# --- Median only ---
results_med <- rfa_simulate(
  sim_type       = "median",
  bestfit_params = jmd_vfc_parameters,
  stage_ts       = jmd_wy1980_stage,
  seasonality    = jmd_seasonality$relative_frequency,
  hydrographs    = hydros,
  resmodel       = jmd_resmodel,
  sim_name       = "jmd"
)

# --- Full uncertainty (parallelized) ---
results_full <- rfa_simulate(
  sim_type       = "full",
  bestfit_params = jmd_vfc_parameters,
  stage_ts       = jmd_wy1980_stage,
  seasonality    = jmd_seasonality$relative_frequency,
  hydrographs    = hydros,
  resmodel       = jmd_resmodel,
  Ncores         = 4,
  sim_name       = "jmd"
)

} # }
```
