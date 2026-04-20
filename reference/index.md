# Package index

## Execute Reservoir Frequency Analysis

Top-level function that uses primary modules to conduct
reservoir-frequency analysis

- [`rfa_simulate()`](https://usace-rmc.github.io/rfaR/reference/rfa_simulate.md)
  : RFA Stage-Frequency Simulation

## Primary Reservoir Frequency Analysis Modules

Core functions for reservoir frequency analysis

- [`stratified_sampler()`](https://usace-rmc.github.io/rfaR/reference/stratified_sampler.md)
  : Stratified Sampler
- [`flow_frequency_sampler()`](https://usace-rmc.github.io/rfaR/reference/flow_frequency_sampler.md)
  : Flow Frequency Sampler
- [`flow_frequency_sampler_expected()`](https://usace-rmc.github.io/rfaR/reference/flow_frequency_sampler_expected.md)
  : Flow Frequency Sampler (Expected Only)
- [`hydrograph_setup()`](https://usace-rmc.github.io/rfaR/reference/hydrograph_setup.md)
  : Hydrograph Setup for RFA Simulation
- [`scale_hydrograph()`](https://usace-rmc.github.io/rfaR/reference/scale_hydrograph.md)
  : Scale Hydrograph
- [`mod_puls_routing()`](https://usace-rmc.github.io/rfaR/reference/mod_puls_routing.md)
  : Modified Puls Reservoir Routing
- [`stage_frequency_curve()`](https://usace-rmc.github.io/rfaR/reference/stage_frequency_curve.md)
  : Compute Stage-Frequency Curve

## Additional Utilities

Supporting utility functions

- [`bootstrap_vfc()`](https://usace-rmc.github.io/rfaR/reference/bootstrap_vfc.md)
  : Bootstrap option for VFC (RFA-style w/ ERL)
- [`qp3()`](https://usace-rmc.github.io/rfaR/reference/qp3.md) : Pearson
  Type III Inverse CDF (Quantile Function)
- [`aep2stage()`](https://usace-rmc.github.io/rfaR/reference/aep2stage.md)
  : Interpolate Stage from AEP
- [`stage2aep()`](https://usace-rmc.github.io/rfaR/reference/stage2aep.md)
  : Interpolate AEP from Stage
- [`interpolate_aep_matrix()`](https://usace-rmc.github.io/rfaR/reference/interpolate_aep_matrix.md)
  : Interpolate AEPs at Target Stages Across Realizations
- [`interpolate_stage_matrix()`](https://usace-rmc.github.io/rfaR/reference/interpolate_stage_matrix.md)
  : Interpolate Stages at Target AEPs Across Realizations
- [`power_function()`](https://usace-rmc.github.io/rfaR/reference/power_function.md)
  : Power Function
- [`theme_rfar_conceptual()`](https://usace-rmc.github.io/rfaR/reference/theme_rfar_conceptual.md)
  : rfaR ggplot theme for conceptual example

## Datasets

Example datasets included with rfaR

- [`jmd_bf_parameter_sets`](https://usace-rmc.github.io/rfaR/reference/jmd_bf_parameter_sets.md)
  : JMD BestFit Parameter Sets
- [`jmd_empirical_stage_wy1980_pt`](https://usace-rmc.github.io/rfaR/reference/jmd_empirical_stage_wy1980_pt.md)
  : JMD Empirical Stage-Frequency with Perception Threshold
- [`jmd_hydro_apr1999`](https://usace-rmc.github.io/rfaR/reference/jmd_hydro_apr1999.md)
  : JMD April 1999 Hydrograph
- [`jmd_hydro_jun1921`](https://usace-rmc.github.io/rfaR/reference/jmd_hydro_jun1921.md)
  : JMD June 1921 Hydrograph
- [`jmd_hydro_jun1965`](https://usace-rmc.github.io/rfaR/reference/jmd_hydro_jun1965.md)
  : JMD June 1965 Hydrograph
- [`jmd_hydro_jun1965_15min`](https://usace-rmc.github.io/rfaR/reference/jmd_hydro_jun1965_15min.md)
  : JMD June 1965 Hydrograph (15 minute intervals)
- [`jmd_hydro_may1955`](https://usace-rmc.github.io/rfaR/reference/jmd_hydro_may1955.md)
  : JMD May 1955 Hydrograph
- [`jmd_hydro_pmf`](https://usace-rmc.github.io/rfaR/reference/jmd_hydro_pmf.md)
  : JMD PMF Hydrograph
- [`jmd_hydro_sdf`](https://usace-rmc.github.io/rfaR/reference/jmd_hydro_sdf.md)
  : JMD SDF Hydrograph
- [`jmd_por_inflow`](https://usace-rmc.github.io/rfaR/reference/jmd_por_inflow.md)
  : JMD Period of Record Inflow
- [`jmd_resmodel`](https://usace-rmc.github.io/rfaR/reference/jmd_resmodel.md)
  : JMD Reservoir Model
- [`jmd_rfa_expected`](https://usace-rmc.github.io/rfaR/reference/jmd_rfa_expected.md)
  : JMD RFA Results - Expected Only
- [`jmd_rfa_full`](https://usace-rmc.github.io/rfaR/reference/jmd_rfa_full.md)
  : JMD RFA Results - Full Uncertainty
- [`jmd_seasonality`](https://usace-rmc.github.io/rfaR/reference/jmd_seasonality.md)
  : JMD Flood Seasonality
- [`jmd_stage_duration`](https://usace-rmc.github.io/rfaR/reference/jmd_stage_duration.md)
  : JMD Monthly Stage Duration Curves
- [`jmd_vfc`](https://usace-rmc.github.io/rfaR/reference/jmd_vfc.md) :
  JMD Volume-Frequency Curve
- [`jmd_vfc_parameters`](https://usace-rmc.github.io/rfaR/reference/jmd_vfc_parameters.md)
  : JMD VFC Parameters
- [`jmd_wy1980_stage`](https://usace-rmc.github.io/rfaR/reference/jmd_wy1980_stage.md)
  : JMD Stage (WY 1980-2024)
- [`cc_hms_results`](https://usace-rmc.github.io/rfaR/reference/cc_hms_results.md)
  : Cherry Cricket Dam HEC-HMS Routing Results
- [`cc_inflowhydro`](https://usace-rmc.github.io/rfaR/reference/cc_inflowhydro.md)
  : Cherry Cricket Dam Inflow Hydrograph
- [`cc_init_elev`](https://usace-rmc.github.io/rfaR/reference/cc_init_elev.md)
  : Cherry Cricket Dam Initial Elevation
- [`cc_resmodel`](https://usace-rmc.github.io/rfaR/reference/cc_resmodel.md)
  : Cherry Cricket Dam Reservoir Model
- [`example_stratified`](https://usace-rmc.github.io/rfaR/reference/example_stratified.md)
  : Stratified Sampling - Example Data
