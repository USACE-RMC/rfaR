# ==============================================================================
# CHERRY CREEK DAM
# ==============================================================================

#' Cherry Creek Dam Reservoir Model
#'
#' Elevation-storage-discharge relationship for Cherry Creek Reservoir. Reservoir models must be monotonic.
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{elev_ft}{Reservoir stage/elevation data (FT-NAVD88)}
#'   \item{stor_acft}{Cumulative reservoir storage (ACRE-FT) corresponding to reservoir elevation (elev_ft).}
#'   \item{outflow_cfs}{Cumulative discharge/outflow (CFS) corresponding to reservoir elevation (elev_ft).}
#' }
"cc_resmodel"

#' Cherry Creek Dam Inflow Hydrograph
#'
#' Inflow hydrograph for an example flood event at Cherry Creek Dam.
#'
#' @format A data frame with 2 columns:
#' \describe{
#'   \item{time_hr}{Time in hours (integer)}
#'   \item{inflow_cfs}{Inflow in cubic-feet per second (CFS)}
#' }
"cc_inflowhydro"

#' Cherry Creek Dam Initial Elevation
#'
#' Initial/starting elevation (FT-NAVD88) for Cherry Creek Dam reservoir routing.
#' This elevation was used in HEC-HMS.
#'
#' @format A single numeric value representing the initial reservoir elevation (FT-NAVD88).
"cc_init_elev"

#' Cherry Creek Dam HEC-HMS Routing Results
#'
#' Routing results from HEC-HMS using the Cherry Creek reservoir model/geometry, inflow hydrograph, and initial reservoir elevation.
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{time_hr}{Time in hours (integer)}
#'   \item{inflow_cfs}{Inflow in cubic-feet per second (CFS)}
#'   \item{elevation_ft}{Routed reservoir stage/elevation data in FT-NAVD88}
#'   \item{stor_acft}{Routed reservoir storage (ACRE-FT) corresponding to reservoir elevation (elev_ft).}
#'   \item{outflow_cfs}{Routed discharge/outflow (CFS) corresponding to reservoir elevation (elev_ft).}
#' }
"cc_hms_results"

# ==============================================================================
# JOHN MARTIN DAM (JMD) CORE EXAMPLE DATA
# ==============================================================================

#' John Martin Dam Reservoir Model
#'
#' Elevation-storage-discharge relationship for John Martin Reservoir (Arkansas River). Reservoir models must be monotonic.
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{elev_ft}{Reservoir stage/elevation data (FT-NAVD88)}
#'   \item{stor_acft}{Cumulative reservoir storage (ACRE-FT) corresponding to reservoir elevation (elev_ft).}
#'   \item{outflow_cfs}{Cumulative discharge/outflow (CFS) corresponding to reservoir elevation (elev_ft).}
#' }
"jmd_resmodel"

#' John Martin Dam HEC-HMS Routing Results
#'
#' Routing results from HEC-HMS using the JMD reservoir model/geometry, inflow hydrograph, and initial reservoir elevation.
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{time_hr}{Time in hours (integer)}
#'   \item{inflow_cfs}{Inflow in cubic-feet per second (CFS)}
#'   \item{elev_ft}{Routed reservoir stage/elevation data in FT-NAVD88}
#'   \item{stor_acft}{Routed reservoir storage (ACRE-FT) corresponding to reservoir elevation (elev_ft).}
#'   \item{outflow_cfs}{Routed discharge/outflow (CFS) corresponding to reservoir elevation (elev_ft).}
#' }
"jmd_hms_results"

#' John Martin Dam Inflow Hydrograph
#'
#' Inflow hydrograph for hypothetical PMF flood event at John Martin Dam.
#'
#' @format A data frame with 2 columns:
#' \describe{
#'   \item{time_hr}{Time in hours (integer)}
#'   \item{inflow_cfs}{Inflow in cubic-feet per second (CFS)}
#' }
"jmd_inflowhydro"

#' John Martin Dam Initial Elevation
#'
#' Initial/starting elevation (FT-NAVD88) for JMD reservoir routing.
#' This elevation was used in HEC-HMS.
#'
#' @format A single numeric value representing the initial reservoir elevation (FT-NAVD88).
"jmd_init_elev"

# ==============================================================================
# JOHN MARTIN DAM - DISCHARGE GAGE
# ==============================================================================

#' John Martin Dam Period of Record Inflow
#'
#' Daily inflow at John Martin Dam for the available period of record (1943-2024).
#'
#' @format A data frame with 40,908 rows and 4 columns:
#' \describe{
#'   \item{timestep}{Timestep index}
#'   \item{date}{Date}
#'   \item{time}{Time}
#'   \item{flow_cfs}{Daily average inflow (cfs)}
#' }
"jmd_por_inflow"

# ==============================================================================
# JOHN MARTIN DAM - EMPIRICAL FREQUENCY
# ==============================================================================

#' John Martin Dam Empirical Stage-Frequency (Full POR)
#'
#' Empirical stage-frequency results for John Martin Dam full period of record.
#'
#' @format A data frame with 82 rows and 3 columns:
#' \describe{
#'   \item{wy}{Water year}
#'   \item{stage_ft}{Stage (FT-NAVD88)}
#'   \item{plot_posit}{Plotting position}
#' }
"jmd_empirical_stage_por"

#' John Martin Dam Empirical Stage-Frequency (WY 1980-2024)
#'
#' Empirical stage-frequency results for John Martin Dam, water years 1980-2024.
#'
#' @format A data frame with 45 rows and 3 columns:
#' \describe{
#'   \item{wy}{Water year}
#'   \item{stage_ft}{Stage (FT-NAVD88)}
#'   \item{plot_posit}{Plotting position}
#' }
"jmd_empirical_stage_wy1980"

#' John Martin Dam Empirical Stage-Frequency with Perception Threshold
#'
#' Empirical stage-frequency results for John Martin Dam (WY 1980-2024) with
#' perception threshold applied.
#'
#' @format A data frame with 54 rows and 3 columns:
#' \describe{
#'   \item{wy}{Water year}
#'   \item{stage_ft}{Stage (FT-NAVD88)}
#'   \item{plot_posit}{Plotting position}
#' }
"jmd_empirical_stage_wy1980_pt"

# ==============================================================================
# JOHN MARTIN DAM - HYDROGRAPHS
# ==============================================================================

#' John Martin Dam April 1999 Hydrograph
#'
#' Inflow hydrograph shape from April 1999 flood event.
#'
#' @format A data frame with 241 rows and 4 columns:
#' \describe{
#'   \item{Ordinate}{Timeseries Ordinate}
#'   \item{Date}{Date mm/dd/yyyy}
#'   \item{Time}{Time in 00:00}
#'   \item{Flow}{Inflow (cfs)}
#' }
"jmd_hydro_apr1999"

#' John Martin Dam June 1921 Hydrograph
#'
#' Inflow hydrograph shape from June 1921 flood event.
#'
#' @format A data frame with 169 rows and 4 columns:
#' \describe{
#'   \item{Ordinate}{Timeseries Ordinate}
#'   \item{Date}{Date mm/dd/yyyy}
#'   \item{Time}{Time in 00:00}
#'   \item{Flow}{Inflow (cfs)}
#' }
"jmd_hydro_jun1921"

#' John Martin Dam June 1965 Hydrograph
#'
#' Inflow hydrograph shape from June 1965 flood event (from HEC-HMS).
#'
#' @format A data frame with 241 rows and 4 columns:
#' \describe{
#'   \item{Ordinate}{Timeseries Ordinate}
#'   \item{Date}{Date mm/dd/yyyy}
#'   \item{Time}{Time in 00:00}
#'   \item{Flow}{Inflow (cfs)}
#' }
"jmd_hydro_jun1965"

#' John Martin Dam June 1965 Hydrograph (15 minute intervals)
#'
#' Inflow hydrograph shape from June 1965 flood event (from HEC-HMS).
#'
#' @format A data frame with 481 rows and 4 columns:
#' \describe{
#'   \item{Ordinate}{Timeseries Ordinate}
#'   \item{Date}{Date mm/dd/yyyy}
#'   \item{Time}{Time in 00:00}
#'   \item{Flow}{Inflow (cfs)}
#' }
"jmd_hydro_jun1965_15min"

#' John Martin Dam May 1955 Hydrograph
#'
#' Inflow hydrograph shape from May 1955 flood event.
#'
#' @format A data frame with 241 rows and 2 columns:
#' \describe{
#'   \item{Ordinate}{Timeseries Ordinate}
#'   \item{Date}{Date mm/dd/yyyy}
#'   \item{Time}{Time in 00:00}
#'   \item{Flow}{Inflow (cfs)}
#' }
"jmd_hydro_may1955"

#' John Martin Dam PMF Hydrograph
#'
#' Inflow hydrograph shape for Probable Maximum Flood (PMF).
#'
#' @format A data frame with 192 rows and 2 columns:
#' \describe{
#'   \item{Ordinate}{Timeseries Ordinate}
#'   \item{Date}{Date mm/dd/yyyy}
#'   \item{Time}{Time in 00:00}
#'   \item{Flow}{Inflow (cfs)}
#' }
"jmd_hydro_pmf"

#' John Martin Dam SDF Hydrograph
#'
#' Inflow hydrograph shape for Spillway Design Flood (SDF).
#'
#' @format A data frame with 337 rows and 2 columns:
#' \describe{
#'   \item{Ordinate}{Timeseries Ordinate}
#'   \item{Date}{Date mm/dd/yyyy}
#'   \item{Time}{Time in 00:00}
#'   \item{Flow}{Inflow (cfs)}
#' }
"jmd_hydro_sdf"

# ==============================================================================
# JOHN MARTIN DAM - SEASONALITY
# ==============================================================================

#' John Martin Dam Flood Seasonality
#'
#' Results of flood seasonality analysis from the full John Martin Dam period
#' of record. Determined using: threshold flow of 5,000 cfs, critical duration
#' of 2 days, max events per year of 5, minimum days between events of 7.
#'
#' @format A data frame with 12 rows and 4 columns:
#' \describe{
#'   \item{month}{Month (1-12)}
#'   \item{frequency}{Event frequency}
#'   \item{relative_frequency}{Relative frequency}
#'   \item{cume_rel_frequency}{Cumulative relative frequency}
#' }
"jmd_seasonality"

# ==============================================================================
# JOHN MARTIN DAM - STAGE DURATION
# ==============================================================================

#' John Martin Dam Monthly Stage Duration Curves
#'
#' Monthly stage duration curves from Water Year 1980-2024.
#'
#' @format A data frame with 45 rows and 13 columns:
#' \describe{
#'   \item{Probability}{Probability of exceedance}
#'   \item{January}{Stage for January (FT-NAVD88)}
#'   \item{February}{Stage for February (FT-NAVD88)}
#'   \item{March}{Stage for March (FT-NAVD88)}
#'   \item{April}{Stage for April (FT-NAVD88)}
#'   \item{May}{Stage for May (FT-NAVD88)}
#'   \item{June}{Stage for June (FT-NAVD88)}
#'   \item{July}{Stage for July (FT-NAVD88)}
#'   \item{August}{Stage for August (FT-NAVD88)}
#'   \item{September}{Stage for September (FT-NAVD88)}
#'   \item{October}{Stage for October (FT-NAVD88)}
#'   \item{November}{Stage for November (FT-NAVD88)}
#'   \item{December}{Stage for December (FT-NAVD88)}
#' }
"jmd_stage_duration"

# ==============================================================================
# JOHN MARTIN DAM - STAGE GAGE
# ==============================================================================

#' John Martin Dam Period of Record Stage
#'
#' Daily stage data from the full John Martin Dam period of record (1943-2024).
#'
#' @format A data frame with 29,858 rows and 4 columns:
#' \describe{
#'   \item{timestep}{Timestep index}
#'   \item{date}{Date}
#'   \item{time}{Time}
#'   \item{stage_ft}{Reservoir stage (FT-NAVD88)}
#' }
"jmd_por_stage"

#' John Martin Dam Stage (WY 1980-2024)
#'
#' Daily stage data from Water Year 1980-2024.
#'
#' @format A data frame with 29,858 rows and 4 columns:
#' \describe{
#'   \item{timestep}{Timestep index}
#'   \item{date}{Date}
#'   \item{time}{Time}
#'   \item{stage_ft}{Reservoir stage (FT-NAVD88)}
#' }
"jmd_wy1980_stage"

# ==============================================================================
# JOHN MARTIN DAM - VOLUME FREQUENCY CURVE
# ==============================================================================

#' John Martin Dam VFC Parameters
#'
#' LP3 distribution parameters of volume-frequency curve results from
#' RMC-BestFit 2.0.
#'
#' @format A data frame with 1 row and 5 columns:
#' \describe{
#'   \item{mean_log}{Mean of log-transformed values}
#'   \item{sd_log}{Standard deviation of log-transformed values}
#'   \item{skew_log}{Skewness of log-transformed values}
#'   \item{erl}{Estimated effective record length}
#'   \item{duration}{Inflow-volume duration in days}
#' }
"jmd_vfc_parameters"

#' John Martin Dam Volume-Frequency Curve
#'
#' Tabular volume-frequency curve results from RMC-BestFit 2.0.
#'
#' @format A data frame with 25 rows and 5 columns:
#' \describe{
#'   \item{aep}{Annual exceedance probability}
#'   \item{ci_95}{95% credible interval}
#'   \item{ci_5}{5% credible interval}
#'   \item{posterior_predictive}{Posterior predictive value}
#'   \item{posterior_mode}{Posterior mode value}
#' }
"jmd_vfc"

#' John Martin Dam BestFit Parameter Sets
#'
#' LP3 distribution parameters sets of volume-frequency curve results from
#' RMC-BestFit 2.0.
#'
#' @format A data frame with 10000 row and 3 columns:
#' \describe{
#'   \item{mean_log}{Mean of log-transformed values}
#'   \item{sd_log}{Standard deviation of log-transformed values}
#'   \item{skew_log}{Skewness of log-transformed values}
#' }
"jmd_bf_parameter_sets"

#' John Martin Dam RFA Results - Expected Only
#'
#' John Martin Dam, expected only stage-frequency results from RFA.
#'
#' @format A data frame with 10000 row and 3 columns:
#' \describe{
#'   \item{AEP}{Annual Exceedace Probabilities}
#'   \item{Expected}{Expected stages corresponding to AEPs}
#' }
"jmd_rfa_expected"

#' John Martin Dam RFA Results - Full Uncertainty
#'
#' John Martin Dam, expected only stage-frequency results from RFA.
#'
#' @format A data frame with 10000 row and 3 columns:
#' \describe{
#'   \item{AEP}{Annual Exceedace Probabilities}
#'   \item{Upper}{Annual Exceedace Probabilities}
#'   \item{Lower}{Annual Exceedace Probabilities}
#'   \item{Expected}{Expected stages corresponding to AEPs}
#'   \item{Median}{Median stages corresponding to AEPs}
#' }
"jmd_rfa_full"

