# ==============================================================================
# CHERRY CRICKET DAM
# ==============================================================================
#' Cherry Cricket Dam Reservoir Model
#'
#' Elevation-storage-discharge relationship for Cherry Cricket Reservoir. Reservoir models must be monotonic.
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{elev_ft}{Reservoir stage/elevation data (FT-NAVD88)}
#'   \item{stor_acft}{Cumulative reservoir storage (ACRE-FT) corresponding to reservoir elevation (elev_ft).}
#'   \item{outflow_cfs}{Cumulative discharge/outflow (CFS) corresponding to reservoir elevation (elev_ft).}
#' }
#' @examples
#' sapply(cc_resmodel,class)
#' head(cc_resmodel)
"cc_resmodel"

#' Cherry Cricket Dam Inflow Hydrograph
#'
#' Inflow hydrograph for an example flood event at Cherry Cricket Dam.
#'
#' @format A data frame with 2 columns:
#' \describe{
#'   \item{time_hr}{Time in hours (integer)}
#'   \item{inflow_cfs}{Inflow in cubic-feet per second (CFS)}
#' }
#' @examples
#' sapply(cc_inflowhydro,class)
#' head(cc_inflowhydro)
"cc_inflowhydro"

#' Cherry Cricket Dam Initial Elevation
#'
#' Initial/starting elevation (FT-NAVD88) for Cherry Cricket Dam reservoir routing.
#' This elevation was used in HEC-HMS.
#'
#' @format A single numeric value representing the initial reservoir elevation (FT-NAVD88).
#' @examples
#' sapply(cc_init_elev,class)
#' head(cc_init_elev)
"cc_init_elev"

#' Cherry Cricket Dam HEC-HMS Routing Results
#'
#' Routing results from HEC-HMS using the Cherry Cricket reservoir model/geometry, inflow hydrograph, and initial reservoir elevation.
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{time_hr}{Time in hours (integer)}
#'   \item{inflow_cfs}{Inflow in cubic-feet per second (CFS)}
#'   \item{elevation_ft}{Routed reservoir stage/elevation data in FT-NAVD88}
#'   \item{storage_acft}{Routed reservoir storage (ACRE-FT) corresponding to reservoir elevation (elev_ft).}
#'   \item{outflow_cfs}{Routed discharge/outflow (CFS) corresponding to reservoir elevation (elev_ft).}
#' }
#' @examples
#' head(cc_hms_results)
"cc_hms_results"

# ==============================================================================
# John McGraw DAM (JMD) CORE EXAMPLE DATA
# ==============================================================================

#' JMD Reservoir Model
#'
#' Elevation-storage-discharge relationship for John McGraw Reservoir. Reservoir models must be monotonic.
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{stage_ft}{Reservoir stage/elevation data (FT-NAVD88)}
#'   \item{stor_acft}{Cumulative reservoir storage (ACRE-FT) corresponding to reservoir elevation (elev_ft).}
#'   \item{discharge_cfs}{Cumulative discharge/outflow (CFS) corresponding to reservoir elevation (elev_ft).}
#' }
#' @examples
#' sapply(jmd_resmodel,class)
#' head(jmd_resmodel)
#' plot(jmd_resmodel[[1]], jmd_resmodel[[2]],
#'      xlab = "Elevation (ft)", ylab = "Storage (acre-ft)",
#'      type = "l")
"jmd_resmodel"

# ==============================================================================
# John McGraw DAM - DISCHARGE GAGE
# ==============================================================================

#' JMD Period of Record Inflow
#'
#' Daily inflow at JMD for the available period of record.
#'
#' @format A data frame with 40,908 rows and 4 columns:
#' \describe{
#'   \item{timestep}{Timestep index}
#'   \item{date}{Date}
#'   \item{time}{Time}
#'   \item{flow_cfs}{Daily average inflow (cfs)}
#' }
#' @examples
#' sapply(jmd_por_inflow,class)
#' head(jmd_por_inflow)
#' plot(as.Date(jmd_por_inflow$date,tryFormats = c("%m/%d/%Y")), jmd_por_inflow$flow_cfs,
#'       xlab = "Year", ylab = "Inflow (cfs)",
#'       type = "l")
"jmd_por_inflow"

# ==============================================================================
# John McGraw DAM - EMPIRICAL FREQUENCY
# ==============================================================================

#' JMD Empirical Stage-Frequency with Perception Threshold
#'
#' Empirical stage-frequency results for JMD (WY 1980-2024) with
#' perception threshold applied.
#'
#' @format A data frame with 54 rows and 3 columns:
#' \describe{
#'   \item{wy}{Water year}
#'   \item{stage_ft}{Stage (FT-NAVD88)}
#'   \item{plot_posit}{Plotting position}
#' }
#' @examples
#' sapply(jmd_empirical_stage_wy1980_pt,class)
#' head(jmd_empirical_stage_wy1980_pt)
"jmd_empirical_stage_wy1980_pt"

# ==============================================================================
# John McGraw DAM - HYDROGRAPHS
# ==============================================================================

#' JMD April 1999 Hydrograph
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
#' @examples
#' sapply(jmd_hydro_apr1999,class)
#' head(jmd_hydro_apr1999)
#' plot(jmd_hydro_apr1999$Ordinate, jmd_hydro_apr1999$Flow,
#'      xlab = "Hour", ylab = "Inflow (cfs)",
#'           type = "l")
"jmd_hydro_apr1999"

#' JMD June 1921 Hydrograph
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
#' @examples
#' sapply(jmd_hydro_jun1921,class)
#' head(jmd_hydro_jun1921)
#' plot(jmd_hydro_jun1921$Ordinate, jmd_hydro_jun1921$Flow,
#'      xlab = "Hour", ylab = "Inflow (cfs)",
#'      type = "l")
"jmd_hydro_jun1921"

#' JMD June 1965 Hydrograph
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
#' @examples
#' sapply(jmd_hydro_jun1965,class)
#' head(jmd_hydro_jun1965)
#' plot(jmd_hydro_jun1965$Ordinate, jmd_hydro_jun1965$Flow,
#'      xlab = "Hour", ylab = "Inflow (cfs)",
#'      type = "l")
"jmd_hydro_jun1965"

#' JMD June 1965 Hydrograph (15 minute intervals)
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
#' @examples
#' sapply(jmd_hydro_jun1965_15min,class)
#' head(jmd_hydro_jun1965_15min)
#' plot(jmd_hydro_jun1965_15min$Ordinate/4, jmd_hydro_jun1965_15min$Flow,
#'      xlab = "Hour", ylab = "Inflow (cfs)",
#'      type = "l")
"jmd_hydro_jun1965_15min"

#' JMD May 1955 Hydrograph
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
#' @examples
#' sapply(jmd_hydro_may1955,class)
#' head(jmd_hydro_may1955)
#' plot(jmd_hydro_may1955$Ordinate, jmd_hydro_may1955$Flow,
#'      xlab = "Hour", ylab = "Inflow (cfs)",
#'      type = "l")
"jmd_hydro_may1955"

#' JMD PMF Hydrograph
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
#' @examples
#' sapply(jmd_hydro_pmf,class)
#' head(jmd_hydro_pmf)
#' plot(jmd_hydro_pmf$Ordinate, jmd_hydro_pmf$Flow,
#'      xlab = "Hour", ylab = "Inflow (cfs)",
#'      type = "l")
"jmd_hydro_pmf"

#' JMD SDF Hydrograph
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
#' @examples
#' sapply(jmd_hydro_sdf,class)
#' head(jmd_hydro_sdf)
#' plot(jmd_hydro_sdf$Ordinate, jmd_hydro_sdf$Flow,
#'      xlab = "Hour", ylab = "Inflow (cfs)",
#'      type = "l")
"jmd_hydro_sdf"

# ==============================================================================
# John McGraw DAM - SEASONALITY
# ==============================================================================

#' JMD Flood Seasonality
#'
#' Results of flood seasonality analysis from the full John McGraw Dam period
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
#' @examples
#' sapply(jmd_seasonality,class)
#' print(jmd_seasonality)
"jmd_seasonality"

# ==============================================================================
# John McGraw DAM - STAGE DURATION
# ==============================================================================

#' JMD Monthly Stage Duration Curves
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
#' @examples
#' head(jmd_stage_duration)
"jmd_stage_duration"

# ==============================================================================
# John McGraw DAM - STAGE GAGE
# ==============================================================================

#' JMD Stage (WY 1980-2024)
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
#' @examples
#' sapply(jmd_wy1980_stage,class)
#' head(jmd_wy1980_stage)
#' plot(as.Date(jmd_wy1980_stage$date,tryFormats = c("%m/%d/%Y")), jmd_wy1980_stage$stage_ft,
#'       xlab = "Year", ylab = "Inflow (cfs)",
#'       type = "l")
"jmd_wy1980_stage"

# ==============================================================================
# John McGraw DAM - VOLUME FREQUENCY CURVE
# ==============================================================================

#' JMD VFC Parameters
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
#' @examples
#' head(jmd_vfc_parameters)
"jmd_vfc_parameters"

#' JMD Volume-Frequency Curve
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
#' @examples
#' head(jmd_vfc)
"jmd_vfc"

#' JMD BestFit Parameter Sets
#'
#' LP3 distribution parameters sets of volume-frequency curve results from
#' RMC-BestFit 2.0.
#'
#' @format A data frame with 10000 rows and 3 columns:
#' \describe{
#'   \item{mean_log}{Mean of log-transformed values}
#'   \item{sd_log}{Standard deviation of log-transformed values}
#'   \item{skew_log}{Skewness of log-transformed values}
#'   \item{log_likelihood}{Log-likelihood of parameter set}
#' }
#' @examples
#' sapply(jmd_bf_parameter_sets,class)
#' jmd_bf_parameter_sets[sample(nrow(jmd_bf_parameter_sets), 5), ]
#' hist(jmd_bf_parameter_sets$mean_log,
#'        xlab = "Mean Log", ylab = "Count")
"jmd_bf_parameter_sets"

#' JMD RFA Results - Expected Only
#'
#' JMD expected only stage-frequency results from RFA.
#'
#' @format A data frame with 10000 row and 3 columns:
#' \describe{
#'   \item{AEP}{Annual Exceedance Probabilities}
#'   \item{Expected}{Expected stages corresponding to AEPs}
#' }
#' @examples
#' head(jmd_rfa_expected)
"jmd_rfa_expected"

#' JMD RFA Results - Full Uncertainty
#'
#' JMD full uncertainty stage-frequency results from RFA.
#'
#' @format A data frame with 10000 rows and 3 columns:
#' \describe{
#'   \item{AEP}{Annual Exceedance Probabilities}
#'   \item{Upper}{Upper 95% CI of realizations}
#'   \item{Lower}{Lower 5% CI of realizations}
#'   \item{Expected}{Expected stages corresponding to AEPs}
#'   \item{Median}{Median stages corresponding to AEPs}
#' }
#' @examples
#' head(jmd_rfa_full)
"jmd_rfa_full"

#' Stratified Sampling - Example Data
#'
#' Example stratification of probability space into 20 bins using three
#' distribution transformations: Uniform, Normal, and Extreme Value Type I (EV1/Gumbel).
#' Demonstrates how different transformations allocate sampling effort across
#' the probability range, with EV1 concentrating more bins in the rare event
#' tail critical for dam safety analysis.
#'
#' @format A data frame with 60 rows and 5 columns:
#' \describe{
#'   \item{distribution}{Stratification distribution type: "uniform", "normal", or "ev1"}
#'   \item{bin}{Bin number (1-20), ordered from most frequent to most rare events}
#'   \item{lower}{Lower bound of the bin in the distribution's transformed space
#'     (probability for Uniform, z-score for Normal, Gumbel reduced variate for EV1)}
#'   \item{upper}{Upper bound of the bin in the distribution's transformed space}
#'   \item{weight}{Probability weight of the bin, representing the fraction of the
#'     total probability captured by each bin. Weights sum to approximately 1
#'     within each distribution.}
#' }
#'
#' @seealso [rfaR::stratified_sampler()]
#' @examples
#' example_stratified[sample(nrow(example_stratified), 5), ]
"example_stratified"

