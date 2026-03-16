#' RFA Stage-Frequency Simulation
#'
#' Performs reservoir flood frequency analysis using Monte Carlo simulation to
#' develop stage-frequency relationships. Combines stratified flow-frequency
#' sampling, flood seasonality, hydrograph scaling, and Modified Puls reservoir
#' routing to estimate annual exceedance probabilities of reservoir stage.
#'
#' Three simulation modes are available:
#' \describe{
#'   \item{\code{"median"}}{Single realization using the most likely (posterior mode)
#'     parameter set from RMC-BestFit. Produces one stage-frequency curve with no
#'     uncertainty bounds. Useful for quick screening or debugging.}
#'   \item{\code{"expected"}}{Single realization using expected value sampling, where
#'     each stratified flow sample is paired 1-to-1 with a different parameter set
#'     from the posterior distribution. Produces one stage-frequency curve representing
#'     the expected (mean) result integrated across parameter uncertainty.}
#'   \item{\code{"full"}}{Nested Monte Carlo with full uncertainty quantification.
#'     The outer loop iterates over all parameter sets (one per row of
#'     \code{bestfit_params}), and each inner loop independently samples natural
#'     variability via stratified sampling, seasonality, starting pool, and
#'     hydrograph shape. Each realization produces an independent stage-frequency
#'     curve; curves are combined on a common stage grid to yield expected, median,
#'     and 5th/95th percentile confidence bounds. The outer loop is parallelized
#'     using the \pkg{future} framework.}
#' }
#'
#' For all modes, natural variability is captured through stratified sampling in
#' EV1 (Gumbel) space with \code{Nbins} bins and \code{events_per_bin} events per
#' bin, providing reliable coverage from common events down to approximately 1e-8 AEP.
#'
#' @param sim_type Character string specifying the simulation mode. One of
#'   \code{"median"}, \code{"expected"}, or \code{"full"}. Default is
#'   \code{"expected"}.
#' @param bestfit_params Data frame or matrix of distribution parameters from
#'   RMC-BestFit MCMC output. Columns 1--3 are distribution parameters:
#'   \itemize{
#'     \item LP3: mean (log), standard deviation (log), skew (log)
#'     \item GEV: location, scale, shape
#'   }
#'   Column 4 is log-likelihood (used only in \code{"median"} mode to identify
#'   the posterior mode). For \code{"expected"} mode, must have at least
#'   \code{Nbins * events_per_bin} rows. For \code{"full"} mode, each row is
#'   treated as an independent realization.
#' @param dist Character string specifying the frequency distribution. Either
#'   \code{"LP3"} (Log-Pearson Type III, default) or \code{"GEV"} (Generalized
#'   Extreme Value).
#' @param stage_ts Data frame of historical reservoir stage with columns
#'   \code{date} (character in M/D/YYYY format) and \code{stage} (numeric, feet).
#' @param seasonality Numeric vector of length 12 giving monthly flood occurrence
#'   probabilities (relative frequencies). Used to sample the month of each
#'   simulated event, which determines the antecedent pool elevation.
#' @param hydrographs List of hydrograph data frames as returned by
#'   \code{\link{hydrograph_setup}}. Each element has columns \code{datetime},
#'   \code{hour}, \code{inflow}, and \code{hydrograph_num}, with attributes
#'   \code{"obs_vol"} (observed max n-day volume) and \code{"dt"} (timestep in
#'   hours). The list must have a \code{"probs"} attribute containing normalized
#'   sampling probabilities.
#' @param resmodel Data frame with three columns: elevation (ft), storage (acre-ft),
#'   and discharge (cfs). Defines the reservoir model for Modified Puls routing.
#' @param Nbins Integer. Number of stratified sampling bins in EV1 space.
#'   Default is 50.
#' @param events_per_bin Integer. Number of events sampled per bin. Default is 200.
#'   Total simulations per realization = \code{Nbins * events_per_bin}.
#' @param routing_dt Numeric. Routing timestep in hours. Passed to
#'   \code{\link{scale_hydrograph}} to resample hydrographs before routing.
#'   Supported values are \code{0.25} (15-min), \code{1} (1-hour, default),
#'   \code{6} (6-hour), and \code{24} (24-hour).
#' @param Ncores Integer or \code{NULL}. Number of parallel workers for
#'   \code{"full"} mode. If \code{NULL} (default), automatically selects based
#'   on available cores (capped at 16). Ignored for \code{"median"} and
#'   \code{"expected"} modes.
#' @param sim_name Optional character string to label the simulation. Used in
#'   the output CSV filename. If \code{NULL} (default), defaults to \code{"sim"}.
#'   Spaces are replaced with underscores in the filename.
#' @param results_dir Optional path to the directory where results are saved.
#'   If \code{NULL} (default), creates an \code{rfaR_results} folder in the
#'   current working directory.
#'
#' @return A list whose contents depend on \code{sim_type}:
#'
#' For \code{"median"} and \code{"expected"}:
#' \describe{
#'   \item{stage_frequency}{Data frame with columns \code{stage} and \code{AEP}
#'     from \code{\link{stage_frequency_curve}}.}
#'   \item{peakStage}{Matrix of simulated peak stages
#'     (\code{events_per_bin} rows by \code{Nbins} columns).}
#'   \item{peakFlow}{Matrix of simulated peak discharges (same dimensions).}
#'   \item{weights}{Stratified sampling bin weights.}
#' }
#'
#' For \code{"full"}:
#' \describe{
#'   \item{stage_frequency}{Data frame with columns \code{stage}, \code{expected},
#'     \code{median}, \code{lower_05}, and \code{upper_95} on a common stage grid.}
#'   \item{all_curves}{List of per-realization stage-frequency data frames
#'     (each with \code{stage} and \code{AEP} columns).}
#'   \item{aep_matrix}{Matrix of interpolated AEP values (500 rows by
#'     \code{Nrealizations} columns) on the common stage grid.}
#'   \item{common_stage}{Numeric vector of the common stage grid (length 500).}
#'   \item{Nrealizations}{Number of parameter set realizations processed.}
#'   \item{Nsims_per_realiz}{Number of simulations per realization.}
#' }
#'
#' @details
#' The stratified sampling approach divides the AEP range (default 0.99 to 1e-8)
#' into equal-width bins in EV1 (Gumbel reduced variate) space and samples events
#' uniformly within each bin. This ensures adequate representation of rare flood
#' events that would require orders of magnitude more samples under crude Monte
#' Carlo sampling.
#'
#' Post-processing uses the law of total probability via
#' \code{\link{stage_frequency_curve}}: for a grid of stage thresholds, the
#' weighted exceedance fraction is computed within each bin and summed across bins.
#'
#' In \code{"full"} mode, each parallel worker independently samples all random
#' inputs (seasonality, starting pool, hydrograph shape, and stratified z-variates)
#' in addition to using its own parameter set. This ensures full Monte Carlo
#' independence across realizations. Each worker post-processes its results into a
#' stage-frequency curve immediately, returning only the curve rather than the full
#' peak stage matrix, keeping memory usage manageable for large numbers of
#' realizations.
#'
#' Curves from individual realizations are combined by interpolating each onto a
#' common stage grid (spanning the global min/max across all realizations) in
#' log10(AEP) space, then computing summary statistics across realizations at
#' each stage threshold.
#'
#' Results are automatically exported as a CSV file named
#' \code{{sim_name}_{sim_type}_{MM_DD_YY_HHMM}.csv} in the \code{results_dir}
#' directory. For example, a median simulation named "Jay McGraw Dam" run on
#' June 4, 2025 at 2:30 PM would produce
#' \code{Jay_McGraw_Dam_median_06_04_25_1430.csv}.
#'
#' @seealso \code{\link{flow_frequency_sampler}},
#'   \code{\link{flow_frequency_sampler_expected}},
#'   \code{\link{stratified_sampler}},
#'   \code{\link{scale_hydrograph}},
#'   \code{\link{mod_puls_routing}},
#'   \code{\link{stage_frequency_curve}},
#'   \code{\link{hydrograph_setup}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # --- Setup ---
#' hydros <- hydrograph_setup(jmd_hydro_apr1999, jmd_hydro_jun1965,
#'                            jmd_hydro_may1955, jmd_hydro_pmf,
#'                            critical_duration = 2, routing_days = 10)
#'
#' # --- Expected only (default) ---
#' results_exp <- rfa_simulate(
#'   sim_type       = "expected",
#'   bestfit_params = jmd_vfc_parameters,
#'   stage_ts       = jmd_wy1980_stage,
#'   seasonality    = jmd_seasonality$relative_frequency,
#'   hydrographs    = hydros,
#'   resmodel       = jmd_resmodel
#' )
#'
#' # --- Median only ---
#' results_med <- rfa_simulate(
#'   sim_type       = "median",
#'   bestfit_params = jmd_vfc_parameters,
#'   stage_ts       = jmd_wy1980_stage,
#'   seasonality    = jmd_seasonality$relative_frequency,
#'   hydrographs    = hydros,
#'   resmodel       = jmd_resmodel
#' )
#'
#' # --- Full uncertainty (parallelized) ---
#' results_full <- rfa_simulate(
#'   sim_type       = "full",
#'   bestfit_params = jmd_vfc_parameters,
#'   stage_ts       = jmd_wy1980_stage,
#'   seasonality    = jmd_seasonality$relative_frequency,
#'   hydrographs    = hydros,
#'   resmodel       = jmd_resmodel,
#'   Ncores         = 4
#' )
#'
#' # Plot full uncertainty results
#' with(results_full$stage_frequency, {
#'   plot(expected, stage, type = "l", log = "x",
#'        xlab = "Annual Exceedance Probability",
#'        ylab = "Stage (ft)", main = "Stage-Frequency with Uncertainty")
#'   lines(median, stage, lty = 2)
#'   lines(lower_05, stage, lty = 3, col = "blue")
#'   lines(upper_95, stage, lty = 3, col = "red")
#'   legend("topleft", c("Expected", "Median", "5%", "95%"),
#'          lty = c(1, 2, 3, 3), col = c("black", "black", "blue", "red"))
#' })
#' }
rfa_simulate <- function(sim_type = "expected", bestfit_params, dist = "LP3",
                         stage_ts, seasonality, hydrographs, resmodel,
                         Nbins = 50, events_per_bin = 200,
                         routing_dt = 1,
                         Ncores = NULL,
                         sim_name = NULL,
                         results_dir = NULL) {

  # If results dir isnt defined
  if(is.null(results_dir)){
    results_dir <- file.path(getwd(),"rfaR_results")
  }
  dir.create(results_dir, showWarnings = FALSE)

  # Sim Name is for auto export of results
  # If its not provided, name the sim the sim_type + datetime
  if(is.null(sim_name)){
    sim_name <- "sim"
    sim_dt <- paste0(format(Sys.time(), "%m_%d_%y_%H%M"))
    result_csv_name <- paste0(sim_name,"_",sim_type,"_",sim_dt,".csv")
  } else{
    sim_name_clean <- gsub(" ", "_", sim_name)
    sim_dt <- paste0(format(Sys.time(), "%m_%d_%y_%H%M"))
    result_csv_name <- paste0(sim_name_clean,"_",sim_type,"_",sim_dt,".csv")
  }

  result_csv_file <- file.path(results_dir,result_csv_name)

  # Target AEPs (for Results - Consider hard coding this as built in data)
  target_aeps <- c(seq(0.99,0.91,by = -0.01),
                   unlist(lapply(0:-7, function(p) {seq(9 * 10^p, 1 * 10^p, by = -1 * 10^p) / 10})))

  # Remove rows with missing stages in stage timeseries
  stage_ts <- stage_ts[!is.na(stage_ts$stage),]

# ============================================================================
# MEDIAN ONLY ====
# ============================================================================
  if (sim_type == "median") {
    cli::cli_alert_success("Median Only")

    # MOST LIKELY PARAMETER SET ================================================
    # Can be switched to mean, confirm log-likelihood column
    # Does log-likelihood column exist?
    LL_exists <- "log_likelihood" %in% names(bestfit_params) | ncol(bestfit_params) > 3

    # if not use only params supplied OR mean of each parameter
    if (nrow(bestfit_params) == 1) {
      bf_params <- c(bestfit_params[1, 1],
                     bestfit_params[1, 2],
                     bestfit_params[1, 3])
    } else if (LL_exists == TRUE) {
      ml_idx <- which.max(bestfit_params[, 4])
      bf_params <- c(bestfit_params[ml_idx, 1],
                     bestfit_params[ml_idx, 2],
                     bestfit_params[ml_idx, 3])
    } else {
      bf_params <- c(mean(bestfit_params[, 1]),
                     mean(bestfit_params[, 2]),
                     mean(bestfit_params[, 3]))
    }

    # FLOW-FREQUENCY SAMPLER ===================================================
    cli::cli_h1("Generating Volume-Frequency Samples")
    cli::cli_alert_info(paste0(dist," distribution"))

    Q_samp <- flow_frequency_sampler(bf_params, freq_dist = dist,
                           strat_dist = "ev1",
                           Nbin = Nbins, Mevent = events_per_bin)
    # Save flow matrix
    Q_matrix <- Q_samp$flow

    cli::cli_alert_success("Stratified Samples of Volume-Frequency Created")

    # SEASONALITY/STARTING POOL SAMPLER ========================================
    cli::cli_h1("Pre-Allocating Seasonality & Starting Pool Sampling")
    Nsims <- Nbins * events_per_bin

    # Initialize vector of sampled months
    InitMonths <- sample(1:12, size = Nsims, replace = TRUE, prob = seasonality)

    # Initialize vector of sampled stages
    InitStages <- numeric(Nsims)

    # Use vector of sampled months to create sample of starting stages
    stage_ts$months <- lubridate::month(lubridate::mdy(stage_ts$date))

    # Extract unique months
    UniqMonths <- sort(unique(InitMonths))

    for (i in 1:length(UniqMonths)) {
      sampleID <- which(InitMonths == UniqMonths[i])
      InitStages[sampleID] <- sample(stage_ts$stage[stage_ts$months %in% UniqMonths[i]],
                                     size = sum(InitMonths == UniqMonths[i]),
                                     replace = TRUE)
    }

    cli::cli_alert_success("Seasonality & Starting Pool Sampled")

    # HYDROGRAPH SCALER/SAMPLER ================================================
    cli::cli_h1("Pre-Allocating Hydrograph Shape Sampling")

    # Sample Order with probs (normlized weights)
    hydroSamps <- sample(1:length(hydrographs), size = Nsims,
                         replace = TRUE,
                         prob = attr(hydrographs, "probs"))

    cli::cli_alert_success("Hydrograph Shape Sampled")

    # ROUTING EVENTS ===========================================================
    # Peak Stage (or Flow) Results matrix
    peakStage <- peakFlow <- matrix(NA, nrow = events_per_bin, ncol = Nbins)

    # Tabular Results of Realization
    # Cant put numeric and character in the same matrix
    realiz_numeric <- matrix(NA_real_, nrow = Nsims, ncol = 5)
    realiz_hydro   <- character(Nsims)

    realiz <- 0

    cli::cli_progress_bar("Median Only Simulations", total = Nsims)

    for (i in 1:nrow(Q_matrix)) {
      for (j in 1:ncol(Q_matrix)) {
        realiz <- (i - 1) * ncol(Q_matrix) + j

        # Hydrograph shape
        hydrograph_shape <- hydrographs[[hydroSamps[realiz]]][,2:3]
        # Hydrograph observed volume
        obs_hydrograph_vol <- attr(hydrographs[[hydroSamps[realiz]]],"obs_vol")
        # Scale hydrograph to sample volume
        scaled_hydrograph <- scale_hydrograph(hydrograph_shape,
                                              obs_hydrograph_vol,
                                              Q_matrix[i, j],
                                              routing_dt = routing_dt)
        # Route scaled hydrograph
        tmpResults <- mod_puls_routing(resmodel_df = resmodel, inflow_df = scaled_hydrograph,
                                       initial_elev = InitStages[realiz], full_results = FALSE)

        # Record results
        peakStage[i, j] <- tmpResults[1]
        peakFlow[i, j] <- tmpResults[2]

        # Realiz table
        realiz_numeric[realiz, ] <- c(InitMonths[realiz], InitStages[realiz], Q_matrix[i,j], tmpResults[1], tmpResults[2])
        realiz_hydro[realiz] <- attr(hydrographs[[hydroSamps[realiz]]], "name")

        cli::cli_progress_update()
      }
    }
    cli::cli_progress_done()
    cli::cli_alert_success("Median Only Simulations Complete")

    # Combine realiz tabluar data
    realiz_results <- data.frame(month      = realiz_numeric[, 1],
                                 init_stage = realiz_numeric[, 2],
                                 inflow_vol = realiz_numeric[, 3],
                                 peak_stage = realiz_numeric[, 4],
                                 peak_dis   = realiz_numeric[, 5],
                                 hydrograph = realiz_hydro)

    # REALIZATION STAGE-FREQUENCY CURVE ========================================
    cli::cli_h1("Calculating exceedance probabilities")
    median_stage_aep <- stage_frequency_curve(peakStage,Q_samp$weights)
    median_stage_freq <- data.frame(AEP = target_aeps,
                                   Median = aep2stage(median_stage_aep$AEP,
                                                      median_stage_aep$stage,
                                                      target_aeps))
    cli::cli_alert_success("Calcuated stage-frequency curve")

    # Write to file ============================================================
    write.csv(median_stage_freq,result_csv_file,row.names = FALSE)
    cli::cli_alert_success("Stage-Frequency curve successfully exported to {.path {result_csv_file}}.")

    # Return
    return(list(
      stage_frequency = median_stage_freq,
      peakStage       = peakStage,
      peakFlow        = peakFlow,
      weights         = Q_samp$weights,
      realization_results = realiz_results
    ))

# ============================================================================
# EXPECTED ONLY ====
# ============================================================================
  } else if (sim_type == "expected") {
    cli::cli_alert_success("Expected Only")

    # FLOW-FREQUENCY SAMPLER ===================================================
    cli::cli_h1("Generating Volume-Frequency Samples")
    cli::cli_alert_info(paste0(dist," distribution"))

    Q_samp <- flow_frequency_sampler_expected(bestfit_params, freq_dist = dist,
                                     strat_dist = "ev1",
                                     Nbin = Nbins, Mevent = events_per_bin)
    # Save flow matrix
    Q_matrix <- Q_samp$flow

    cli::cli_alert_success("Stratified Samples of Volume-Frequency Created")

    # SEASONALITY/STARTING POOL SAMPLER ========================================
    cli::cli_h1("Pre-Allocating Seasonality & Starting Pool Sampling")
    Nsims <- Nbins * events_per_bin

    # Initialize vector of sampled months
    InitMonths <- sample(1:12, size = Nsims, replace = TRUE, prob = seasonality)

    # Initialize vector of sampled stages
    InitStages <- numeric(Nsims)

    # Use vector of sampled months to create sample of starting stages
    stage_ts$months <- lubridate::month(lubridate::mdy(stage_ts$date))

    # Extract unique months
    UniqMonths <- sort(unique(InitMonths))

    for (i in 1:length(UniqMonths)) {
      sampleID <- which(InitMonths == UniqMonths[i])
      InitStages[sampleID] <- sample(stage_ts$stage[stage_ts$months %in% UniqMonths[i]],
                                     size = sum(InitMonths == UniqMonths[i]),
                                     replace = TRUE)
    }

    cli::cli_alert_success("Seasonality & Starting Pool Sampled")

    # HYDROGRAPH SCALER/SAMPLER ================================================
    cli::cli_h1("Pre-Allocating Hydrograph Shape Sampling")

    # Sample Order with probs (normlized weights)
    hydroSamps <- sample(1:length(hydrographs), size = Nsims,
                         replace = TRUE,
                         prob = attr(hydrographs, "probs"))

    cli::cli_alert_success("Hydrograph Shape Sampled")

    # ROUTING EVENTS ===========================================================
    # Peak Stage (or Flow) Results matrix
    peakStage <- peakFlow <- matrix(NA, nrow = events_per_bin, ncol = Nbins)

    # Tabular Results of Realization
    # Cant put numeric and character in the same matrix
    realiz_numeric <- matrix(NA_real_, nrow = Nsims, ncol = 5)
    realiz_hydro   <- character(Nsims)

    realiz <- 0
    cli::cli_progress_bar("Expected Only Simulations", total = Nsims)

    for (i in 1:nrow(Q_matrix)) {
      for (j in 1:ncol(Q_matrix)) {
        realiz <- (i - 1) * ncol(Q_matrix) + j

        # Hydrograph shape
        hydrograph_shape <- hydrographs[[hydroSamps[realiz]]][,2:3]
        # Hydrograph observed volume
        obs_hydrograph_vol <- attr(hydrographs[[hydroSamps[realiz]]],"obs_vol")
        # Scale hydrograph to sample volume
        scaled_hydrograph <- scale_hydrograph(hydrograph_shape,
                                              obs_hydrograph_vol,
                                              Q_matrix[i, j],
                                              routing_dt = routing_dt)
        # Route scaled hydrograph
        tmpResults <- mod_puls_routing(resmodel_df = resmodel, inflow_df = scaled_hydrograph,
                                       initial_elev = InitStages[realiz], full_results = FALSE)

        # Record results
        peakStage[i, j] <- tmpResults[1]
        peakFlow[i, j] <- tmpResults[2]

        # Realiz table
        realiz_numeric[realiz, ] <- c(InitMonths[realiz], InitStages[realiz], Q_matrix[i,j], tmpResults[1], tmpResults[2])
        realiz_hydro[realiz] <- attr(hydrographs[[hydroSamps[realiz]]], "name")

        cli::cli_progress_update()
      }
    }
    cli::cli_progress_done()
    cli::cli_alert_success("Expected Only Simulations Complete")

    # Combine realiz tabular data
    realiz_results <- data.frame(month      = realiz_numeric[, 1],
                                 init_stage = realiz_numeric[, 2],
                                 inflow_vol = realiz_numeric[, 3],
                                 peak_stage = realiz_numeric[, 4],
                                 peak_dis   = realiz_numeric[, 5],
                                 hydrograph = realiz_hydro)

    # REALIZATION STAGE-FREQUENCY CURVE ========================================
    cli::cli_h1("Calculating exceedance probabilities")
    expected_stage_aep <- stage_frequency_curve(peakStage,Q_samp$weights)
    expected_stage_freq <- data.frame(AEP = target_aeps,
                                      Expected = aep2stage(expected_stage_aep$AEP,
                                                         expected_stage_aep$stage,
                                                         target_aeps))
    cli::cli_alert_success("Calcuated stage-frequency curve")

    # Write to file ============================================================
    write.csv(expected_stage_freq,result_csv_file,row.names = FALSE)
    cli::cli_alert_success("Stage-Frequency curve successfully exported to {.path {result_csv_file}}.")

    # Return
    return(list(
      stage_frequency = expected_stage_freq,
      peakStage       = peakStage,
      peakFlow        = peakFlow,
      weights         = Q_samp$weights,
      realization_results = realiz_results
    ))

# ============================================================================
# FULL UNCERT ===
# ============================================================================
  } else if (sim_type == "full") {
    cli::cli_alert_success("Full Uncertainty")

    # CONFIGURATION ============================================================
    Nrealizations <- nrow(bestfit_params)
    Nsims_per_realiz <- Nbins * events_per_bin

    # If Ncores usn't defined, default to something reasonably
    # The last thing I want is rfaR nuking someone's motherboard
    if (is.null(Ncores)) {
      total_cores <- future::availableCores(omit = 1)
      if(total_cores >= 19){
        use_cores <- 16
      }else{
        use_cores <- 12
      }
      Ncores <- min(use_cores, total_cores)
    }

    # Results directory - add functionality for user to set this
    realization_dir <- file.path(results_dir,"realizations")
    dir.create(realization_dir, showWarnings = FALSE)

    cli::cli_alert_info("Realizations: {Nrealizations}")
    cli::cli_alert_info("Simulations per realization: {Nsims_per_realiz}")
    cli::cli_alert_info("Total routings: {Nrealizations * Nsims_per_realiz}")
    cli::cli_alert_info("Parallel workers: {Ncores}")
    cli::cli_alert_info("Saving realization results to: {realization_dir}")

    # PARALLEL EXECUTION =======================================================
    cli::cli_h1("Running Full Uncertainty Simulations")

    # Avoid memory issue with workers
    if (is.null(getOption("future.globals.maxSize"))) {
      options(future.globals.maxSize = 4 * 1024^3)
    }

    cli::cli_alert_info("Distributing {Nrealizations} realizations across {Ncores} workers")

    # Progress bar setup
    has_progressr <- requireNamespace("progressr", quietly = TRUE)
    if (has_progressr) {
      progressr::handlers("cli")
    } else {
      cli::cli_alert_warning("Install {.pkg progressr} for progress bars in parallel mode")
    }

    future::plan(future::multisession, workers = Ncores)
    on.exit(future::plan(future::sequential), add = TRUE)

    run_parallel <- function() {
      if (has_progressr) {
        p <- progressr::progressor(steps = Nrealizations)
      }

      future.apply::future_lapply(1:Nrealizations, function(k) {
        tryCatch({

          # SEASONALITY/STARTING POOL SAMPLER ================================
          InitMonths <- sample(1:12, size = Nsims_per_realiz, replace = TRUE,
                               prob = seasonality)

          InitStages <- numeric(Nsims_per_realiz)

          stage_ts$months <- lubridate::month(lubridate::mdy(stage_ts$date))
          UniqMonths <- sort(unique(InitMonths))

          for (i in seq_along(UniqMonths)) {
            sampleID <- which(InitMonths == UniqMonths[i])
            InitStages[sampleID] <- sample(
              stage_ts$stage[stage_ts$months %in% UniqMonths[i]],
              size = sum(InitMonths == UniqMonths[i]),
              replace = TRUE
            )
          }

          # HYDROGRAPH SCALER/SAMPLER ========================================
          hydroSamps <- sample(1:length(hydrographs), size = Nsims_per_realiz,
                               replace = TRUE,
                               prob = attr(hydrographs, "probs"))

          # FLOW-FREQUENCY SAMPLER ===========================================
          bf_params_k <- c(bestfit_params[k, 1],
                           bestfit_params[k, 2],
                           bestfit_params[k, 3])

          Q_samp <- flow_frequency_sampler(bf_params_k, freq_dist = dist,
                                           strat_dist = "ev1",
                                           Nbin = Nbins, Mevent = events_per_bin)
          Q_matrix <- Q_samp$flow

          # ROUTING EVENTS ===================================================
          peakStage_k <- matrix(NA, nrow = events_per_bin, ncol = Nbins)

          realiz <- 0

          for (i in 1:nrow(Q_matrix)) {
            for (j in 1:ncol(Q_matrix)) {
              realiz <- (i - 1) * ncol(Q_matrix) + j

              hydrograph_shape <- hydrographs[[hydroSamps[realiz]]][, 2:3]
              obs_hydrograph_vol <- attr(hydrographs[[hydroSamps[realiz]]], "obs_vol")

              scaled_hydrograph <- scale_hydrograph(hydrograph_shape,
                                                    obs_hydrograph_vol,
                                                    Q_matrix[i, j],
                                                    routing_dt = routing_dt)

              tmpResults <- mod_puls_routing(resmodel_df = resmodel,
                                             inflow_df = scaled_hydrograph,
                                             initial_elev = InitStages[realiz],
                                             full_results = FALSE)

              peakStage_k[i, j] <- tmpResults[1]
            }
          }

          # POST-PROCESS =====================================================
          realization_k <- stage_frequency_curve(peakStage_k, Q_samp$weights)

          # Save to disk
          saveRDS(realization_k, file = file.path(realization_dir,paste0("realization_", k, ".rds")))

          if (has_progressr) p()

          realization_k

        }, error = function(e) {
          if (has_progressr) p()
          list(error = TRUE, message = e$message, realization = k)
        })
      }, future.seed = TRUE,
      future.packages = c("rfaR", "zoo", "lmom"))
    }

    # Run with or without progressr
    if (has_progressr) {
      results_list <- progressr::with_progress(run_parallel())
    } else {
      results_list <- run_parallel()
    }

    future::plan(future::sequential)

    # CHECK FOR FAILURES =======================================================
    failures <- sapply(results_list, function(x) is.list(x) && isTRUE(x$error))
    n_failed <- sum(failures)

    if (n_failed > 0) {
      cli::cli_alert_warning("{n_failed} of {Nrealizations} realizations failed")
      failed_msgs <- sapply(results_list[failures], function(x) x$message)
      cli::cli_alert_info("First failure: {failed_msgs[1]}")
    }

    cli::cli_alert_success("Full Uncertainty Simulations Complete")

    # ============================================================================
    # FULL UNCERT POST PROCESSING
    # ============================================================================
    cli::cli_h1("Full Uncertainty Post-Processing")

    # COLLECT ALL RESULTS FROM DISK ============================================
    cli::cli_h3("Loading realizations from disk")

    all_files <- list.files(realization_dir, pattern = "realization_.*\\.rds", full.names = TRUE)
    results_list <- lapply(all_files, readRDS)

    cli::cli_alert_info("Loaded {length(results_list)} of {Nrealizations} realizations")

    # CHECK FOR INCOMPLETE RUN =================================================
    if (length(results_list) < Nrealizations) {
      cli::cli_alert_warning("Only {length(results_list)} of {Nrealizations} realizations completed")
      Nrealizations <- length(results_list)
    }

    # CONFIDENCE INTERVALS =====================================================
    # Hard coded to 5 and 95 - possible to add optional functionallity later
    cli::cli_h1("Computing confidence bounds across realizations")

    # Stage matrix
    stage_matrix <- interpolate_stage_matrix(results_list, target_aeps)

    # 5, 50, and 95th stage quantiles
    median_50   <- apply(stage_matrix, 1, quantile, probs = 0.50, na.rm = TRUE)
    lower_05     <- apply(stage_matrix, 1, quantile, probs = 0.05, na.rm = TRUE)
    upper_95     <- apply(stage_matrix, 1, quantile, probs = 0.95, na.rm = TRUE)
    cli::cli_alert_success("Upper and Lower CI calculated")

    # EXPECTED =================================================================
    cli::cli_h1("Computing Expected Curve")
    # min and max stage
    global_min <- min(sapply(results_list, function(x) min(x$stage)))
    global_max <- max(sapply(results_list, function(x) max(x$stage)))

    # Stage Vector
    expect_stage_vect <- seq(global_min, global_max, length.out = events_per_bin)

    # AEPs of Stages from Stage Vector
    aep_interp <- interpolate_aep_matrix(results_list, expect_stage_vect)

    # Take the average of the AEPs (horizontally across stages)
    expected_aeps <- rowMeans(aep_interp, na.rm = TRUE)

    # Expected curve
    expected <- aep2stage(expected_aeps, expect_stage_vect, target_aeps)
    cli::cli_alert_success("Expected curve calculated")

    full_stage_freq <- data.frame(AEP = target_aeps,
                                  Upper = upper_95,
                                  Lower = lower_05,
                                  Expected = expected,
                                  Median = median_50)

    cli::cli_alert_success("Full Uncertainty Analysis Complete")

    # Write to file ============================================================
    write.csv(full_stage_freq,result_csv_file,row.names = FALSE)
    cli::cli_alert_success("Stage-Frequency curve successfully exported to {.path {result_csv_file}}.")

    return(list(
      stage_frequency  = full_stage_freq,
      Nrealizations    = Nrealizations,
      Nsims_per_realiz = Nsims_per_realiz,
      realization_directory = realization_dir))
  }
}
