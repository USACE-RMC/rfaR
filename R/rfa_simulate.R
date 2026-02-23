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
#'
#'   @param results_dir Directory to save full uncertainty progress in case of failure.
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
#'   stage_ts       = jmd_por_stage,
#'   seasonality    = jmd_seasonality$relative_frequency,
#'   hydrographs    = hydros,
#'   resmodel       = jmd_resmodel
#' )
#'
#' # --- Median only ---
#' results_med <- rfa_simulate(
#'   sim_type       = "median",
#'   bestfit_params = jmd_vfc_parameters,
#'   stage_ts       = jmd_por_stage,
#'   seasonality    = jmd_seasonality$relative_frequency,
#'   hydrographs    = hydros,
#'   resmodel       = jmd_resmodel
#' )
#'
#' # --- Full uncertainty (parallelized) ---
#' results_full <- rfa_simulate(
#'   sim_type       = "full",
#'   bestfit_params = jmd_vfc_parameters,
#'   stage_ts       = jmd_por_stage,
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
                         results_dir = NULL) {

  # IF NO BINS OR EVENTS ARE DEFINED
  if(is.null(Nbins)){
    Nbins <- 50
  }
  if(is.null(events_per_bin)){
    events_per_bin <- 200
  }

  if(is.null(results_dir)){
    results_dir <- getwd()
  }


# ============================================================================
# MEDIAN ONLY ====
# ============================================================================
  if (sim_type == "median") {
    cli::cli_alert_success("Median Only")

    # MOST LIKELY PARAMETER SET ================================================
    # Can be switched to mean, confirm log-likelihood column
    ml_idx <- which.max(bestfit_params[,4])
    bf_params <- c(bestfit_params[ml_idx,1],
                   bestfit_params[ml_idx,2],
                   bestfit_params[ml_idx,3])

    # add provision that if only one parameter set is supplied, it can be used

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

        cli::cli_progress_update()
      }
    }
    cli::cli_progress_done()
    cli::cli_alert_success("Median Only Simulations Complete")

    # REALIZATION STAGE-FREQUENCY CURVE ========================================
    cli::cli_h1("Calculating exceedance probabilities")
    median_stage_freq <- stage_frequency_curve(peakStage,Q_samp$weights, Sbins = 500)
    cli::cli_alert_success("Calcuated stage-frequency curve")

    # Raw Return right now
    return(list(
      stage_frequency = median_stage_freq,
      peakStage       = peakStage,
      peakFlow        = peakFlow,
      weights         = Q_samp$weights
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

        cli::cli_progress_update()
      }
    }
    cli::cli_progress_done()
    cli::cli_alert_success("Expected Only Simulations Complete")

    # REALIZATION STAGE-FREQUENCY CURVE ========================================
    cli::cli_h1("Calculating exceedance probabilities")
    expected_stage_freq <- stage_frequency_curve(peakStage,Q_samp$weights, Sbins = 500)
    cli::cli_alert_success("Calcuated stage-frequency curve")

    # Raw Return right now
    return(list(
      stage_frequency = expected_stage_freq,
      peakStage       = peakStage,
      peakFlow        = peakFlow,
      weights         = Q_samp$weights
    ))

# ============================================================================
# FULL UNCERT ===
# ============================================================================
  } else if (sim_type == "full") {
    cli::cli_alert_success("Full Uncertainty")

    # DEPENDENCY CHECK =========================================================
    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
      stop("Packages 'future' and 'future.apply' are required for full uncertainty mode.\n",
           "Install with: install.packages(c('future', 'future.apply'))")
    }

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

    cli::cli_alert_info("Realizations: {Nrealizations}")
    cli::cli_alert_info("Simulations per realization: {Nsims_per_realiz}")
    cli::cli_alert_info("Total routings: {Nrealizations * Nsims_per_realiz}")
    cli::cli_alert_info("Parallel workers: {Ncores}")

    # PARALLEL EXECUTION =======================================================
    cli::cli_h1("Running Full Uncertainty Simulations")
    cli::cli_alert_info("Distributing {Nrealizations} realizations across {Ncores} workers...")

    future::plan(future::multisession, workers = Ncores)

    # Progress bar setup
    has_progressr <- requireNamespace("progressr", quietly = TRUE)
    if (has_progressr) {
      progressr::handlers("cli")
    } else {
      cli::cli_alert_warning("Install {.pkg progressr} for progress bars in parallel mode")
    }

    run_parallel <- function() {
      if (has_progressr) {
        p <- progressr::progressor(steps = Nrealizations)
      }

      future.apply::future_lapply(1:Nrealizations, function(k) {
        tryCatch({

          # SEASONALITY/STARTING POOL SAMPLER ==================================
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

          # HYDROGRAPH SCALER/SAMPLER ==========================================
          hydroSamps <- sample(1:length(hydrographs), size = Nsims_per_realiz,
                               replace = TRUE,
                               prob = attr(hydrographs, "probs"))

          # FLOW-FREQUENCY SAMPLER =============================================
          bf_params_k <- c(bestfit_params[k, 1],
                           bestfit_params[k, 2],
                           bestfit_params[k, 3])

          Q_samp <- flow_frequency_sampler(bf_params_k, freq_dist = dist,
                                           strat_dist = "ev1",
                                           Nbin = Nbins, Mevent = events_per_bin)
          Q_matrix <- Q_samp$flow

          # ROUTING EVENTS =====================================================
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

          # POST-PROCESS =======================================================
          curve_k <- stage_frequency_curve(peakStage_k, Q_samp$weights, Sbins = 500)

          # SAVE PROGRESS
          saveRDS(curve_k, file = file.path(results_dir, paste0("curve_", k, ".rds")))

          # Signal progress
          if (has_progressr) p()

          curve_k

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
      cli::cli_alert_info("Saving partial results to rfaR_partial_results.RData")
      save(results_list, failures, file = "rfaR_partial_results.RData")
      # Keep only successful results
      results_list <- results_list[!failures]
      Nrealizations <- length(results_list)
      cli::cli_alert_success("Continuing with {Nrealizations} successful realizations")
    } else {
      cli::cli_alert_success("Full Uncertainty Simulations Complete")
    }

    # COMBINE REALIZATIONS ON A COMMON STAGE GRID ==============================
    cli::cli_h1("Computing confidence bounds across realizations")

    # Build a common stage grid spanning all realizations
    global_min <- min(sapply(results_list, function(x) min(x$stage)))
    global_max <- max(sapply(results_list, function(x) max(x$stage)))
    common_stage <- seq(global_min, global_max, length.out = 500)

    # Interpolate each realization's AEP onto the common stage grid
    # Interpolate in log-AEP space for better behavior in the tails
    aep_matrix <- matrix(NA, nrow = 500, ncol = Nrealizations)

    for (k in 1:Nrealizations) {
      curve_k <- results_list[[k]]
      # Remove any zero AEPs before log transform
      valid <- curve_k$AEP > 0
      if (sum(valid) >= 2) {
        aep_matrix[, k] <- 10^approx(x = curve_k$stage[valid],
                                     y = log10(curve_k$AEP[valid]),
                                     xout = common_stage,
                                     rule = 2)$y
      }
    }

    # Summarize across realizations
    expected_aep <- rowMeans(aep_matrix, na.rm = TRUE)
    median_aep   <- apply(aep_matrix, 1, quantile, probs = 0.50, na.rm = TRUE)
    lower_05     <- apply(aep_matrix, 1, quantile, probs = 0.05, na.rm = TRUE)
    upper_95     <- apply(aep_matrix, 1, quantile, probs = 0.95, na.rm = TRUE)

    full_stage_freq <- data.frame(
      stage      = common_stage,
      expected   = expected_aep,
      median     = median_aep,
      lower_05   = lower_05,
      upper_95   = upper_95
    )

    cli::cli_alert_success("Full Uncertainty Analysis Complete")

    return(list(
      stage_frequency    = full_stage_freq,
      all_curves         = results_list,
      aep_matrix         = aep_matrix,
      common_stage       = common_stage,
      Nrealizations      = Nrealizations,
      Nsims_per_realiz   = Nsims_per_realiz
    ))
  }
}
