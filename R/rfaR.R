#' RFA Stage-Frequency Analysis
#'
#' Performs reservoir flood analysis using Monte Carlo simulation to develop
#' stage-frequency relationships. Combines flow-frequency sampling, flood
#' seasonality, hydrograph scaling, and Modified Puls routing.
#'
#' @param bestfit_params Data frame of distribution parameters from RMC-BestFit.
#'   For LP3: columns are mean (log), sd (log), skew (log).
#'   For GEV: columns are location, scale, shape.
#' @param dist Distribution type. Either `"LP3"` (default) or `"GEV"`.
#' @param stage_ts Data frame of historical stage time series with columns
#'   `date` (m/d/yyyy) and `stage`.
#' @param seasonality Vector of monthly relative flood probabilities (length 12).
#' @param hydrographs List of hydrograph shapes. Each list item is a hydrograph data.frame. See: [hydrograph_setup()]
#' @param resmodel Data frame with three columns: elevation (ft), storage (acre-feet),
#'   and discharge (cfs). Must be in that order.
#' @param critical_dur Critical duration in days for hydrograph scaling.
#' @param routing_dur Routing duration in days.
#' @param expected_only Logical. If `TRUE` (default), uses expected value samples only.
#'   If `FALSE`, includes full uncertainty ensemble.
#'
#' @return A data frame containing stage-frequency results.
#'
#' @export
#'
#' @seealso [flow_frequency_sampler()], [mod_puls_routing()], [scale_hydrograph()],[hydrograph_setup()]
#'
#' @examples
#' # Example usage
#' # results <- rfaR(jmd_vfc_parameters,
#' #                 stage_ts = jmd_por_stage,
#' #                 seasonality = jmd_seasonality$relative_frequency,
#' #                 hydrographs = hydrograph_matrix,
#' #                 resmodel = jmd_resmodel,
#' #                 critical_dur = 48,
#' #                 routing_dur = 240)
rfaR <- function(bestfit_params, dist = "LP3", stage_ts, seasonality, hydrographs,
                 resmodel, critical_dur, routing_dur, expected_only = TRUE, Nbins = 50, events_per_bin = 20) {
  cli::cli_h1("Running rfaR :)")
  if(expected_only){
    cli::cli_alert_success("Expected Only")
  } else{
    cli::cli_alert_success("Full Uncertainty")
  }

  # ============================================================================
  # FLOW FREQUENCY SAMPLER
  # ============================================================================
  cli::cli_h1("Generating Volume-Frequency Samples")
  cli::cli_alert_info(paste0(dist," distribution"))

  Q_samp <- flow_frequency_sampler(bestfit_params = bestfit_params, dist = dist, ExpectedOnly = expected_only)

  cli::cli_alert_success("Stratified Samples of Volume-Frequency Created")

  # ============================================================================
  # SEASONALITY/STARTING POOL SAMPLER
  # ============================================================================
  cli::cli_h1("Pre-Allocating Seasonality & Starting Pool Sampling")
  # Number of simulations
  Nsims <- nrow(Q_samp$flow) * ncol(Q_samp$flow)

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
                                   size = sum(InitMonths == UniqMonths[i]), replace = TRUE)
  }
  cli::cli_alert_success("Seasonality & Starting Pool Sampled")

  # ============================================================================
  # HYDROGRAPH SCALER/SAMPLER
  # ============================================================================
  cli::cli_h1("Pre-Allocating Hydrograph Shape Sampling")
  # Sample Order
  hydroSamps <- sample(1:length(hydrographs), size = Nsims, replace = TRUE)

  # Duration of Hydrograph
  hydroDur <- sapply(hydrographs, function(df) nrow(df))

  # K-Day Max Observed of Hydrograph
  obsVol <- sapply(hydrographs, function(df) max(zoo::rollmean(df$inflow, k = critical_dur)))
  cli::cli_alert_success("Hydrograph Shape Sampled")

  # ============================================================================
  # SIMULATING SAMPLES
  # ============================================================================
  peakStage <- peakFlow <- matrix(NA, nrow = nrow(Q_samp$flow), ncol = ncol(Q_samp$flow))
  n_inner <- ncol(Q_samp$flow)

  # EXPECTED ONLY ++++++
  if(expected_only){
    cli::cli_h1("Simulating Expected Only Frequency Analysis")

    realiz <- 0
    cli::cli_progress_bar("Expected Only Simulation", total = nrow(Q_samp$flow))
    for (i in 1:nrow(Q_samp$flow)) {
      cli::cli_progress_update()
      for (j in 1:ncol(Q_samp$flow)) {
        realiz <- (i - 1) * n_inner + j
        # Hydrograph shape
        hydrograph_shape <- hydrographs[[hydroSamps[realiz]]][,2:3]
        # Hydrograph observed volume
        obs_hydrograph_vol <- attr(hydrographs[[hydroSamps[realiz]]],"obs_vol")
        # Scale hydrograph to sample volume
        scaled_hydrograph <- scale_hydrograph(hydrograph_shape,
                                              obs_hydrograph_vol,
                                              Q_samp$flow[i,j])
        # Route scaled hydrograph
        tmpResults <- mod_puls_routing(resmodel_df = resmodel, inflow_df = scaled_hydrograph,
                                       initial_elev = InitStages[realiz], full_results = FALSE)
        # Record results
        peakStage[i, j] <- tmpResults[1]
        peakFlow[i, j] <- tmpResults[2]
      }
    }
    cli::cli_progress_done()
  # FULL UNCERT ++++++
  }else{
    cli::cli_h1("Simulating Full Uncertainty Frequency Analysis - (no progress bar yet, assume 30 minutes)")
    future::plan(future::multisession, workers = future::availableCores() - 1)

    n_inner <- ncol(Q_samp$flow)

    results <- future.apply::future_lapply(1:nrow(Q_samp$flow), function(i) {
      stage_row <- numeric(n_inner)
      flow_row <- numeric(n_inner)

      cli::cli_alert_success(paste0("Full Uncertainty Requires Parallel Processing. Running simulation using ", as.numeric(future::availableCores() - 1)," cores."))

      for (j in 1:n_inner) {
        realiz <- (i - 1) * n_inner + j
        # Hydrograph shape
        hydrograph_shape <- hydrographs[[hydroSamps[realiz]]][,2:3]
        # Hydrograph observed volume
        obs_hydrograph_vol <- attr(hydrographs[[hydroSamps[realiz]]],"obs_vol")
        # Scale hydrograph to sample volume
        scaled_hydrograph <- scale_hydrograph(hydrograph_shape,
                                              obs_hydrograph_vol,
                                              Q_samp$flow[i,j])
        # Route scaled hydrograph
        tmpResults <- mod_puls_routing(resmodel_df = resmodel,
                                       inflow_df = scaled_hydrograph,
                                       initial_elev = InitStages[realiz],
                                       full_results = FALSE)
        # Record results
        stage_row[j] <- tmpResults[1]
        flow_row[j] <- tmpResults[2]
      }
      list(stage = stage_row, flow = flow_row)
      },
      future.seed = TRUE,
      future.packages = c("rfaR", "zoo"))

    future::plan(future::sequential)

    peakStage <- do.call(rbind, lapply(results, `[[`, "stage"))
    peakFlow <- do.call(rbind, lapply(results, `[[`, "flow"))
    cli::cli_alert_success("Full Uncertainty Complete")
  }

  # ============================================================================
  # POST PROCESS
  # ============================================================================
  cli::cli_h1("Post Processing")
  Sbins = 1000
  Nbins <- Q_samp$nbins
  Mevents <- Q_samp$events_per_bin
  Weights <- Q_samp$weights
  peakStages <- seq(from = min(peakStage), to = max(peakStage), length.out = Sbins)

  # EXPECTED ONLY ++++++
  if(expected_only){
    # CALC WEIGHTED AEP FOR PEAK STAGES
    aepStages <- matrix(0, nrow = Sbins, ncol = ncol(peakStage))
    cli::cli_progress_bar("Calculating AEP (expected only)", total = ncol(peakStage))
    for (m in 1:ncol(peakStage)) {
      tmpStage <- matrix((peakStage[, m]), nrow = Mevents, ncol = Nbins)
      for (i in 1:Sbins) {
        for (j in 1:Nbins) {
          n <- 0
          for (k in 1:Mevents) {
            if (tmpStage[k, j] > peakStages[i]) {
              n <- n + 1
            }
          }
          aepStages[i, m] <- aepStages[i, m] + n / Mevents * Weights[j]
        }
      }
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
    # FULL UNCERT ++++++
  } else{
    aepStages <- matrix(0, nrow = Sbins, ncol = ncol(peakStage))
    cli::cli_progress_bar("Calculating AEP (full uncertainty)", total = ncol(peakStage))
    for (m in 1:ncol(peakStage)) {
      tmpStage <- matrix(peakStage[, m], nrow = Mevents, ncol = Nbins)
      for (i in 1:Sbins) {
        exceed_prop <- colMeans(tmpStage > peakStages[i])
        aepStages[i, m] <- sum(exceed_prop * Weights)
      }
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
  }

  # ============================================================================
  # Create Result DF
  # ============================================================================
  cli::cli_h1("Finalizing Results Data Frame")
  cli::cli_progress_bar(total = 100)
  cli::cli_progress_update(25)
  # AEP Sequence
  aep_sequence <- function(from = 1e-1, to = 1e-8) {
    magnitudes <- 10^seq(log10(from), log10(to), by = -1)
    unlist(lapply(magnitudes, function(x) seq(9, 1) * x))
  }
  cli::cli_progress_update(50)
  # EXPECTED ONLY ++++++
  if(expected_only){
    # Functions for Stages - Gumble
    expected_fun <- approxfun(x = -log(-log(1-aepStages[,1])), y = peakStages)

    # Results DF
    result_df <- data.frame(AEP = c(.99,aep_sequence()),
                            Z = NA,
                            Gumb = NA,
                            Expected = NA)
    # Populate Results
    result_df$Z <- qnorm(1 - result_df$AEP)
    result_df$Gumb <- -log(-log(1 - result_df$AEP))
    result_df$Expected <- expected_fun(result_df$Gumb)
    cli::cli_progress_update(75)

  # FULL UNCERT ++++++
  } else{
    lower_05 <- apply(aepStages, 1, quantile, probs = 0.05)
    median_50 <- apply(aepStages, 1, quantile, probs = 0.50)
    upper_95 <- apply(aepStages, 1, quantile, probs = 0.95)

    # Expected - Is this correct?
    expected <- apply(aepStages, 1, mean)

    # Functions for Stages - Gumble
    upper_95_fun <- approxfun(x = -log(-log(1-upper_95)), y = peakStages)
    lower_95_fun <- approxfun(x = -log(-log(1-lower_05)), y = peakStages)
    median_fun <- approxfun(x = -log(-log(1-median_50)), y = peakStages)
    expected_fun <- approxfun(x = -log(-log(1-expected)), y = peakStages)

    # Results DF
    result_df <- data.frame(AEP = c(.99,aep_sequence()),
                            Z = NA,
                            Gumb = NA,
                            Upper_95 = NA,
                            Lower_95 = NA,
                            Median = NA,
                            Expected = NA)

    # Populate Results
    result_df$Z <- qnorm(1 - result_df$AEP)
    result_df$Gumb <- -log(-log(1 - result_df$AEP))
    result_df$Upper_95 <- upper_95_fun(result_df$Gumb)
    result_df$Lower_95 <- lower_95_fun(result_df$Gumb)
    result_df$Median <- median_fun(result_df$Gumb)
    result_df$Expected <- expected_fun(result_df$Gumb)
    cli::cli_progress_update(75)
  }
  cli::cli_progress_update(100)
  cli::cli_alert_success("Returning Result Data Frame")
  return(result_df)
}
