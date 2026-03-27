#' Flow Frequency Sampler (Expected Only)
#'
#' Generates a stratified matrix of flow values by pairing each z-ordinate with
#' a different parameter set from the posterior distribution. This collapses the
#' nested Monte Carlo structure into a single pass, simultaneously sampling
#' natural variability (via stratified z-ordinates) and knowledge uncertainty
#' (via varying parameters). Used internally by [rfaR::rfa_simulate()] for expected-only
#' mode.
#'
#' @param bestfit_params Data frame of distribution parameters from RMC-BestFit.
#'   Must have `Nbin * Mevent` rows (one parameter set per z-ordinate).
#'   For LP3: columns are mean (log), sd (log), skew (log).
#'   For GEV: columns are location, scale, shape.
#' @param freq_dist Character. Distribution type. Either `"LP3"` (default) or `"GEV"`.
#' @param strat_dist Character. Probability space for stratification bins.
#'   Passed to [rfaR::stratified_sampler()]. One of `"ev1"` (default), `"normal"`,
#'   or `"uniform"`. See [rfaR::stratified_sampler()] for details.
#' @param Nbin Integer. Number of stratified bins. Default is `50`.
#' @param Mevent Integer. Number of events per bin. Default is `200`.
#'
#' @return A list containing:
#' \describe{
#'   \item{flow}{Matrix of sampled flow values `[Mevent x Nbin]`}
#'   \item{nbins}{Number of stratified bins}
#'   \item{mevents}{Number of events per bin}
#'   \item{weights}{Probability weights for each bin from [rfaR::stratified_sampler()]}
#' }
#'
#' @export
#'
#' @seealso [rfaR::stratified_sampler()], [rfaR::qp3()], [rfaR::flow_frequency_sampler()], [rfaR::rfa_simulate()]
#'
#' @examples
#' # Expected only using all 10,000 parameter sets
#' result <- flow_frequency_sampler_expected(jmd_bf_parameter_sets,
#'                                           Nbin = 20, Mevent = 500)
#' dim(result$flow)  # 500 x 20
#'
#' #' # Using bootstrapped parameter samples
#' jmd_samples <- bootstrap_vfc(
#'   c(jmd_vfc_parameters$mean_log,
#'     jmd_vfc_parameters$sd_log,
#'     jmd_vfc_parameters$skew_log),
#'   dist = "LP3",
#'   ERL  = jmd_vfc_parameters$erl)
#'
#' jmd_result <- flow_frequency_sampler_expected(
#'   jmd_samples$params,
#'   freq_dist = "LP3",
#'   Nbin      = 20,
#'   Mevent    = 500)
flow_frequency_sampler_expected <- function(bestfit_params, freq_dist = "LP3",
                                            strat_dist = "ev1",
                                            Nbin = NULL, Mevent = NULL) {

  # IF NO BINS OR EVENTS ARE DEFINED
  if (is.null(Nbin)) {
    Nbin <- 50
  }
  if (is.null(Mevent)) {
    Mevent <- 200
  }

  if (nrow(bestfit_params) < Nbin * Mevent) {
    cli::cli_abort("bestfit_params has {nrow(bestfit_params)} rows but Nbin * Mevent requires {Nbin * Mevent}")
  }

  # ==========================================================================
  # EXPECTED ONLY
  # ==========================================================================

  # CREATE STRATIFIED SAMPLE OF Z ORDINATES TO EST. FLOW
  ords <- stratified_sampler(Nbins = Nbin,
                             Mevents = Mevent,
                             dist = strat_dist)

  # Z-variate matrix [Mevent x Nbin]
  z_matrix <- matrix(ncol = ords$Nbins, nrow = ords$Mevents)

  for (i in 1:ords$Nbins) {
    # upper and lower
    bin_lower <- ords$Zlower[i]
    bin_upper <- ords$Zupper[i]

    # Vector of random values
    z_matrix[, i] <- bin_lower + runif(ords$Mevents) * (bin_upper - bin_lower)
  }

  # ==========================================================================
  # DISTRIBUTION - iterate each z-variate with a different parameter set
  # ==========================================================================

  Q_matrix <- matrix(ncol = ords$Nbins, nrow = ords$Mevents)

  if (freq_dist == "LP3") {
    for (j in 1:ncol(z_matrix)) {
      for (i in 1:nrow(z_matrix)) {
        # Z-variate
        z_var <- z_matrix[i, j]

        # Parameter set (1-to-1 mapping)
        idx <- (j - 1) * nrow(z_matrix) + i

        # Sampled Flow
        Q_matrix[i, j] <- 10^qp3(pnorm(z_var),
                                 bestfit_params[idx, 1],
                                 bestfit_params[idx, 2],
                                 bestfit_params[idx, 3])
      }
    }
  } else {
    # GEV DISTRIBUTION - additional distributions will come later
    for (j in 1:ncol(z_matrix)) {
      for (i in 1:nrow(z_matrix)) {
        # Z-variate
        z_var <- z_matrix[i, j]

        # Parameter set (1-to-1 mapping)
        idx <- (j - 1) * nrow(z_matrix) + i

        # Sampled Flow (lmom package)
        Q_matrix[i, j] <- max(lmom::quagev(pnorm(z_var),
                                           c(bestfit_params[idx, 1],
                                             bestfit_params[idx, 2],
                                             bestfit_params[idx, 3])), 1E-6)

      }
    }
  }

  # RETURN
  return(list(flow = Q_matrix,
              nbins = ords$Nbins,
              mevents = ords$Mevents,
              weights = ords$Weights))
}
