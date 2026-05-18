#' Thin rejection sampling output for plotting
#'
#' Thins a large sample of sorted z-scores and stages for plotting efficiency.
#' Retains 5,000 evenly-spaced points across the full range plus the last
#' 10,000 points for dense tail coverage.
#'
#' @param n            Integer. Total number of samples.
#' @param z_sorted     Numeric vector of sorted z-scores (ascending), length \code{n}.
#' @param stage_sorted Numeric vector of sorted stage values (ascending), length \code{n}.
#'
#' @return A data frame with columns \code{z_aep} and \code{stage}.
#'
#' @keywords internal
thin_samples <- function(n, z_sorted, stage_sorted) {
  thin_idx <- unique(c(
    round(seq(1, n, length.out = 5000)),
    (n - 10000):n
  ))
  thin_idx <- sort(unique(thin_idx))
  thin_idx <- thin_idx[thin_idx >= 1 & thin_idx <= n]
  data.frame(
    z_aep = z_sorted[thin_idx],
    stage = stage_sorted[thin_idx]
  )
}

#' Parameterize a Three-Parameter Lognormal PMF for Stage
#'
#' Computes the parameters of a three-parameter lognormal distribution for use
#' as a probabilistic maximum stage (PMF) in rejection sampling. The shift
#' parameter defines the hard lower bound of the distribution. The function
#' supports two modes: (1) supplying a best estimate and assuming sigma, or
#' (2) supplying a best estimate, low, and high to solve for sigma numerically.
#'
#' @param pmf_shift Numeric. Hard lower bound (shift) of the lognormal
#'   distribution. Must be less than all of \code{pmf_best}, \code{pmf_low},
#'   and \code{pmf_high}.
#' @param pmf_best Numeric. Best estimate (mean) of the PMF stage distribution.
#' @param pmf_sigma Numeric. Standard deviation on the log scale. Required if
#'   \code{pmf_low} and \code{pmf_high} are not supplied. Defaults to
#'   \code{NULL}.
#' @param pmf_low Numeric. Low estimate of PMF stage (5th percentile). Optional.
#'   If supplied, \code{pmf_high} must also be supplied.
#' @param pmf_high Numeric. High estimate of PMF stage (95th percentile).
#'   Optional. If supplied, \code{pmf_low} must also be supplied.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{pmf_shift}{Hard lower bound of the distribution.}
#'   \item{pmf_best}{Best estimate of PMF stage.}
#'   \item{pmf_sigma}{Standard deviation on the log scale.}
#'   \item{pmf_mu}{Derived location parameter on the log scale.}
#'   \item{pmf_p05}{5th percentile of the PMF stage distribution.}
#'   \item{pmf_p95}{95th percentile of the PMF stage distribution.}
#' }
#'
#' @examples
#' # Mode 1: assume sigma
#' pmf <- pmf_stage_lognormal(pmf_shift = 239, pmf_best = 241.9, pmf_sigma = 0.5)
#'
#' # Mode 2: solve for sigma from low/high estimates
#' pmf <- pmf_stage_lognormal(pmf_shift = 239, pmf_best = 241.9,
#'                             pmf_low = 239, pmf_high = 245)
#' @export
pmf_stage_lognormal <- function(pmf_shift, pmf_best,
                                pmf_sigma = NULL,
                                pmf_low   = NULL,
                                pmf_high  = NULL) {

  # Use inputs to determine which functional mode.
  # No pmf_low or pmf high? -> Must assume sigma = 0.5
  if (!is.null(pmf_low) && !is.null(pmf_high)) {
    if (pmf_shift > pmf_low) {
      cli::cli_abort(c(
        "{.arg pmf_shift} must be less than {.arg pmf_low}.",
        "x" = "{.arg pmf_shift} is {.val {pmf_shift}} but {.arg pmf_low} is {.val {pmf_low}}."
      ))
    }
    if (pmf_low >= pmf_best) {
      cli::cli_abort(c(
        "{.arg pmf_low} must be less than {.arg pmf_best}.",
        "x" = "{.arg pmf_low} is {.val {pmf_low}} but {.arg pmf_best} is {.val {pmf_best}}."
      ))
    }
    if (pmf_best >= pmf_high) {
      cli::cli_abort(c(
        "{.arg pmf_best} must be less than {.arg pmf_high}.",
        "x" = "{.arg pmf_best} is {.val {pmf_best}} but {.arg pmf_high} is {.val {pmf_high}}."
      ))
    }

    # --- Mode 2: solve for sigma ---
    sigma_obj <- function(sigma) {
      mu          <- log(pmf_best - pmf_shift) - sigma^2 / 2
      implied_p05 <- pmf_shift + qlnorm(0.05, mu, sigma)
      implied_p95 <- pmf_shift + qlnorm(0.95, mu, sigma)
      (implied_p05 - pmf_low)^2 + (implied_p95 - pmf_high)^2
    }
    pmf_sigma <- optimise(sigma_obj, interval = c(1e-6, 10))$minimum

  } else {

    # --- Mode 1: sigma supplied ---
    if (is.null(pmf_sigma)) {
      cli::cli_abort(
        "Either {.arg pmf_sigma} or both {.arg pmf_low} and {.arg pmf_high} must be supplied."
      )
    }
    if (pmf_sigma <= 0) {
      cli::cli_abort(c(
        "{.arg pmf_sigma} must be positive.",
        "x" = "{.arg pmf_sigma} is {.val {pmf_sigma}}."
      ))
    }
    if (pmf_shift >= pmf_best) {
      cli::cli_abort(c(
        "{.arg pmf_shift} must be less than {.arg pmf_best}.",
        "x" = "{.arg pmf_shift} is {.val {pmf_shift}} but {.arg pmf_best} is {.val {pmf_best}}."
      ))
    }
  }

  # --- Derive mu and quantiles ---
  pmf_mu  <- log(pmf_best - pmf_shift) - (pmf_sigma^2 / 2)
  pmf_p05 <- pmf_shift + qlnorm(0.05, pmf_mu, pmf_sigma)
  pmf_p95 <- pmf_shift + qlnorm(0.95, pmf_mu, pmf_sigma)

  list(
    pmf_shift = pmf_shift,
    pmf_best  = pmf_best,
    pmf_sigma = pmf_sigma,
    pmf_mu    = pmf_mu,
    pmf_p05   = pmf_p05,
    pmf_p95   = pmf_p95
  )
}

#' Rejection Sampling of Stage Bounded by a Probabilistic Maximum Stage
#'
#' Draws \code{n_samples} stage values from a stage-frequency curve, rejecting
#' any draws that exceed a probabilistic maximum stage (PMF) sampled from a
#' three-parameter lognormal distribution. Accepted samples are ranked and
#' assigned Weibull plotting positions for use in frequency analysis.
#'
#' @param pmf_stage_LN A named list returned by \code{\link{pmf_stage_lognormal}}
#'   containing the lognormal PMF parameters.
#' @param stage_freq_df A data frame with AEP in column 1 and stage in column 2.
#'   Mutually exclusive with \code{aep} and \code{stage}.
#' @param aep Numeric vector of annual exceedance probabilities. Must be
#'   supplied with \code{stage}. Mutually exclusive with \code{stage_freq_df}.
#' @param stage Numeric vector of stages corresponding to \code{aep}. Must be
#'   supplied with \code{aep}. Mutually exclusive with \code{stage_freq_df}.
#' @param n_samples Integer. Number of samples to draw. Defaults to \code{1e7}.
#'
#' @return A data frame of thinned accepted samples with columns for z-score
#'   and stage, suitable for plotting a probabilistically bounded stage-frequency
#'   curve. Output is produced by \code{thin_samples()}.
#'
#' @seealso \code{\link{pmf_stage_lognormal}}
#'
#' @examples
#' \dontrun{
#' pmf_ln <- pmf_stage_lognormal(pmf_shift = 239, pmf_best = 241.9, pmf_sigma = 0.5)
#'
#' result <- rejection_sampling_stage(
#'   pmf_stage_LN  = pmf_ln,
#'   stage_freq_df = my_stage_freq_df,
#'   n_samples     = 1e7
#' )
#' }
#'
#' @export
rejection_sampling_stage <- function(pmf_stage_LN, stage_freq_df = NULL, aep = NULL, stage = NULL, n_samples = 1e7){
  # Resolve inputs
  if (!is.null(stage_freq_df)) {
    if (!is.null(aep) || !is.null(stage)) {
      cli::cli_abort("Provide either {.arg stage_freq_df} or {.arg aep}/{.arg stage}, not both.")
    }
    aep_vals   <- stage_freq_df[[1]]
    stage_vals <- stage_freq_df[[2]]

  } else if (!is.null(aep) && !is.null(stage)) {
    aep_vals   <- aep
    stage_vals <- stage

  } else {
    cli::cli_abort(c(
      "Must provide either {.arg stage_freq_df} or both {.arg aep} and {.arg stage}.",
      "i" = "Supply a data frame via {.arg stage_freq_df}, or pass numeric vectors to {.arg aep} and {.arg stage} directly."
    ))
  }

  # PMF Log-Normal Parameters ==================================================
  pmf_shift <- pmf_stage_LN[["pmf_shift"]]
  pmf_mu    <- pmf_stage_LN[["pmf_mu"]]
  pmf_sigma <- pmf_stage_LN[["pmf_sigma"]]

  # Transform to z-space for interpolation function ============================
  z_aep <- qnorm(1-aep_vals)
  zaep_stage <- approxfun(z_aep, stage_vals, rule = 2)

  # First sample rejection =====================================================
  # Sample AEPs
  aep_draws <- runif(n_samples, min = 1e-9, max = 1 - 1e-9)

  # Return Stage corresponding to sampled AEP (as z-value)
  stage_draws <- zaep_stage(qnorm(1 - aep_draws))

  # Randomly Sample PMF Stage from log-normal above
  pmf_draws <- pmf_shift + rlnorm(n_samples, pmf_mu, pmf_sigma)

  # Reject (aep,stage) where the stage exceeds sampled PMF Stage
  reject_idx <- which(stage_draws > pmf_draws)

  # Rejection Iterations =======================================================
  iter <- 0
  while (length(reject_idx) > 0) {
    # Number of rejects
    n_rej <- length(reject_idx)
    # New AEP sample
    aep_new <- runif(n_rej, min = 1e-9, max = 1 - 1e-9)
    # Corresponding stage sample
    stage_new <- zaep_stage(qnorm(1 - aep_new))
    # New PMF Stage sample
    pmf_new <- pmf_shift + rlnorm(n_rej, pmf_mu, pmf_sigma)
    # Replace rejections with resampled aep,stage
    stage_draws[reject_idx] <- stage_new
    pmf_draws[reject_idx] <- pmf_new
    # Next rejection index
    reject_idx <- reject_idx[stage_new > pmf_new]
    # Count iterations
    iter <- iter + 1
    if (iter >= 1E3) {
      cli::cli_warn(c(
        "Rejection sampling did not converge after {iter} iterations.",
        "i" = "{length(reject_idx)} sample{?s} remain unresolved.",
        "i" = "Check that {.arg pmf_stage_LN} is consistent with the provided stage-frequency curve."
      ))
      break
    }
  }

  # Assign Weibull to the accepted samples =====================================
  # Sort Stages
  stage_sorted <- sort(stage_draws)

  # Assign Weibull pp
  weibull_aep <- 1 - seq_len(n_samples) / (n_samples + 1)

  # Convert to z_aep
  z_sorted <- qnorm(1 - weibull_aep)

  # Thin for plotting purposes
  probabilistic_bounded <- thin_samples(n_samples, z_sorted, stage_sorted)
  return(probabilistic_bounded)
}
