#' Stratified Sampler for Monte Carlo Simulation
#'
#' Creates stratified bins in standard normal space for use in stratified
#' Monte Carlo sampling. Stratification improves sampling efficiency by
#' ensuring adequate coverage of rare events.
#'
#' @param minAEP Minimum annual exceedance probability. Default is `1E-8`.
#' @param maxAEP Maximum annual exceedance probability. Default is `0.99`.
#' @param dist Character. Probability space for stratification. One of:
#'   \describe{
#'     \item{"EV1"}{Extreme Value Type I (Gumbel) space. Recommended for flood
#'       frequency analysis. Allocates more bins to rare events through the
#'       transformation `-log(-log(1-AEP))`, improving tail estimation efficiency.}
#'     \item{"Normal"}{Standard normal (z-score) space. Uniform bins in z-space.
#'       Use when the underlying phenomenon is normally distributed.}
#'     \item{"Uniform"}{Uniform probability space. Equal probability width per bin.
#'       Generally inefficient for rare event estimation.}
#'   }
#'   Default is "EV1". Invalid values trigger a warning and default to "EV1".
#' @param Nbins Number of stratified bins. Default is `20`.
#' @param Mevents Number of events per bin. Default is `500`.
#'
#' @return A list containing:
#' \describe{
#'   \item{normOrd}{Vector of standard normal ordinates spanning all bins}
#'   \item{Weights}{Vector of probability weights for each bin}
#'   \item{Zlower}{Vector of lower bounds for each bin (standard normal)}
#'   \item{Zupper}{Vector of upper bounds for each bin (standard normal)}
#'   \item{Nbins}{Number of bins}
#'   \item{Mevents}{Number of events per bin}
#' }
#'
#' @details
#' The function divides the probability space into bins using the specified
#' transformation. EV1 transformation is recommended for heavy-tailed distributions
#' common in flood frequency analysis, as it naturally allocates more sampling
#' effort to rare events critical for dam safety assessments.
#' @examples
#' # Default stratification
#' strat <- stratified_sampler()
#'
#' # Custom bins and events
#' strat <- stratified_sampler(minAEP = 1E-6, maxAEP = 0.5, dist = "EV1", Nbins = 10, Mevents = 100)
#'
#' @export
stratified_sampler <- function(minAEP = 1E-8, maxAEP = 0.99, dist = "ev1", Nbins = NULL, Mevents = NULL){
  if(is.null(Nbins)){
    Nbins = 20
  }
  if(is.null(Mevents)){
    Mevents = 500
  }

  # Did the user pass one of the distributions?
  dist <- tolower(dist)
  dist_logical = dist %in% c("ev1", "normal", "uniform")

  # If not, set to null (EV1)
  if (dist_logical) {
    dist = dist
  } else {
    dist = NULL
    cli::cli_warn("Unknown stratification selected. Defaulting to EV1")
  }

  if(is.null(dist)){
    dist = "ev1"
  }

  if(dist == "ev1"){
    # EXTREME VALUE 1 --------
    EV1_lower = numeric(Nbins)
    EV1_upper = numeric(Nbins)
    EV1_weights = numeric(Nbins)

    # EV1 Min, Max, Delta
    max_EV1 = -log(-log(1-maxAEP))
    min_EV1 = -log(-log(1-minAEP))
    EV1_delta = (min_EV1 - max_EV1)/Nbins

    # EV1 Stratified Bins & Weights
    EV1_lower[1] = max_EV1
    EV1_upper[1] = EV1_lower[1] + EV1_delta
    EV1_weights[1] = exp(-exp(-EV1_upper[1]))

    for (i in 2:Nbins){
      EV1_lower[i] = EV1_upper[i-1]
      EV1_upper[i] = EV1_lower[i] + EV1_delta
      EV1_weights[i] = exp(-exp(-EV1_upper[i])) - exp(-exp(-EV1_lower[i]))
    }
    EV1_weights[Nbins] = 1 -  exp(-exp(-EV1_lower[Nbins]))

    # Fixed z-variates
    z = seq(from = min(qnorm(exp(-exp(-EV1_lower)))), to = max(qnorm(exp(-exp(-EV1_upper)))), length.out=Nbins*Mevents)

    # Z-variates for upper and lower bins
    return(list(normOrd = z,
                Weights = EV1_weights,
                Zlower = qnorm(exp(-exp(-EV1_lower))),
                Zupper = qnorm(exp(-exp(-EV1_upper))),
                Nbins = Nbins,
                Mevents = Mevents))

  } else if(dist == "normal"){
    # NORMAL --------------
    Z_lower = numeric(Nbins)
    Z_upper = numeric(Nbins)
    Z_weights = numeric(Nbins)

    # Normal Min, Max, Delta
    max_Z = qnorm(1-minAEP)
    min_Z = qnorm(1-maxAEP)
    Z_delta = (max_Z - min_Z)/Nbins

    # Normal Stratified Bins & Weights
    Z_lower[1] = min_Z
    Z_upper[1] = Z_lower[1] + Z_delta
    Z_weights[1] = pnorm(Z_upper[1])

    for (i in 2:Nbins){
      Z_lower[i] = Z_upper[i-1]
      Z_upper[i] = Z_lower[i] + Z_delta
      Z_weights[i] = pnorm(Z_upper[i]) -  pnorm(Z_lower[i])
    }

    Z_weights[Nbins] = 1 -  pnorm(Z_lower[Nbins])

    z = seq(from=min(Z_lower), to=max(Z_upper), length.out=Nbins*Mevents)

    return(list(normOrd = z,
                Weights = Z_weights,
                Zlower = Z_lower,
                Zupper = Z_upper,
                Nbins = Nbins,
                Mevents = Mevents))

  } else if(dist == "uniform"){
    # UNIFORM ------
    U_lower = numeric(Nbins)
    U_upper = numeric(Nbins)
    U_weights = numeric(Nbins)

    # uniform Min, Max, Delta
    U_delta <- (maxAEP - minAEP)/Nbins
    U_lower[1] = maxAEP
    U_upper[1] = U_lower[1] - U_delta
    U_weights[1] = 1 - U_upper[1]

    # Uniform Stratified Bins & Weights
    for (i in 2:Nbins){
      U_lower[i] = U_upper[i-1]
      U_upper[i] = U_lower[i] - U_delta
      U_weights[i] = U_lower[i] -  U_upper[i]
    }

    U_weights[Nbins] = U_lower[Nbins]

    # Fixed z-variates
    z = seq(from = min(qnorm(1-U_lower)), to = max(qnorm(1-U_upper)), length.out=Nbins*Mevents)

    # Z-variates for upper and lower bins
    return(list(normOrd = z,
                Weights = U_weights,
                Zlower = qnorm(1-U_lower),
                Zupper = qnorm(1-U_upper),
                Nbins = Nbins,
                Mevents = Mevents))
  }
}
