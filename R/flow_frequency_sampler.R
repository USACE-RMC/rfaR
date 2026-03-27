#' Flow Frequency Sampler
#'
#' Generates a stratified matrix of flow values from a single set of frequency
#' distribution parameters using stratified Monte Carlo sampling. Used internally
#' by [rfaR::rfa_simulate()] for median-only and full uncertainty modes.
#'
#' @param bestfit_params Numeric vector of length 3 containing distribution parameters.
#'   For LP3: `c(mean_log, sd_log, skew_log)`.
#'   For GEV: `c(location, scale, shape)`.
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
#' @seealso [rfaR::stratified_sampler()], [rfaR::qp3()], [rfaR::rfa_simulate()]
#'
#' @examples
#' # Single parameter set (posterior mode)
#' params <- c(4.85, 0.39, -0.15)
#' result <- flow_frequency_sampler(params, freq_dist = "LP3",
#'                                  Nbin = 20, Mevent = 500)
#' dim(result$flow)  # 500 x 20

flow_frequency_sampler <- function(bestfit_params, freq_dist = "LP3",
                                   strat_dist = "ev1",
                                   Nbin = NULL, Mevent = NULL) {

  # IF NO BINS OR EVENTS ARE DEFINED
  if(is.null(Nbin)){
    Nbin = 50
  }
  if(is.null(Mevent)){
    Mevent = 200
  }

  # ============================================================================
  # PROCESS ONE PARAMETER SET
  # ============================================================================
  # CREATE STRATIFIED SAMPLE OF Z ORDINATES TO EST. FLOW
  ords <- stratified_sampler(Nbins = Nbin,
                             Mevents = Mevent,
                             dist = strat_dist)

  # Matrix with 20 bins of 500 events (represented by the z-variate)
  z_matrix <- matrix(ncol = ords$Nbins, nrow = ords$Mevents)

  for (i in 1:ords$Nbins){
    # upper and lower
    bin_lower <- ords$Zlower[i]
    bin_upper <- ords$Zupper[i]

    # Vector of random values
    z_matrix[,i] <- (bin_lower + (runif(ords$Mevents, min = 0, max = 1)) * (bin_upper - bin_lower))
  }

  # ============================================================================
  # DISTRIBUTION
  # ============================================================================
  # LP3 DISTRIBUTION
  if (freq_dist == "LP3") {
    # Params from vector
    meanlog <- bestfit_params[1]
    sdlog <- bestfit_params[2]
    skewlog <- bestfit_params[3]

    # Entirely vectorized
    Q_matrix <- 10^qp3(pnorm(z_matrix), meanlog, sdlog, skewlog)

  } else {

  #GEV DISTRIBUTION - additional distributions will come later
    # Params from vector
    xi <- unlist(unname(bestfit_params[1]))
    alfa <- unlist(unname(bestfit_params[2]))
    k <- unlist(unname(bestfit_params[3]))

    # Uses lmom package
    #Q_matrix <- lmom::quagev(pnorm(z_matrix),c(xi, alfa, k))
    Q_matrix <- pmax(lmom::quagev(pnorm(z_matrix), c(xi, alfa, k)), 1E-6)

  }

  # RETURN
  Nbins <- ords$Nbins
  Mevents <- ords$Mevents
  Weights <- ords$Weights


  return(list(flow = Q_matrix,
              nbins = Nbins,
              mevents = Mevents,
              weights = Weights))
}

