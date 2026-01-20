#' Stratified Sampler for Monte Carlo Simulation
#'
#' Creates stratified bins in standard normal space for use in stratified
#' Monte Carlo sampling. Stratification improves sampling efficiency by
#' ensuring adequate coverage of rare events.
#'
#' @param minAEP Minimum annual exceedance probability. Default is `1E-8`.
#' @param maxAEP Maximum annual exceedance probability. Default is `0.99`.
#' @param Nbins Number of stratified bins. Default is `20`.
#' @param Mevents Number of events per bin. Default is `50`.
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
#' @export
#'
#' @examples
#' # Default stratification
#' strat <- stratified_sampler()
#'
#' # Custom bins and events
#' strat <- stratified_sampler(minAEP = 1E-6, maxAEP = 0.5, Nbins = 10, Mevents = 100)
stratified_sampler <- function(minAEP = 1E-8, maxAEP = 0.99, Nbins = NULL, Mevents = NULL){
  if(is.null(Nbins)){
    Nbins = 50
  }
  if(is.null(Mevents)){
    Mevents = 200
  }

  maxZ = qnorm(1-minAEP)
  minZ = qnorm(1-maxAEP)
  zlower = numeric(Nbins)
  zupper = numeric(Nbins)
  weights = numeric(Nbins)

  # create stratified bins
  delta = (maxZ - minZ)/Nbins
  zlower[1] = minZ
  zupper[1] = zlower[1] + delta
  weights[1] = pnorm(zupper[1])
  for (i in 2:Nbins){
    zlower[i] = zupper[i-1]
    zupper[i] = zlower[i] + delta
    weights[i] = pnorm(zupper[i]) -  pnorm(zlower[i])
  }
  weights[Nbins] = 1 -  pnorm(zlower[Nbins])

  z = seq(from=min(zlower), to=max(zupper), length.out=Nbins*Mevents)

  return(list(normOrd = z,
              Weights = weights,
              Zlower = zlower,
              Zupper = zupper,
              Nbins = Nbins,
              Mevents = Mevents))
}
