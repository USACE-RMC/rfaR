#' Flow Frequency Sampler
#'
#' Generates stratified samples of flow values from a fitted frequency distribution
#' using parameters from RMC-BestFit.
#'
#' @param bestfit_params Data frame of distribution parameters from RMC-BestFit.
#'   For LP3: columns are mean (log), sd (log), skew (log).
#'   For GEV: columns are location, scale, shape.
#' @param dist Distribution type. Either `"LP3"` (default) or `"GEV"`.
#' @param ExpectedOnly Logical. If `TRUE` (default), returns expected value
#'   samples only. If `FALSE`, returns full uncertainty ensemble.
#'
#' @return A list containing:
#' \describe{
#'   \item{flow}{Matrix of sampled flow values}
#'   \item{nbins}{Number of stratified bins}
#'   \item{events_per_bin}{Number of events per bin}
#'   \item{weights}{Probability weights for each bin}
#' }
#'
#' @export
#'
#' @seealso [stratified_sampler()], [qp3()]
#'
#' @examples
#' # Sample using JMD VFC parameters
#' # samples <- flow_frequency_sampler(jmd_vfc_parameters)
flow_frequency_sampler <- function(bestfit_params, dist = "LP3", Nbin = NULL, Mevent = NULL, ExpectedOnly = TRUE) {
  # IF NO BINS OR EVENTS ARE DEFINED
  if(is.null(Nbin)){
    Nbin = 50
  }
  if(is.null(Mevent)){
    Mevent = 200
  }

  # ============================================================================
  # EXPECTED ONLY
  # ============================================================================
  if (ExpectedOnly) {
    # CREATE STRATIFIED SAMPLE OF Z ORDINATES TO EST. FLOW
    ords <- stratified_sampler(Nbins = Nbin,
                               Mevents = ceiling(nrow(bestfit_params)/Nbin))

    # Trying less bins
    # ords <- stratified_sampler(Nbins = 20,
    #                            Mevents = 500)

    # EMPTY INFLOW VOL VECTOR
    Q <- numeric(nrow(bestfit_params))

    # LP3 DISTRIBUTION
    if (dist == "LP3") {
      for (i in 1:nrow(bestfit_params)) {
        # Custom PE3 Function
        Q[i] <- 10^qp3(pnorm(ords$normOrd[i]), bestfit_params[i, 1], bestfit_params[i, 2], bestfit_params[i, 3])

        # PE3 from lmom
        # Q[i] <- 10^(lmom::quape3(pnorm(ords$normOrd[i]),
        #                          c(bestfit_params[i, 1],
        #                            bestfit_params[i, 2],
        #                            bestfit_params[i, 3])))
      }
    # ELSE USE GEV (uses lmom) - additional distributions will come later
    } else {
      for (i in 1:nrow(bestfit_params)) {
        # evd package - requires sign switch on K
        # Q[i] <- evd::qgev(pnorm(ords$normOrd), bestfit_params[i, 1], bestfit_params[i, 2], -bestfit_params[i, 3])

        # lmom package
        Q[i] <- lmom::quagev(pnorm(ords$normOrd[i]),
                             c(bestfit_params[i, 1],
                               bestfit_params[i, 2],
                               bestfit_params[i, 3]))
      }
    }
    # SORT INFLOW VOLS
    Q <- matrix(sort(Q), ncol = 1)
  } else {
  # ============================================================================
  # FULL UNCERTAINTY
  # ============================================================================
    # CREATE STRATIFIED SAMPLE OF Z ORDINATES
    ords <- stratified_sampler(Nbins = Nbin, Mevents = Mevent)
    # EMPTY INFLOW VOL VECTOR
    Q <- matrix(NA, nrow = length(ords$normOrd), ncol = nrow(bestfit_params))

    # LP3 DISTRIBUTION
    if (dist == "LP3") {
      for (i in 1:nrow(bestfit_params)) {

        # Custom PE3 Function
        Q[, i] <- 10^qp3(pnorm(ords$normOrd), bestfit_params[i, 1], bestfit_params[i, 2], bestfit_params[i, 3])

        # PE3 from lmom
        # Q[, i] <- 10^(lmom::quape3(pnorm(ords$normOrd[i]),
        #                          c(bestfit_params[i, 1],
        #                            bestfit_params[i, 2],
        #                            bestfit_params[i, 3])))
      }
    # ELSE USE GEV
    } else {
      for (i in 1:nrow(bestfit_params)) {
        # evd package - requires sign switch on K
        # Q[, i] <- evd::qgev(pnorm(ords$normOrd), bestfit_params[i, 1], bestfit_params[i, 2], -bestfit_params[i, 3])

        # lmom package
        Q[,i] <- lmom::quagev(pnorm(ords$normOrd[i]),
                              c(bestfit_params[i, 1],
                                bestfit_params[i, 2],
                                bestfit_params[i, 3]))
      }
    }
  }

  # RETURN
  Nbins <- ords$Nbins
  Mevents <- ords$Mevents
  Weights <- ords$Weights
  return(list(flow = Q,
              nbins = Nbins,
              events_per_bin = Mevents,
              weights = Weights))
}

