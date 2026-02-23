#' Bootstrap option for VFC (RFA-style w/ ERL)
#'
#' Generates matrix of parameters using posterior mode/mean and ERL
#'
#' @param bestfit_postmode Vector of distribution parameters from RMC-BestFit.
#'   For LP3: columns are mean (log), sd (log), skew (log).
#'   For GEV: columns are location, scale, shape.
#' @param dist Distribution type. Either `"LP3"` (default) or `"GEV"`.
#' @param ERL psuedo effective record length for bootstrapping
#' @param Nboots number of bootstraps
#'
#' @return A list containing:
#' \describe{
#'   \item{params}{Matrix of bootstrapped parameters}
#'   \item{dist}{Distribution selected (LP3 or GEV)}
#'   \item{postmode}{Posterior mode parameters provided to bootstrap}
#'   \item{ERL}{pseudo effective record length provided to bootstrap}
#' }
#'
#' @export
#'
#' @seealso [flow_frequency_sampler()]
#'
#' @examples
#' # Sample using JMD VFC parameters
#' # samples <- flow_frequency_sampler(jmd_vfc_parameters)
bootstrap_vfc <- function(bestfit_postmode, dist = "LP3", ERL=150, Nboots=10000) {

  if(!(dist %in% c("LP3","GEV"))){
    errorCondition("Specify either LP3 or GEV as distribution. ")
  }

  # do the damn thing
  params.out = matrix(NA,nrow=Nboots,ncol=3)

  # random quantiles for bootstrap
  quants = matrix(runif(n=Nboots*ERL, min=0, max=1), nrow=Nboots, ncol=ERL)

  if(dist == "LP3"){
    # nested functions: qp3 transforms quantiles to LP3 space; samlmu computes sample L-moments of LP3 sample; pelpe3 converts L-moments to LP3 parameters
    params.out = t(apply(quants, 1, function(x) lmom::pelpe3(lmom::samlmu(qp3(p=x, mu=as.numeric(bestfit_postmode[1]), sigma=as.numeric(bestfit_postmode[2]), gamma=as.numeric(bestfit_postmode[3]))))))
  }
  if(dist == "GEV"){
    # nested functions: quagev transforms quantiles to GEV space; samlmu computes sample L-moments of GEV sample; pelgev converts L-moments to GEV parameters
    params.out = t(apply(quants, 1, function(x) lmom::pelgev(lmom::samlmu(lmom::quagev(f=x, para=bestfit_postmode)))))
  }

  # append posterior mode to top of matrix
  colnames(params.out) = colnames(bestfit_postmode)
  params.out = rbind(bestfit_postmode, params.out)

  # send the damn samples
  return(list(params = params.out,
              dist = dist,
              postmode = bestfit_postmode,
              ERL = ERL))
}

