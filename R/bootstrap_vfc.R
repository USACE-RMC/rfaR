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
#' @seealso [rfaR::flow_frequency_sampler()]
#'
#' @examples
#' # Sample using JMD VFC parameters
#' jmd_samples <- bootstrap_vfc(c(jmd_vfc_parameters$mean_log,
#'                                 jmd_vfc_parameters$sd_log,
#'                                 jmd_vfc_parameters$skew_log),
#'                                 dist = "LP3",
#'                                 ERL = jmd_vfc_parameters$erl)
#'
#' gev_example <- c(3.0, 1.0, -0.1)
#' gev_samples <- bootstrap_vfc(gev_example,
#'                               dist = "GEV",
#'                               ERL = 200,
#'                               Nboots = 5000)
#' hist(gev_samples$params[,1])
#'
#' lp3_example <- c(3.5, 0.22, 0.1)
#' lp3_samples <- bootstrap_vfc(lp3_example,
#'                               dist = "LP3",
#'                               ERL = 300,
#'                               Nboots = 1000)
#' hist(lp3_samples$params[,3])
bootstrap_vfc <- function(bestfit_postmode, dist = "LP3", ERL = 150, Nboots = 10000) {

  if(!(dist %in% c("LP3","GEV"))){
    errorCondition("Specify either LP3 or GEV as distribution. ")
  }

  # do the damn thing
  params.out = matrix(NA, nrow = Nboots, ncol = 3)

  # random quantiles for bootstrap
  quants = matrix(runif(n = Nboots*ERL, min = 0, max = 1), nrow = Nboots, ncol = ERL)

  if(dist == "LP3"){
    # nested functions: qp3 transforms quantiles to LP3 space; samlmu computes sample L-moments of LP3 sample; pelpe3 converts L-moments to LP3 parameters
    params.out = t(apply(quants, 1, function(x) lmom::pelpe3(lmom::samlmu(qp3(p = x,
                                                                              mu = as.numeric(bestfit_postmode[1]),
                                                                              sigma = as.numeric(bestfit_postmode[2]),
                                                                              gamma = as.numeric(bestfit_postmode[3]))))))
    colnames(params.out) = c("mean_log","sd_log","skew_log")
  }
  if(dist == "GEV"){
    # nested functions: quagev transforms quantiles to GEV space; samlmu computes sample L-moments of GEV sample; pelgev converts L-moments to GEV parameters
    params.out = t(apply(quants, 1, function(x) lmom::pelgev(lmom::samlmu(lmom::quagev(f = x, para = bestfit_postmode)))))
    colnames(params.out) = c("location","scale","shape")
  }

  # Removing this for now - can call the postmode from the list if it's desired
  # params.out = rbind(bestfit_postmode, params.out)

  # return samples and parent distros
  return(list(params = data.frame(params.out),
              dist = dist,
              postmode = bestfit_postmode,
              ERL = ERL))
}

