#' Pearson Type III Inverse CDF (Quantile Function)
#'
#' Computes quantiles from a Pearson Type III distribution given
#' probabilities and distribution parameters (mean, standard deviation, skewness).
#'
#' NOTE: This is the non-vectorized version of the function (it will be used in a loop)
#' Vectorized version is part of future development.
#'
#' @param p Vector of probabilities (between 0 and 1).
#' @param mu Mean of the distribution.
#' @param sigma Standard deviation of the distribution.
#' @param gamma Skewness coefficient of the distribution.
#'
#' @return Vector of quantiles corresponding to the input probabilities.
#'
#' @details
#' The Pearson Type III distribution is parameterized by mean (`mu`),
#' standard deviation (`sigma`), and skewness (`gamma`). These are converted
#' internally to location, scale, and shape parameters.
#'
#' When skewness is near zero (`abs(gamma) < 1E-3`), the normal distribution
#' is used as an approximation.
#'
#' @export
#'
#' @examples
#' # Single quantile
#' qp3(0.99, mu = 10, sigma = 2, gamma = 0.5)
#'
#' # Multiple quantiles
#' qp3(c(0.5, 0.9, 0.99), mu = 10, sigma = 2, gamma = 0.5)
#'
#' # Use with JMD volume-frequency parameters
#' qp3(0.99,
#'     mu = jmd_vfc_parameters$mean_log,
#'     sigma = jmd_vfc_parameters$sd_log,
#'     gamma = jmd_vfc_parameters$skew_log)

qp3 = function(p, mu, sigma, gamma)
{
  # convert to parameters
  xi = mu-2*sigma/gamma #location
  beta = 0.5*sigma*gamma #scale
  alpha = 4/gamma^2 #shape
  # get min and max support
  min = -.Machine$double.xmax
  if(beta > 0) min = xi
  max = .Machine$double.xmax
  if(beta < 0) max = xi
  # check support
  if (any(p == 0)) break# return(min)
  if (any(p == 1)) break# return(max)
  # check if normal
  if (abs(gamma) < 1E-3)
  {
    return(qnorm(p=p, mean=mu, sd=sigma))
  }
  if (beta > 0)
  {
    return(xi + qgamma(p=p, shape=alpha, scale=abs(beta)))
  }
  else
  {
    return(xi - qgamma(p=1-p, shape=alpha, scale=abs(beta)))
  }
}
