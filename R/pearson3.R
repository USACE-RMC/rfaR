### Pearson Type III Distribution Functions ###

#' Pearson Type III Probability Density Function
#'
#' Density function for the Pearson Type III distribution.
#'
#' @param x Vector of quantiles (in log10 space for Log-Pearson III)
#' @param mu Mean
#' @param sigma Standard deviation
#' @param gamma Skewness coefficient
#'
#' @return Vector of densities
#' @export
#'
#' @examples
#' # Single value
#' dp3(4.5, mu = 4.232, sigma = 0.153, gamma = 0.401)
#'
#' # Vector of values
#' x = seq(3.5, 5.0, length.out = 100)
#' d = dp3(x, mu = 4.232, sigma = 0.153, gamma = 0.401)
#' plot(x, d, type = "l")
dp3 = function(x, mu, sigma, gamma) {
  # Convert to gamma parameters
  xi = mu - 2 * sigma / gamma
  beta = 0.5 * sigma * gamma
  alpha = 4 / gamma^2

  # Get min and max support
  min_val = -.Machine$double.xmax
  if(beta > 0) min_val = xi
  max_val = .Machine$double.xmax
  if(beta < 0) max_val = xi

  # Initialize result
  result = numeric(length(x))

  # Check if normal
  if (abs(gamma) < 1e-3) {
    return(dnorm(x = x, mean = mu, sd = sigma))
  }

  if (beta > 0) {
    # Positive beta case
    result = ifelse(x < min_val | x > max_val, 0,
                    dgamma(x = x - xi, shape = alpha, scale = abs(beta)))
  } else {
    # Negative beta case
    result = ifelse(x < min_val | x > max_val, 0,
                    dgamma(x = xi - x, shape = alpha, scale = abs(beta)))
  }

  return(result)
}

#' Pearson Type III Cumulative Distribution Function
#'
#' Distribution function for the Pearson Type III distribution.
#'
#' @param x Vector of quantiles (in log10 space for Log-Pearson III)
#' @param mu Mean
#' @param sigma Standard deviation
#' @param gamma Skewness coefficient
#'
#' @return Vector of probabilities
#' @export
#'
#' @examples
#' # Single value
#' pp3(4.5, mu = 4.232, sigma = 0.153, gamma = 0.401)
#'
#' # Vector of values
#' x = seq(3.5, 5.0, length.out = 100)
#' p = pp3(x, mu = 4.232, sigma = 0.153, gamma = 0.401)
#' plot(x, p, type = "l")
pp3 = function(x, mu, sigma, gamma) {
  # Convert to gamma parameters
  xi = mu - 2 * sigma / gamma
  beta = 0.5 * sigma / gamma
  alpha = 4 / gamma^2

  # Get min and max support
  min_val = -.Machine$double.xmax
  if(beta > 0) min_val = xi
  max_val = .Machine$double.xmax
  if(beta < 0) max_val = xi

  # Initialize result
  result = numeric(length(x))

  # Check if normal
  if (abs(gamma) < 1e-3) {
    return(pnorm(q = x, mean = mu, sd = sigma))
  }

  if (beta > 0) {
    # Positive beta case
    result = ifelse(x <= min_val, 0,
                    ifelse(x >= max_val, 1,
                           pgamma(q = x - xi, shape = alpha, scale = abs(beta))))
  } else {
    # Negative beta case
    result = ifelse(x <= min_val, 0,
                    ifelse(x >= max_val, 1,
                           1 - pgamma(q = xi - x, shape = alpha, scale = abs(beta))))
  }

  return(result)
}

#' Pearson Type III Quantile Function
#'
#' Quantile function (inverse CDF) for the Pearson Type III distribution.
#'
#' @param p Vector of probabilities
#' @param mu Mean
#' @param sigma Standard deviation
#' @param gamma Skewness coefficient
#'
#' @return Vector of quantiles (in log10 space for Log-Pearson III)
#' @export
#'
#' @examples
#' # Single value
#' qp3(0.5, mu = 4.232, sigma = 0.153, gamma = 0.401)
#'
#' # Vector of values
#' p = seq(0.01, 0.99, length.out = 100)
#' q = qp3(p, mu = 4.232, sigma = 0.153, gamma = 0.401)
#' plot(p, q, type = "l")
qp3 = function(p, mu, sigma, gamma) {
  # Convert to gamma parameters
  xi = mu - 2 * sigma / gamma
  beta = 0.5 * sigma / gamma
  alpha = 4 / gamma^2

  # Get min and max support
  min_val = -.Machine$double.xmax
  if(beta > 0) min_val = xi
  max_val = .Machine$double.xmax
  if(beta < 0) max_val = xi

  # Check if normal
  if (abs(gamma) < 1e-3) {
    return(qnorm(p = p, mean = mu, sd = sigma))
  }

  # Initialize result
  result = numeric(length(p))

  if (beta > 0) {
    # Positive beta case
    result = ifelse(p == 0, min_val,
                    ifelse(p == 1, max_val,
                           xi + qgamma(p = p, shape = alpha, scale = abs(beta))))
  } else {
    # Negative beta case
    result = ifelse(p == 0, min_val,
                    ifelse(p == 1, max_val,
                           xi - qgamma(p = 1 - p, shape = alpha, scale = abs(beta))))
  }

  return(result)
}
