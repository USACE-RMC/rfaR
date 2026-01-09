qp3v = function(p, mu, sigma, gamma)
{
  # Convert to parameters
  xi = mu - 2*sigma/gamma      # Vectorized ✓
  beta = 0.5*sigma*gamma       # Vectorized ✓
  alpha = 4/gamma^2            # Vectorized ✓

  # Initialize result vector
  result <- numeric(length(p))

  # Handle p = 0 or p = 1 (edge cases)
  result[p == 0] <- ifelse(beta[p == 0] > 0, xi[p == 0], -.Machine$double.xmax)
  result[p == 1] <- ifelse(beta[p == 1] > 0, .Machine$double.xmax, xi[p == 1])

  # Normal approximation for near-zero skew
  normal_cases <- abs(gamma) < 1E-3 & p > 0 & p < 1
  result[normal_cases] <- qnorm(p[normal_cases],
                                mean = mu[normal_cases],
                                sd = sigma[normal_cases])

  # Positive skew (beta > 0)
  pos_skew <- beta > 0 & abs(gamma) >= 1E-3 & p > 0 & p < 1
  result[pos_skew] <- xi[pos_skew] + qgamma(p[pos_skew],
                                            shape = alpha[pos_skew],
                                            scale = abs(beta[pos_skew]))

  # Negative skew (beta < 0)
  neg_skew <- beta < 0 & abs(gamma) >= 1E-3 & p > 0 & p < 1
  result[neg_skew] <- xi[neg_skew] - qgamma(1 - p[neg_skew],
                                            shape = alpha[neg_skew],
                                            scale = abs(beta[neg_skew]))

  return(result)
}

