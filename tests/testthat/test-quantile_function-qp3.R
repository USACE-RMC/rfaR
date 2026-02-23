test_that("qp3 matches lmom::quape3 for positive skewness", {
  mu <- 3.57
  sigma <- 0.375
  gamma <- 0.54
  probs <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)

  qp3_results <- qp3(probs, mu, sigma, gamma)
  lmom_results <- lmom::quape3(probs, c(mu, sigma, gamma))

  expect_equal(qp3_results, lmom_results, tolerance = 1e-6)
})

test_that("qp3 matches lmom::quape3 for negative skewness", {
  mu <- 4.0
  sigma <- 0.5
  gamma <- -0.8
  probs <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)

  qp3_results <- qp3(probs, mu, sigma, gamma)
  lmom_results <- lmom::quape3(probs, c(mu, sigma, gamma))

  expect_equal(qp3_results, lmom_results, tolerance = 1e-6)
})

test_that("qp3 matches normal distribution when skewness near zero", {
  mu <- 3.5
  sigma <- 0.4
  gamma <- 0.0005
  probs <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)

  qp3_results <- qp3(probs, mu, sigma, gamma)
  normal_results <- qnorm(probs, mean = mu, sd = sigma)

  expect_equal(qp3_results, normal_results, tolerance = 1e-6)
})

test_that("qp3 matches lmom::quape3 using JMD parameters", {
  probs <- c(0.001, 0.01, 0.10, 0.50, 0.90, 0.99, 0.999)

  for (i in 1:min(10, nrow(jmd_bf_parameter_sets))) {
    mu <- jmd_bf_parameter_sets[i, 1]
    sigma <- jmd_bf_parameter_sets[i, 2]
    gamma <- jmd_bf_parameter_sets[i, 3]

    qp3_results <- qp3(probs, mu, sigma, gamma)
    lmom_results <- lmom::quape3(probs, c(mu, sigma, gamma))

    expect_equal(qp3_results, lmom_results, tolerance = 1e-6,
                 label = paste("Parameter set", i))
  }
})

test_that("qp3 is monotonically increasing", {
  mu <- 3.57
  sigma <- 0.375
  gamma <- 0.54
  probs <- seq(0.01, 0.99, by = 0.01)

  results <- qp3(probs, mu, sigma, gamma)

  expect_true(all(diff(results) > 0))
})

test_that("qp3 median equals mu when skewness is zero", {
  mu <- 5.0
  sigma <- 1.0
  # near zero triggers normal approximation
  gamma <- 0.0001

  result <- qp3(0.5, mu, sigma, gamma)

  expect_equal(result, mu, tolerance = 1e-3)
})
