test_that("90% credible intervals (5,95) match those from jmd_vfc", {
  aeps <- jmd_vfc$aep

  ci_matrix <- matrix(nrow = nrow(jmd_bf_parameter_sets), ncol = length(aeps))
  for (i in 1:ncol(ci_matrix)) {
    for (j in 1:nrow(ci_matrix)) {
      ci_matrix[j, i] <- 10^qp3(1 - aeps[i],
                                jmd_bf_parameter_sets[j, 1],
                                jmd_bf_parameter_sets[j, 2],
                                jmd_bf_parameter_sets[j, 3])
    }
  }

  ci_5 <- numeric(ncol(ci_matrix))
  ci_95 <- numeric(ncol(ci_matrix))
  for (i in 1:ncol(ci_matrix)) {
    ci_5[i] <- as.numeric(quantile(ci_matrix[, i], 0.05))
    ci_95[i] <- as.numeric(quantile(ci_matrix[, i], 0.95))
  }

  max_diff_95 <- max(abs(ci_95 - jmd_vfc$ci_95))
  max_diff_5 <- max(abs(ci_5 - jmd_vfc$ci_5))

  # Differences are from interpolation method, not methodology
  expect_lt(max_diff_95, 0.05)
  expect_lt(max_diff_5, 0.05)
})

test_that("posterior predictive AEPs match those from jmd_vfc", {
  aeps <- jmd_vfc$aep
  discharges <- jmd_vfc$posterior_predictive
  log_discharge <- log(discharges, base = 10)

  posterior_matrix <- matrix(nrow = nrow(jmd_bf_parameter_sets), ncol = length(log_discharge))
  for (i in 1:ncol(posterior_matrix)) {
    for (j in 1:nrow(posterior_matrix)) {
      nep <- lmom::cdfpe3(log_discharge[i],
                          c(jmd_bf_parameter_sets[j, 1],
                            jmd_bf_parameter_sets[j, 2],
                            jmd_bf_parameter_sets[j, 3]))
      posterior_matrix[j, i] <- nep
    }
  }

  post_pred <- numeric(ncol(posterior_matrix))
  for (i in 1:ncol(posterior_matrix)) {
    post_pred[i] <- 1 - mean(posterior_matrix[, i])
  }

  max_diff <- max(abs(post_pred - aeps))

  expect_lt(max_diff, 1e-4)
})
