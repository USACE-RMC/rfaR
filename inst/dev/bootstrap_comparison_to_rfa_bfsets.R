aeps <- jmd_vfc$aep

# 9 and 95 CI from parameter sets
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

# Boostrap Parameter sets 5 & 95 CI
jmd_bootstrap <- bootstrap_vfc(c(jmd_vfc_parameters$mean_log,jmd_vfc_parameters$sd_log,jmd_vfc_parameters$skew_log),
                               dist = "LP3", ERL = jmd_vfc_parameters$erl)

boot_ci_matrix <- matrix(nrow = nrow(jmd_bootstrap$params), ncol = length(aeps))
for (i in 1:ncol(boot_ci_matrix)) {
  for (j in 1:nrow(boot_ci_matrix)) {
    boot_ci_matrix[j, i] <- 10^qp3(1 - aeps[i],
                                   jmd_bootstrap$params[j, 1],
                                   jmd_bootstrap$params[j, 2],
                                   jmd_bootstrap$params[j, 3])
  }
}

boot_ci_5 <- numeric(ncol(boot_ci_matrix))
boot_ci_95 <- numeric(ncol(boot_ci_matrix))
for (i in 1:ncol(boot_ci_matrix)) {
  boot_ci_5[i] <- as.numeric(quantile(boot_ci_matrix[, i], 0.05))
  boot_ci_95[i] <- as.numeric(quantile(boot_ci_matrix[, i], 0.95))
}

# AEPS of predictive curve
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

# Bootstrap aeps
jmd_bootstrap <- bootstrap_vfc(c(jmd_vfc_parameters$mean_log,jmd_vfc_parameters$sd_log,jmd_vfc_parameters$skew_log),
                               dist = "LP3", ERL = jmd_vfc_parameters$erl)

boot_posterior_matrix <- matrix(nrow = nrow(jmd_bootstrap$params), ncol = length(log_discharge))
for (i in 1:ncol(boot_posterior_matrix)) {
  for (j in 1:nrow(boot_posterior_matrix)) {
    boot_nep <- lmom::cdfpe3(log_discharge[i],
                             c(jmd_bootstrap$params[j, 1],
                               jmd_bootstrap$params[j, 2],
                               jmd_bootstrap$params[j, 3]))
    boot_posterior_matrix[j, i] <- boot_nep
  }
}

boot_post_pred <- numeric(ncol(boot_posterior_matrix))
for (i in 1:ncol(boot_posterior_matrix)) {
  boot_post_pred[i] <- 1 - mean(boot_posterior_matrix[, i])
}

# Data above
bootstrap_df <- jmd_bootstrap$params
colnames(bootstrap_df) <- c("mean_log","sd_log","skew_log")

ggplot() +
  geom_density(data = bootstrap_df, aes(x = mean_log),fill = "lightblue", alpha = 0.5) +
  geom_density(data = jmd_bf_parameter_sets, aes(x = mean_log),fill = "lightgreen", alpha = 0.5)

ggplot() +
  geom_density(data = bootstrap_df, aes(x = sd_log),fill = "lightblue", alpha = 0.5) +
  geom_density(data = jmd_bf_parameter_sets, aes(x = sd_log),fill = "lightgreen", alpha = 0.5)

ggplot() +
  geom_density(data = bootstrap_df, aes(x = skew_log),fill = "lightblue", alpha = 0.5) +
  geom_density(data = jmd_bf_parameter_sets, aes(x = skew_log),fill = "lightgreen", alpha = 0.5)

# Predictive Lines
ci_df <- data.frame(z_aeps = qnorm(1 - jmd_vfc$aep),
                    rfa_ci5 = jmd_vfc$ci_5,
                    rfa_ci95 = jmd_vfc$ci_95,
                    pset_ci5 = ci_5,
                    pset_ci95 = ci_95,
                    boot_ci5 = boot_ci_5,
                    boot_ci95 = boot_ci_95)

pred_df <- data.frame(z_aeps = qnorm(1 - jmd_vfc$aep),
                      discharges = jmd_vfc$posterior_predictive,
                      log_discharge = log(discharges, base = 10),
                      pset_z_aeps = qnorm(1 - post_pred),
                      boot_z_aeps = qnorm(1 - boot_post_pred))

ggplot(pred_df) +
  geom_point(aes(x = z_aeps,y = discharges, color = "rfa")) +
  geom_line(aes(x = z_aeps,y = discharges, color = "rfa")) +
  #geom_point(aes(x = pset_z_aeps,y = discharges, color = "param set")) +
  geom_line(aes(x = pset_z_aeps,y = discharges, color = "param set")) +
  geom_point(aes(x = boot_z_aeps,y = discharges, color = "boot")) +
  geom_line(aes(x = boot_z_aeps,y = discharges, color = "boot")) +
  scale_y_log10(breaks = scales::log_breaks(n = 10, base = 10)) +
  theme_bw()

ggplot(ci_df) +
  geom_line(aes(x = z_aeps,y = rfa_ci5, color = "rfa")) +
  geom_line(aes(x = z_aeps,y = rfa_ci95, color = "rfa")) +
  geom_line(aes(x = z_aeps,y = pset_ci5, color = "param set")) +
  geom_line(aes(x = z_aeps,y = pset_ci95, color = "param set")) +
  geom_line(aes(x = z_aeps,y = boot_ci5, color = "boot")) +
  geom_line(aes(x = z_aeps,y = boot_ci95, color = "boot")) +
  scale_y_log10(breaks = scales::log_breaks(n = 10, base = 10)) +
  theme_bw()


