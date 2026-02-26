target_aeps <- c(seq(0.99,0.91,by = -0.01),
                 unlist(lapply(0:-7, function(p) {seq(9 * 10^p, 1 * 10^p, by = -1 * 10^p) / 10})))

# FILES ========================================================================
results_dir <- fs::dir_ls(getwd(),glob = "*full_uncert_testing",recurse = T)

all_files <- list.files(results_dir, pattern = "*\\.rds", full.names = TRUE)
results_list <- lapply(all_files, readRDS)

# CI ===========================================================================
# Convert each realiz stage-aep to target AEPs-Stage
Nrealizations <- length(results_list)

stage_matrix <- interpolate_stage_matrix(results_list, target_aeps)

median_stage   <- apply(stage_matrix, 1, quantile, probs = 0.50, na.rm = TRUE)
lower_05     <- apply(stage_matrix, 1, quantile, probs = 0.05, na.rm = TRUE)
upper_95     <- apply(stage_matrix, 1, quantile, probs = 0.95, na.rm = TRUE)

test_stages <- data.frame(aep = target_aeps,
                          z_aep = qnorm(1 - target_aeps),
                          lower = lower_05,
                          upper = upper_95,
                          median = median_stage)

ggplot(test_stages) +
  geom_ribbon(aes(x = z_aep, ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(x = z_aep, y = median))

# Expected =====================================================================
# Compute first
curve1 <- results_list[[1]]

global_min <- min(sapply(results_list, function(x) min(x$stage)))
global_max <- max(sapply(results_list, function(x) max(x$stage)))

# make length equal mevents
expect_stage_vect <- seq(global_min, global_max, length.out = 500)

aep_interp <- interpolate_aep_matrix(results_list, expect_stage_vect)
expected_aeps <- rowMeans(aep_interp)
expected <- aep2stage(expected_aeps, expect_stage_vect, target_aeps)

test_stages <- data.frame(aep = target_aeps,
                          z_aep = qnorm(1 - target_aeps),
                          expected = expected,
                          lower = lower_05,
                          upper = upper_95,
                          median = median_stage)

ggplot(test_stages) +
  geom_ribbon(aes(x = z_aep, ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(x = z_aep, y = median))+
  geom_line(aes(x = z_aep, y = expected),linewidth = 1)
