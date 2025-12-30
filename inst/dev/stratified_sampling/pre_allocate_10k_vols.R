# Pre-sample all 10,000 volumes at once (vectorized)
n_bins = 50
n_events_per_bin = 200

# AEPS
minAEP = 1E-8
maxAEP = 0.99
maxZ = qnorm(1-minAEP)
minZ = qnorm(1-maxAEP)

# Create all bin boundaries
bin_boundaries = create_bins(minAEP, maxAEP, n_bins)

# Pre-allocate
volumes = numeric(n_bins * n_events_per_bin)

# Parameters
mu_vol <- jmd_vfc_parameters[,1]
sigma_vol <- jmd_vfc_parameters[,2]
gamma_vol <- jmd_vfc_parameters[,3]

# Sample all volumes at once
idx = 1
for (bin in 1:n_bins) {
  p_lower = bin_boundaries[bin]
  p_upper = bin_boundaries[bin + 1]

  # Sample 200 probabilities in this bin
  p_samples = runif(n_events_per_bin, p_lower, p_upper)

  # Get all 200 volumes at once (vectorized)
  volumes[idx:(idx + n_events_per_bin - 1)] = 10^qp3(p_samples, mu_vol, sigma_vol, gamma_vol)

  idx = idx + n_events_per_bin
}

# Test plots -------------------------------------------------------------------
library(ggplot2)
vol_df <- data.frame(vol = volumes,
                     logvol = log10(volumes))

ggplot(data = vol_df) +
  geom_histogram(aes(x = vol))+
  theme_minimal()

ggplot(data = vol_df) +
  geom_histogram(aes(x = logvol))+
  theme_minimal()

ggplot(data = vol_df) +
  geom_histogram(aes(x = vol), fill = "lightblue2", color = "grey", alpha = 0.6) +
  scale_x_log10() +
  theme_minimal()
