### Educational Stratified Sampling for Flood Frequency Analysis ###

# This script demonstrates how stratified sampling improves Monte Carlo estimation
# by ensuring good coverage across both common and rare flood events.

### Setup: Distribution Functions ####################################################

# Pearson Type III distribution functions (PDF, CDF, Inverse CDF)
# These are used to model flood flows - standard in hydrology

dp3 = function(x, mu, sigma, gamma) {
  xi = mu - 2 * sigma / gamma
  beta = 0.5 * sigma * gamma
  alpha = 4 / gamma^2
  min = -.Machine$double.xmax
  if(beta > 0) min = xi
  max = .Machine$double.xmax
  if(beta < 0) max = xi
  if (x < min || x > max) return(0)
  if (abs(gamma) < 1E-3) {
    return(dnorm(x = x, mean = mu, sd = sigma))
  }
  if (beta > 0) {
    return(dgamma(x = x - xi, shape = alpha, scale = abs(beta)))
  } else {
    return(dgamma(x = xi - x, shape = alpha, scale = abs(beta)))
  }
}

pp3 = function(x, mu, sigma, gamma) {
  xi = mu - 2 * sigma / gamma
  beta = 0.5 * sigma / gamma
  alpha = 4 / gamma^2
  min = -.Machine$double.xmax
  if(beta > 0) min = xi
  max = .Machine$double.xmax
  if(beta < 0) max = xi
  if (x <= min) return(0)
  if (x >= max) return(1)
  if (abs(gamma) < 1E-3) {
    return(pnorm(q = x, mean = mu, sd = sigma))
  }
  if (beta > 0) {
    return(pgamma(q = x - xi, shape = alpha, scale = abs(beta)))
  } else {
    return(1 - pgamma(q = xi - x, shape = alpha, scale = abs(beta)))
  }
}

qp3 = function(p, mu, sigma, gamma) {
  # Convert to parameters (same for all elements)
  xi = mu - 2 * sigma / gamma
  beta = 0.5 * sigma / gamma
  alpha = 4 / gamma^2

  # Get min and max support
  min_val = -.Machine$double.xmax
  if(beta > 0) min_val = xi
  max_val = .Machine$double.xmax
  if(beta < 0) max_val = xi

  # Initialize result vector
  result = numeric(length(p))

  # Check if normal
  is_normal = abs(gamma) < 1E-3

  if (is_normal) {
    # All values use normal distribution
    result = qnorm(p = p, mean = mu, sd = sigma)
  } else if (beta > 0) {
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

### Flow-Frequency Distribution Parameters ###########################################

# Pearson Type III parameters for annual peak flows (in log10 space)
# These might come from fitting historical gage data
mu    = 3.5504   # Mean of log10(flow)
sigma = 0.3718   # Standard deviation of log10(flow)
gamma = 0.7555   # Skewness coefficient

cat("Flow-Frequency Distribution (Pearson Type III)\n")
cat("  Mean (log10):    ", mu, "\n")
cat("  Std Dev (log10): ", sigma, "\n")
cat("  Skewness:        ", gamma, "\n\n")

### APPROACH 1: Crude Monte Carlo ####################################################

cat("=== APPROACH 1: CRUDE MONTE CARLO ===\n\n")

# Simple random sampling - just draw random samples and see what happens
n_samples = 1000
set.seed(12345)

# Generate random flows
peak_flows_crude = numeric(n_samples)
for (i in 1:n_samples) {
  # Draw random probability, then get corresponding flow
  random_prob = runif(1)  # Random number between 0 and 1
  peak_flows_crude[i] = qp3(random_prob, mu, sigma, gamma)
}

# Sort in descending order and assign plotting positions
peak_flows_crude = sort(peak_flows_crude, decreasing = TRUE)
plotting_position = (1:n_samples) / (n_samples + 1)
aep_crude = plotting_position  # Annual Exceedance Probability
z_crude = qnorm(1 - aep_crude)  # Convert to standard normal (Z-variate)

# Show some results
cat("Crude Monte Carlo Results (", n_samples, " samples):\n", sep = "")
cat("  Largest flow:  ", 10^peak_flows_crude[1], " cfs (AEP = ",
    aep_crude[1], ")\n", sep = "")
cat("  Smallest flow: ", 10^peak_flows_crude[n_samples], " cfs (AEP = ",
    aep_crude[n_samples], ")\n\n", sep = "")

# The problem: With 1000 samples, we only reliably sample down to about 0.001 AEP
# Rare events (like 0.0001 or 1e-6 AEP) are poorly represented

### APPROACH 2: Stratified Sampling ##################################################

cat("=== APPROACH 2: STRATIFIED SAMPLING ===\n\n")

# Define the range of AEPs we care about
min_aep = 1e-8     # Very rare event (1 in 100 million years)
max_aep = 0.99     # Very common event (99% chance per year)

# Convert to Z-variates (standard normal space)
max_z = qnorm(1 - min_aep)  # ~5.61
min_z = qnorm(1 - max_aep)  # ~-2.33

cat("Analysis Range:\n")
cat("  AEP Range:   ", min_aep, " to ", max_aep, "\n", sep = "")
cat("  Z Range:     ", round(min_z, 2), " to ", round(max_z, 2), "\n\n", sep = "")

# Stratification setup
n_bins = 20         # Number of strata (bins)
n_events_per_bin = 50  # Number of samples within each bin

cat("Stratification:\n")
cat("  Number of bins:          ", n_bins, "\n")
cat("  Events per bin:          ", n_events_per_bin, "\n")
cat("  Total simulated events:  ", n_bins * n_events_per_bin, "\n\n")

# Storage for stratified results
strat_results = matrix(nrow = n_events_per_bin, ncol = n_bins)

# Define bin boundaries and weights
bin_lower = numeric(n_bins)
bin_upper = numeric(n_bins)
bin_weight = numeric(n_bins)

# Create equal-width bins in Z-space
bin_width = (max_z - min_z) / n_bins

for (i in 1:n_bins) {
  bin_lower[i] = min_z + (i - 1) * bin_width
  bin_upper[i] = min_z + i * bin_width

  # Weight = probability that an event falls in this bin
  # This is P(Z < upper) - P(Z < lower)
  bin_weight[i] = pnorm(bin_upper[i]) - pnorm(bin_lower[i])
}

cat("Bin Structure (first 5 and last 5):\n")
cat("Bin |   Z Lower |   Z Upper |    Weight (Prob)\n")
cat("----+-----------+-----------+------------------\n")
for (i in c(1:5, (n_bins-4):n_bins)) {
  cat(sprintf("%3d | %9.3f | %9.3f | %16.10f\n",
              i, bin_lower[i], bin_upper[i], bin_weight[i]))
  if (i == 5) cat("... \n")
}
cat("\n")

# Perform stratified simulation
set.seed(12345)
for (i in 1:n_bins) {
  for (j in 1:n_events_per_bin) {
    # Sample uniformly WITHIN this bin
    z_sample = bin_lower[i] + runif(1) * (bin_upper[i] - bin_lower[i])

    # Convert Z to probability, then to flow
    prob = pnorm(z_sample)
    flow_log10 = qp3(prob, mu, sigma, gamma)

    # Store result
    strat_results[j, i] = flow_log10
  }
}

# Post-process: Create flow-frequency curve from stratified results
# We'll create bins of flow values and count exceedances

min_flow = min(strat_results)
max_flow = max(strat_results)

n_flow_bins = 100  # Number of flow thresholds to evaluate

flow_thresholds = seq(min_flow, max_flow, length.out = n_flow_bins)

aep_stratified = numeric(n_flow_bins)

# For each flow threshold, calculate the exceedance probability
for (i in 1:n_flow_bins) {
  threshold = flow_thresholds[i]

  # Loop through each Z-bin
  for (j in 1:n_bins) {
    # Count how many events in this bin exceed the threshold
    n_exceed = sum(strat_results[, j] > threshold)

    # Probability of exceedance within this bin
    prob_exceed_in_bin = n_exceed / n_events_per_bin

    # Weight by the probability of being in this bin
    aep_stratified[i] = aep_stratified[i] + prob_exceed_in_bin * bin_weight[j]
  }
}

z_stratified = qnorm(1 - aep_stratified)

### Comparison and Plotting ##########################################################
library(ggplot2)
library(dplyr)
library(tidyr)

# Create analytical curve for reference
z_analytical = seq(min_z, max_z, length.out = 200)
flows_analytical = 10^qp3(pnorm(z_analytical), mu, sigma, gamma)

# Prepare data for plotting
plot_data = bind_rows(
  tibble(
    z = z_crude,
    flow = 10^peak_flows_crude,
    method = "Crude MC",
    type = "Simulated"
  ),
  tibble(
    z = z_stratified,
    flow = 10^flow_thresholds,
    method = "Stratified",
    type = "Simulated"
  ),
  tibble(
    z = z_analytical,
    flow = flows_analytical,
    method = "Crude MC",
    type = "Analytical"
  ),
  tibble(
    z = z_analytical,
    flow = flows_analytical,
    method = "Stratified",
    type = "Analytical"
  ),
  tibble(
    z = z_stratified2,
    flow = 10^flow_thresholds,
    method = "Stratified2",
    type = "Simulated-test"
  )
)

# Create faceted plot
p = ggplot(plot_data, aes(x = z, y = flow, color = type, linetype = type)) +
  geom_line(data = filter(plot_data, type == "Analytical"), linewidth = 1.2) +
  geom_point(data = filter(plot_data, type == "Simulated" & method == "Crude MC"),
             size = 0.8, alpha = 0.6) +
  geom_line(data = filter(plot_data, type == "Simulated" & method == "Stratified"),
            linewidth = 1) +
  geom_line(data = filter(plot_data, type == "Simulated-test" & method == "Stratified2"),
            linewidth = 1) +
  facet_wrap(~method, ncol = 2) +
  scale_y_log10(
    labels = scales::comma,
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    minor_breaks = scales::trans_breaks("log10", function(x) 10^x)
  ) +
  scale_color_manual(values = c("Analytical" = "red", "Simulated" = "blue", "Simulated-test" = "green2")) +
  scale_linetype_manual(values = c("Analytical" = "solid", "Simulated" = "solid", "Simulated-test" = "solid")) +
  labs(
    title = "Comparison: Crude Monte Carlo vs Stratified Sampling",
    subtitle = paste0("Both methods use ", n_samples, " total samples"),
    x = "Standard Normal Quantile (Z)",
    y = "Peak Flow (cfs, log scale)",
    color = "Source",
    linetype = "Source"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_line(color = "gray90"),
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 11)
  )

print(p)

ggplot(plot_data, aes(x = z, y = flow, color = type, linetype = type)) +
  geom_line(data = filter(plot_data, type == "Analytical"), linewidth = 1.2) +
  geom_line(data = filter(plot_data, type == "Simulated" & method == "Stratified"),
            linewidth = 1) +
  geom_line(data = filter(plot_data, type == "Simulated-test" & method == "Stratified2"),
            linewidth = 1) +
  scale_y_log10(
    labels = scales::comma,
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    minor_breaks = scales::trans_breaks("log10", function(x) 10^x)
  ) +
  #scale_color_manual(values = c("Analytical" = "red", "Simulated" = "blue")) +
  #scale_linetype_manual(values = c("Analytical" = "solid", "Simulated" = "solid")) +
  labs(
    title = "Comparison: Crude Monte Carlo vs Stratified Sampling",
    subtitle = paste0("Both methods use ", n_samples, " total samples"),
    x = "Standard Normal Quantile (Z)",
    y = "Peak Flow (cfs, log scale)",
    color = "Source",
    linetype = "Source"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_line(color = "gray90"),
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 11)
  )
