# JMD Parameter Set and Strat Sampling Figure
library(tidyverse)


# Parameter Distributions ======================================================
# jmd_bf_parameter_sets

plot_theme <- theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 10),
        axis.title = element_text(size = 8))

# final_color <- "#BB0021FF"
#"#d02b28"
#print(pal_aaas("default")(10))

# Create individual plots
p1 <- ggplot(jmd_bf_parameter_sets, aes(x = mean_log)) +
  geom_density(fill = "gray80", alpha = 0.7, linewidth = 0.5) +
  labs(title = "Mean (of Log)", x = "Value", y = "Density") +
  plot_theme

p2 <- ggplot(jmd_bf_parameter_sets, aes(x = sd_log)) +
  geom_density(fill = "gray80", alpha = 0.7, linewidth = 0.5) +
  labs(title = "Standard Deviation (of Log)", x = "Value", y = "Density") +
  plot_theme

p3 <- ggplot(jmd_bf_parameter_sets, aes(x = skew_log)) +
  geom_density(fill = "gray80", alpha = 0.7, linewidth = 0.5) +
  labs(title = "Skew (of Log)", x = "Value", y = "Density") +
  plot_theme

# Combine with patchwork
combined_plot <- (p1 | p2 | p3) +
  plot_annotation(
    title = "LP3 Parameter Distributions",
    subtitle = "From 10,000 parameter sets in BestFit",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

print(combined_plot)
ggsave("D:/0.RMC/Reefer/Flood_Haz_January_2026/figs/Parameter_Summary.png",combined_plot, width = 10, height = 7, dpi = 400)

# Q_Samp Plot ===
Q_Samp <- flow_frequency_sampler(jmd_bf_parameter_sets,
                                 dist = "LP3",
                                 ExpectedOnly = T)
Q_samp_df <- tibble(Flow = Q_Samp$flow[,1])

q_samp_plot <- ggplot(Q_samp_df) +
  geom_histogram(aes(x = Flow),
                     fill = "lightblue2",
                     color = "grey20",
                 bins = 100) +
  scale_x_log10(breaks = scales::log_breaks(n = 5, base = 10),
                minor_breaks = scales::minor_breaks_log(detail = 1)) +
  labs(y = "Count",
       x = "Flow (cfs)",
       title = "Distribution of Expected Only VFC Stratified Sample",
       subtitle = "From 10,000 parameter sets in BestFit") +
       theme(plot.title = element_text(face = "bold", size = 14))
ggsave("D:/0.RMC/Reefer/Flood_Haz_January_2026/figs/Flow_Sample_Hist.png",q_samp_plot, width = 7, height = 5, dpi = 400)

# Stratified Sample Bins weights ===
aep_breaks <- c(9.9e-1, 9e-1, 5e-1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10)
minor_aep_breaks <- unlist(lapply(2:9, function(i) i * 10^-(1:10)))

z_breaks <- qnorm(1-aep_breaks)
z_breaks_minor <- qnorm(1-minor_aep_breaks)

# Set up data
assign_weight <- function(z_values, bins_df) {
  weights <- numeric(length(z_values))
  for (i in seq_along(z_values)) {
    bin_idx <- which(z_values[i] >= bins_df$Zlower & z_values[i] < bins_df$ZUpper)
    weights[i] <- if(length(bin_idx) > 0) bins_df$Weight[bin_idx[1]] else NA
  }
  return(weights)
}

Q_strat <- stratified_sampler()

Q_samp_strat <- tibble(Z_Ord = strat_test$normOrd,
                       AEP = 1-pnorm(Z_Ord),
                       Flow = Q_Samp$flow[,1])

strat_bins_df <- tibble(Zlower = strat_test$Zlower,
                        ZUpper = strat_test$Zupper,
                        Weight = strat_test$Weights) |>
  mutate(bin_id = cut(ZUpper,
                      breaks = c(strat_bins_df$Zlower, last(strat_bins_df$ZUpper)),
                      labels = FALSE,
                      include.lowest = TRUE,
                      right = FALSE)) |>
  mutate(bin_id_20 = ceiling(bin_id / 10))

Q_samp_strat_weighted <- Q_samp_strat %>%
  mutate(Weight = assign_weight(Z_Ord, strat_bins_df))

# Create a bin identifier
Q_samp_strat_weighted <- Q_samp_strat_weighted |>
  mutate(
    bin_id = cut(Z_Ord,
                 breaks = c(strat_bins_df$Zlower, last(strat_bins_df$ZUpper)),
                 labels = FALSE,
                 include.lowest = TRUE,
                 right = FALSE)) |>
  mutate(bin_id_20 = ceiling(bin_id / 10))

strat_bins_df_20 <- strat_bins_df |>
  group_by(bin_id_20) |>
  slice_max(ZUpper)

blue_to_red <- colorRampPalette(c("blue", "cyan", "yellow", "orange", "red"))(20)

expected_vfc_bins <- ggplot(Q_samp_strat_weighted) +
  geom_line(aes(x = Z_Ord, y = Flow, color = factor(bin_id_20)),linewidth = 2,
              alpha = 0.8) +
  geom_vline(xintercept = strat_bins_df_20$ZUpper,
             color = "grey10",
             linetype = "dashed",
             linewidth = 0.5)+
  scale_color_manual(values = blue_to_red, name = "Bin Group") +
  scale_y_log10(breaks = scales::log_breaks(n = 5, base = 10),
                minor_breaks = scales::minor_breaks_log(detail = 1)) +
  scale_x_continuous(breaks = z_breaks,
                     minor_breaks = z_breaks_minor,
                     labels = aep_breaks) +
  coord_cartesian(xlim = c(-2.326348, 5.612001)) +
  theme_bw() +
  labs(x = "AEP",
       y = "Flow",
       title = "Expected Only VFC Stratified Sample",
       subtitle = "From 10,000 parameter sets in BestFit") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))

ggsave("D:/0.RMC/Reefer/Flood_Haz_January_2026/figs/Flow_Sample_Bins.png",expected_vfc_bins, width = 7, height = 5, dpi = 400)

q_sample_plot_combined <- q_samp_plot/expected_vfc_bins
ggsave("D:/0.RMC/Reefer/Flood_Haz_January_2026/figs/Flow_Sample_Bins_combined.png",q_sample_plot_combined, width = 7, height = 7, dpi = 400)


