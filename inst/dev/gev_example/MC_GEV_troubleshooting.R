library(tidyverse)
theme_set(theme_bw())

mc_gev <- fs::dir_ls(path = getwd(), glob = "*mc_bf_parameter_sets.csv", recurse = T) |>
  read_csv() |>
  data.frame()

Q_Samp <- flow_frequency_sampler(mc_gev,
                                 dist = "GEV",
                                 ExpectedOnly = T)

Q_samp_df <- tibble(Flow = Q_Samp$flow[,1])
neg_Q_samp <- Q_samp_df |>
  filter(Flow < 0)

ggplot() +
  geom_histogram(data = Q_samp_df,
                 aes(x = Flow),
                 fill = "lightblue2",
                 color = "grey20",
                 bins = 100) +
  # geom_histogram(data = neg_Q_samp,
  #                aes(x = Flow),
  #                fill = "red4",
  #                color = "grey20",
  #                bins = 100) +
  scale_x_log10(breaks = scales::log_breaks(n = 5, base = 10),
                minor_breaks = scales::minor_breaks_log(detail = 1)) +
  labs(y = "Count",
       x = "Flow (cfs)",
       title = "Distribution of VFC Stratified Sample",
       subtitle = "From 10,000 parameter sets in BestFit") +
  theme(plot.title = element_text(face = "bold", size = 14))


# Stratified Sample Bins weights ==========
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

Q_samp_strat <- tibble(Z_Ord = Q_strat$normOrd,
                       AEP = 1-pnorm(Z_Ord),
                       Flow = Q_Samp$flow[,1])

strat_bins_df <- tibble(Zlower = Q_strat$Zlower,
                        ZUpper = Q_strat$Zupper,
                        Weight = Q_strat$Weights) |>
  mutate(bin_id = cut(ZUpper,
                      breaks = c(Zlower, last(ZUpper)),
                      labels = FALSE,
                      include.lowest = TRUE,
                      right = FALSE))

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

# plot
ggplot(Q_samp_strat_weighted) +
  geom_line(aes(x = Z_Ord, y = Flow, color = factor(bin_id)),
            linewidth = 2,
            alpha = 0.8) +
  # scale_y_log10(breaks = scales::log_breaks(n = 5, base = 10),
  #               minor_breaks = scales::minor_breaks_log(detail = 1)) +
  scale_x_continuous(breaks = z_breaks,
                     minor_breaks = z_breaks_minor,
                     labels = aep_breaks) +
  scale_color_manual(values = rep(c("#F28E2B", "#0072B2"), length.out = 50)) +
  coord_cartesian(xlim = c(-2.326348, 5.612001)) +
  theme_bw() +
  labs(x = "AEP",
       y = "Flow",
       title = "VFC Stratified Sample",
       subtitle = "From 10,000 parameter sets in BestFit") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 30))
