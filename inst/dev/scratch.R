##### DM Scratch -----------
# bfParams
jmd_bf_parameter_sets

# stage_ts
jmd_por_stage

# seasonality
jmd_seasonality

# hydrographs
hydrographs <- hydrograph_setup(jmd_hydro_apr1999,
                         jmd_hydro_jun1921,
                         jmd_hydro_jun1965,
                         # jmd_hydro_jun1965_15min,
                         jmd_hydro_may1955,
                         jmd_hydro_pmf,
                         jmd_hydro_sdf)

expected_test <- rfaR(jmd_bf_parameter_sets,
                      dist = "LP3",
                      jmd_por_stage,
                      jmd_seasonality$relative_frequency,
                      hydrographs,
                      jmd_resmodel,
                      critical_dur = 2,
                      routing_dur = 10,
                      expected_only = TRUE)

## Test Plot
library(tidyverse)
# Critical Elevations ----------------------------------------------------------
crit_elevs <- tibble(
  Name = c("Upper PMF", "Recommended PMF", "Top of Dam", "Record Pool", "Flood Control Pool", "Spillway Crest"),
  Elev = c(3893.8,3890.9,3881.8,3862.2,3871.8,3841.8))

crit_elevs <- crit_elevs %>%
  mutate(label_text = paste(Name,"=",Elev))

jmd_empirical_stage_wy1980_pt$Gumb <- -log(-log(1 - jmd_empirical_stage_wy1980_pt$plot_posit))

# Plot by Sensitivity Group ----------------------------------------------------

# 1. AEP breaks for plotting -----
aep_breaks <- c(9.9e-1, 9e-1, 5e-1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10)
minor_aep_breaks <- unlist(lapply(2:9, function(i) i * 10^-(1:10)))

Gumbel_AEP_breaks <- -log(-log(1-aep_breaks))
Gumbel_AEP_breaks_minor <- -log(-log(1-minor_aep_breaks))

z_breaks <- qnorm(1-aep_breaks)
z_breaks_minor <- qnorm(1-minor_aep_breaks)

gumb_limit1 <- -log(-log(1-(9.9e-1)))
gumb_limit2 <- -log(-log(1-(1e-7)))

# PLOT
ggplot() +

  # Final 95 CI
  # geom_ribbon(
  #   data = result_df,
  #   aes(x = Gumb, ymin = Lower_95, ymax = Upper_95),
  #   fill = "grey70", alpha = .5) +

  geom_line(
    data = expected_test,
    aes(x = Gumb, y = Expected, color = "Expected"),
    linewidth = 0.85) +

  # Add AAAS color scale
  ggsci::scale_color_aaas() +

  # Observed Stage points
  geom_point(data = jmd_empirical_stage_wy1980_pt, aes(x = Gumb, y = stage_ft, shape = "Obs. Stages (WY 1948-2020)"),size = 1.5,alpha = 0.7) +

  scale_shape_manual(values = c("Obs. Stages (WY 1948-2020)" = 16)) +

  # Scales
  scale_x_continuous(
    breaks = Gumbel_AEP_breaks,
    minor_breaks = Gumbel_AEP_breaks_minor,
    labels = aep_breaks
  ) +
  scale_y_continuous(
    breaks = seq(3700,3910,10),
    minor_breaks = seq(3700,3910,5),
    labels = scales::comma
  ) +

  # Labels
  labs(
    x = "AEP",
    y = "Stage (ft-NAVD88)",
    title = paste("Reefer Madness"),
    shape = "Observations",
    color = NULL) +
  geom_hline(yintercept = crit_elevs$Elev) +
  annotate("text",x = -log(-log(1 - 0.999)), y = crit_elevs$Elev,
           label = crit_elevs$label_text,
           size = 2.5, hjust = 0, vjust = -.5) +

  # Theme
  theme_bw() +
  theme(
    legend.position = "inside",
    #legend.position.inside = c(0.7, 0.6),
    legend.justification = c(0.95, 0.1),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7, face = "bold"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key.width = unit(1, "cm"),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    plot.title = element_text(size = 11, face = "bold")
  )+
  coord_cartesian(xlim = c(gumb_limit1, gumb_limit2), ylim = c(3800,3900))


ggsave(file.path(getwd(),"inst","dev","aHA.png"), height = 6, width = 8, dpi = 500)
