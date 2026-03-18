##### DM Scratch -----------
# bfParams
jmd_bf_parameter_sets

# stage_ts
# jmd_por_stage
jmd_wy1980_stage

# seasonality
jmd_seasonality

# hydrographs
jmd_hydrographs <- hydrograph_setup(jmd_hydro_pmf,
                                jmd_hydro_jun1965_15min,
                                jmd_hydro_may1955,
                                jmd_hydro_jun1965,
                                jmd_hydro_apr1999,
                                jmd_hydro_sdf,
                                jmd_hydro_jun1921,
                                critical_duration = 2,
                                routing_days = 10)

# Hydrograph shape
# hydroSamps <- sample(1:length(hydrographs), size = 1000, replace = TRUE,
#                      prob = attr(hydrographs, "probs"))

# realiz <- 1
hydrograph_shape <- hydrographs[[5]][,2:3]

# Hydrograph observed volume
obs_hydrograph_vol <- attr(hydrographs[[5]],"obs_vol")
dt <- attr(hydrographs[[5]],"dt")

# scaled_hydrograph_15min <- scale_hydrograph(hydrograph_shape,
#                                           obs_hydrograph_vol,
#                                           200000,
#                                           routing_dt = 0.25)
# Mod Puls testing
# route_15min <- mod_puls_routing(resmodel_df = jmd_resmodel, inflow_df = scaled_hydrograph_15min,
#                                initial_elev = 3830.0, full_results = T)
# route_15min$Timestep <- "0.25"

jmd_expected <- rfa_simulate(sim_type        = "expected",
                             bestfit_params = jmd_bf_parameter_sets,
                             stage_ts       = jmd_wy1980_stage,
                             seasonality    = jmd_seasonality$relative_frequency,
                             hydrographs    = jmd_hydrographs,
                             resmodel       = jmd_resmodel,
                             Nbins          = 50,
                             events_per_bin = 200,
                             sim_name       = "jmd")

results_median <- rfa_simulate(sim_type = "median",
                               bestfit_params = jmd_bf_parameter_sets,
                               stage_ts = jmd_wy1980_stage,
                               seasonality = jmd_seasonality$relative_frequency,
                               hydrographs = hydrographs,
                               resmodel = jmd_resmodel,
                               Nbins = 50, events_per_bin = 200)

results_full <- rfa_simulate(sim_type = "full",
                             bestfit_params = jmd_bf_parameter_sets,
                             stage_ts = jmd_wy1980_stage,
                             seasonality = jmd_seasonality$relative_frequency,
                             hydrographs = hydrographs,
                             resmodel = jmd_resmodel,
                             Nbins = 50, events_per_bin = 200,
                             Ncores = 26,
                             results_dir = "D:/0.RMC/Reefer/rfaR/full_uncert_testing/")
# Median =======================================================================
# harlan_median <- rfa_simulate(sim_type = "median",
#                               bestfit_params = harlan_bestfit_params,
#                               stage_ts = harlan_stage_ts,
#                               seasonality = harlan_seasonality$relative_frequency,
#                               hydrographs = harlan_hydrographs,
#                               resmodel = harlan_resmodel,
#                               Nbins = 50, events_per_bin = 200,
#                               sim_name = "harlan")
#
# # Expected Only ================================================================
# harlan_expected <- rfa_simulate(sim_type = "expected",
#                                 bestfit_params = harlan_bestfit_params,
#                                 stage_ts = harlan_stage_ts,
#                                 seasonality = harlan_seasonality$relative_frequency,
#                                 hydrographs = harlan_hydrographs,
#                                 resmodel = harlan_resmodel,
#                                 Nbins = 50, events_per_bin = 200,
#                                 sim_name = "harlan")
#
# # Full Uncertainty =============================================================
# harlan_full <- rfa_simulate(sim_type = "full",
#                             bestfit_params = harlan_bestfit_params,
#                             stage_ts = harlan_stage_ts,
#                             seasonality = harlan_seasonality$relative_frequency,
#                             hydrographs = harlan_hydrographs,
#                             resmodel = harlan_resmodel,
#                             Nbins = 50, events_per_bin = 200,
#                             Ncores = 26)

jmd_expected$realization_results
ggplot(jmd_expected$realization_results)+
  geom_point(aes(x = init_stage, y = peak_stage, color = hydrograph))

# COMPARE WITH RFA RESULTS =====================================================
library(tidyverse)

# Critical Elevations ----------------------------------------------------------
crit_elevs <- tibble(
  Name = c("Upper PMF", "Recommended PMF", "Top of Dam", "Record Pool", "Flood Control Pool", "Spillway Crest"),
  Elev = c(3893.8,3890.9,3881.8,3862.2,3871.8,3841.8))

crit_elevs <- crit_elevs %>%
  mutate(label_text = paste(Name,"=",Elev))

jmd_empirical_stage_wy1980_pt$Gumb <- -log(-log(1 - jmd_empirical_stage_wy1980_pt$plot_posit))
jmd_empirical_stage_wy1980_pt$Z_var <- qnorm(1 - jmd_empirical_stage_wy1980_pt$plot_posit)

# Plot by Sensitivity Group ----------------------------------------------------
jmd_rfa_expected <- jmd_rfa_expected |> mutate(Gumb = -log(-log(1 - AEP)),
                                               Z_var = qnorm(1-AEP))

jmd_rfa_full <- jmd_rfa_full |> mutate(Gumb = -log(-log(1 - AEP)),
                                       Z_var = qnorm(1-AEP))

# 1. AEP breaks for plotting -----
aep_breaks <- c(9.9e-1, 9e-1, 5e-1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10)
minor_aep_breaks <- unlist(lapply(2:9, function(i) i * 10^-(1:10)))

Gumbel_AEP_breaks <- -log(-log(1-aep_breaks))
Gumbel_AEP_breaks_minor <- -log(-log(1-minor_aep_breaks))

z_breaks <- qnorm(1-aep_breaks)
z_breaks_minor <- qnorm(1-minor_aep_breaks)

gumb_limit1 <- -log(-log(1-(9.9e-1)))
gumb_limit2 <- -log(-log(1-(1e-7)))

z_limit1 <- qnorm(1 - 9.9e-1)
z_limit2 <- qnorm(1 - 1e-7)

# Expected result
# expected_sbins_1k <- stage_frequency_curve(results_expected$peakStage,results_expected$weights,1000) |>
#   mutate(Gumb = -log(-log(1 - AEP)),
#          Z_var = qnorm(1-AEP))

expected_test <- results_expected$stage_frequency |> mutate(Gumb = -log(-log(1 - AEP)),
                                                            Z_var = qnorm(1-AEP))

# PLOT
ggplot() +
  # Final 95 CI
  # geom_ribbon(
  #   data = result_df,
  #   aes(x = Gumb, ymin = Lower_95, ymax = Upper_95),
  #   fill = "grey70", alpha = .5) +
  geom_line(data = expected_test,
            aes(x = Z_var, y = Expected, color = "rfaR"),
            linewidth = 0.85) +
  geom_line(data = jmd_rfa_expected,#data = jmd_rfa_full,
            aes(x = Z_var, y = Expected, color = "RFA"),
            linewidth = 0.85) +
  # Add AAAS color scale
  ggsci::scale_color_aaas() +
  # Observed Stage points
  geom_point(data = jmd_empirical_stage_wy1980_pt, aes(x = Z_var, y = stage_ft, shape = "Obs. Stages (WY 1948-2020)"),size = 1.5,alpha = 0.7) +
  scale_shape_manual(values = c("Obs. Stages (WY 1948-2020)" = 16)) +
  # Scales
  scale_x_continuous(breaks = z_breaks,
                     minor_breaks = z_breaks_minor,
                     labels = aep_breaks) +
  scale_y_continuous(breaks = seq(3700,3910,10),
                     minor_breaks = seq(3700,3910,5),
                     labels = scales::comma) +
  # Labels
  labs(x = "AEP",
       y = "Stage (ft-NAVD88)",
       title = paste("Reefer Madness"),
       shape = "Observations",
       color = NULL) +
  geom_hline(yintercept = crit_elevs$Elev) +
  annotate("text",x = qnorm(1 - 0.99), y = crit_elevs$Elev,
           label = crit_elevs$label_text,
           size = 2.5, hjust = 0, vjust = -.5) +
  # Theme
  theme_bw() +
  theme(legend.position = "inside",
        #legend.position.inside = c(0.7, 0.6),
        legend.justification = c(0.95, 0.1),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7, face = "bold"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.key.width = unit(1, "cm"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 11, face = "bold"))+
  coord_cartesian(xlim = c(z_limit1, z_limit2), ylim = c(3800,3900))

ggsave(file.path(getwd(),"inst","dev","aHA.png"), height = 6, width = 8, dpi = 500)

