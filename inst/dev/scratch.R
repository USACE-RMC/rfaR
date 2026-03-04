##### DM Scratch -----------
# bfParams
jmd_bf_parameter_sets

# stage_ts
# jmd_por_stage
jmd_wy1980_stage

# seasonality
jmd_seasonality

# hydrographs
hydrographs <- hydrograph_setup(jmd_hydro_pmf,
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

results_expected <- rfa_simulate(sim_type = "expected",
                                 bestfit_params = jmd_bf_parameter_sets,
                                 stage_ts = jmd_wy1980_stage,
                                 seasonality = jmd_seasonality$relative_frequency,
                                 hydrographs = hydrographs,
                                 resmodel = jmd_resmodel,
                                 Nbins = 50, events_per_bin = 200)

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



# Hydrograph function scratch -----

hydrographs <- hydrograph_setup(jmd_hydro_apr1999,
                                jmd_hydro_jun1921,
                                jmd_hydro_jun1965,
                                jmd_hydro_jun1965_15min,
                                jmd_hydro_may1955,
                                jmd_hydro_pmf,
                                jmd_hydro_sdf,
                                critical_duration = 2,
                                routing_days = 10,
                                weights = c(0.5,1,2,2,2,4))
attr(hydrographs, "probs")
attr(hydrographs, "dt")

# Flow Frequency - Correcting Sampling Bug ------
Q_Samp_ev1 <- flow_frequency_sampler(jmd_bf_parameter_sets,
                                 freq_dist = "LP3",
                                 ExpectedOnly = T)

Q_samp_df_ev1 <- tibble(Flow = Q_Samp_ev1$flow[,1])

Q_Samp_norm <- flow_frequency_sampler(jmd_bf_parameter_sets,
                                     freq_dist = "LP3",
                                     strat_dist = "normal",
                                     ExpectedOnly = T)

Q_samp_df_norm <- tibble(Flow = Q_Samp_norm$flow[,1])

Q_Samp_uni <- flow_frequency_sampler(jmd_bf_parameter_sets,
                                      freq_dist = "LP3",
                                      strat_dist = "uniform",
                                      ExpectedOnly = T)

Q_samp_df_uni <- tibble(Flow = Q_Samp_uni$flow[,1])

ggplot(Q_samp_df_ev1) +
  geom_histogram(aes(x = Flow),
                 fill = "lightblue2",
                 color = "grey20",
                 bins = 100) +
  scale_x_log10(breaks = scales::log_breaks(n = 5, base = 10),
                minor_breaks = scales::minor_breaks_log(detail = 1)) +
  labs(y = "Count",
       x = "Flow (cfs)",
       title = "Distribution of VFC Stratified Sample",
       subtitle = "From 10,000 parameter sets in BestFit") +
  theme(plot.title = element_text(face = "bold", size = 14))

# Strat Sample
ords_ev1 <- stratified_sampler(Nbins = 20,
                           Mevents = 500,
                           dist = "ev1")

ords_norm <- stratified_sampler(Nbins = 20,
                                Mevents = 500,
                                dist = "normal")

ords_uni <- stratified_sampler(Nbins = 20,
                               Mevents = 500,
                               dist = "uniform")
ords_ev1$Zlower
ords_norm$Zlower
ords_uni$Zlower

# Try to create Haden's spreadsheet ==
# BF Posterior mode
meanlog <- jmd_bf_parameter_sets$mean_log[which.max(jmd_bf_parameter_sets$log_likelihood)]
sdlog <- jmd_bf_parameter_sets$sd_log[which.max(jmd_bf_parameter_sets$log_likelihood)]
skewlog <- jmd_bf_parameter_sets$skew_log[which.max(jmd_bf_parameter_sets$log_likelihood)]

# Z variate matrix
z_matrix <- matrix(ncol = ords_ev1$Nbins, nrow = ords_ev1$Mevents)
set.seed(2124)

for (i in 1:ords_ev1$Nbins){
  # upper and lower
  # ev1
  bin_lower <- ords_ev1$Zlower[i]
  bin_upper <- ords_ev1$Zupper[i]

  # normal
  # bin_lower <- ords_norm$Zlower[i]
  # bin_upper <- ords_norm$Zupper[i]

  # uniform
  # bin_lower <- ords_uni$Zlower[i]
  # bin_upper <- ords_uni$Zupper[i]

  # Vector of random values
  z_matrix[,i] <- (bin_lower + (runif(500, min = 0, max = 1))*(bin_upper - bin_lower))
}

# Flow Matrix
#Q_matrix <- matrix(ncol = ords_ev1$Nbins, nrow = ords_ev1$Mevents)

# # Vectorized over columns
# Q_matrix <- matrix(ncol = ords_ev1$Nbins, nrow = ords_ev1$Mevents)
#
# for (m in 1:ncol(z_matrix)) {
#   Q_matrix[, m] <- 10^qp3(pnorm(z_matrix[, m]), meanlog, sdlog, skewlog)
# }

# Entirely vectorized (should work)
Q_matrix <- 10^qp3(pnorm(z_matrix), meanlog, sdlog, skewlog)

# Full loop - Expected(iterate each index with new parameter set)
for (m in 1:ncol(z_matrix)){
  for (n in 1:nrow(z_matrix)){
    # Z-variate
    z_var <- z_matrix[n,m]

    # Parameter set
    idx <- (m-1) * nrow(z_matrix) + n
    meanlog <- jmd_bf_parameter_sets[idx,1]
    sdlog <- jmd_bf_parameter_sets[idx,2]
    skewlog <- jmd_bf_parameter_sets[idx,3]

    # Sampled Flow
    Q_z <- 10^qp3(pnorm(z_var),meanlog, sdlog, skewlog)

    # Save to Matrix
    Q_matrix[n,m] <- Q_z
  }
}

# Then Route Each Flow Through and obtain related peak stage
# sample month/start stage and hydrograph shape
# 1. For each of the 10,000 flows: sample a month → starting pool elevation, sample a hydrograph shape, scale it to the flow volume, route it, store peak stage
# 2. Result: stage_matrix [500 x 20] — same dimensions as Q_matrix
# 3. Post-process with ords_ev1$Weights to get the stage-frequency curve

# NUMBER OF SIMULATIONS
Nsims <- nrow(Q_matrix) * ncol(Q_matrix)

# STARTING POOL
# Initialize vector of sampled months
InitMonths <- sample(1:12, size = Nsims, replace = TRUE, prob = jmd_seasonality$relative_frequency)

# Initialize vector of sampled stages
InitStages <- numeric(Nsims)

# Use vector of sampled months to create sample of starting stages
stage_ts <- jmd_por_stage
stage_ts$months <- lubridate::month(lubridate::mdy(stage_ts$date))

# Extract unique months
UniqMonths <- sort(unique(InitMonths))

for (i in 1:length(UniqMonths)) {
  sampleID <- which(InitMonths == UniqMonths[i])
  InitStages[sampleID] <- sample(stage_ts$stage[stage_ts$months %in% UniqMonths[i]],
                                 size = sum(InitMonths == UniqMonths[i]), replace = TRUE)
}

# RFA Starting Stage Results
InitStages <- jmd_rfa_median_starting_stage$starting_stage

# Hydrographs
hydrographs <- hydrograph_setup(jmd_hydro_apr1999,
                                jmd_hydro_jun1921,
                                jmd_hydro_jun1965,
                                jmd_hydro_jun1965_15min,
                                jmd_hydro_may1955,
                                jmd_hydro_pmf,
                                jmd_hydro_sdf,
                                critical_duration = 2,
                                routing_days = 10)

# Sample Order with probs (normalized weights)
hydroSamps <- sample(1:length(hydrographs), size = Nsims, replace = TRUE,
                     prob = attr(hydrographs, "probs"))

# Peak Stage Matrix
peakStage <- peakFlow <- matrix(NA, nrow = nrow(Q_matrix), ncol = ncol(Q_matrix))
n_inner <- ncol(Q_matrix)

# Routings
realiz <- 0

cli::cli_progress_bar("Expected Only Simulation", total = Nsims)

for (i in 1:nrow(Q_matrix)) {

  for (j in 1:ncol(Q_matrix)) {
    realiz <- (i - 1) * n_inner + j

    # Hydrograph shape
    hydrograph_shape <- hydrographs[[hydroSamps[realiz]]][,2:3]

    # Hydrograph observed volume
    obs_hydrograph_vol <- attr(hydrographs[[hydroSamps[realiz]]],"obs_vol")

    # Scale hydrograph to sample volume
    scaled_hydrograph <- scale_hydrograph(hydrograph_shape,
                                          obs_hydrograph_vol,
                                          Q_matrix[i,j])

    # Route scaled hydrograph
    tmpResults <- mod_puls_routing(resmodel_df = jmd_resmodel, inflow_df = scaled_hydrograph,
                                   initial_elev = InitStages[realiz], full_results = FALSE)

    # Record results
    peakStage[i, j] <- tmpResults[1]
    peakFlow[i, j] <- tmpResults[2]

    cli::cli_progress_update()
  }
}

cli::cli_progress_done()
cli::cli_alert_success("Expected Only Simulation Complete")

# Post Process Stages
min_stage <- min(peakStage)
max_stage <- max(peakStage)

stage_vect <- rep(NA,ords_ev1$Mevents-1)

for (b in 1:(ords_ev1$Mevents-1)){
  if(b < 2){
    stage = min_stage
  }else{
    stage = stage_vect[b-1] + (max_stage - min_stage)/(ords_ev1$Mevents-1)
  }
  stage_vect[b] = stage
}

exceedance_matrix <- matrix(nrow = length(stage_vect), ncol = ncol(Q_matrix))

for(m in 1:ncol(peakStage)){
  # Get Bin of routed stages
  mevents <- peakStage[,m]

  for(n in 1:length(stage_vect)){
    # Get Stage
    exceedance_stage = stage_vect[n]
    # How many exceedances in Bin
    exceed_count = sum(mevents > exceedance_stage)
    # Exceedance Prob in Bin
    exceed_prob = exceed_count/length(mevents)
    # Save to exceedance matrix
    exceedance_matrix[n,m] <- exceed_prob
  }
}

# AEP Vector
aep_vect <- rep(NA,length(stage_vect))

for(m in 1:nrow(exceedance_matrix)){
  # Grab row of exceedances
  exceedance_probs <- exceedance_matrix[m,]
  # Sum the dot product of exceedances and weights
  aep <- sum(exceedance_probs*ords_ev1$Weights)
  #save to vector
  aep_vect[m] <- aep
}

# Z Variate
z_vect <- qnorm(1-aep_vect)

test_result <- tibble(
  AEP = aep_vect,
  Z_AEP = z_vect,
  Gumb = -log(-log(1 - AEP)),
  Stage = stage_vect
)

curve1 <- curve1|> mutate(Gumb = -log(-log(1 - AEP)),
                         Z_var = qnorm(1-AEP),
                         Stage = stage)

curve_2 <- readRDS("D:/0.RMC/Reefer/full_uncert_testing/curve_2.rds")|> mutate(Gumb = -log(-log(1 - AEP)),
                                                                               Z_var = qnorm(1-AEP),
                                                                               Stage = stage)

curve_3 <- readRDS("D:/0.RMC/Reefer/full_uncert_testing/curve_3.rds")|> mutate(Gumb = -log(-log(1 - AEP)),
                                                                               Z_var = qnorm(1-AEP),
                                                                               Stage = stage)


# Plot
jmd_rfa_median <- jmd_rfa_median |> mutate(Gumb = -log(-log(1 - AEP)),
          Z_var = qnorm(1-AEP))

jmd_rfa_expected <- jmd_rfa_expected |> mutate(Gumb = -log(-log(1 - AEP)),
                                               Z_var = qnorm(1-AEP))
ggplot() +
  geom_line(
    #data = test_result,
    data = curve1,
    aes(x = Gumb, y = Stage, color = "rfaR"),
    linewidth = 0.85) +

  geom_line(
    #data = test_result,
    data = curve_2,
    aes(x = Gumb, y = Stage, color = "rfaR"),
    linewidth = 0.85) +

  geom_line(
    #data = test_result,
    data = curve_3,
    aes(x = Gumb, y = Stage, color = "rfaR"),
    linewidth = 0.85) +

  geom_line(
    #data = jmd_rfa_median,
    data = jmd_rfa_expected,
    aes(x = Gumb, y = Expected, color = "RFA"),
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

ggsave(file.path(getwd(),"inst","dev","Feb_20.png"), height = 6, width = 8, dpi = 500)

