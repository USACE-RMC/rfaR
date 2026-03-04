# Reefer Concept
# This script serves as a conceptual guide to the processes in RFA and rfa_simulate
library(tidyverse) # Used later

# Set up Stratified Bins =======================================================
# 20 bins with 500 events per bin
# experiment with norm or uniform if you're interested
ords <- stratified_sampler(Nbins = 20,
                               Mevents = 500,
                               dist = "ev1")
# Set seed ====
cat(think)
set.seed(062154)

# Use the stratified bin ordinates to construction a Z-variate matrix ==========
# The 500 events in each bin will be sampled as the corresponding quantile
# Ensure it's the same size
z_matrix <- matrix(ncol = ords$Nbins, nrow = ords$Mevents)

for (i in 1:ords$Nbins){
  # upper and lower
  # ev1
  bin_lower <- ords$Zlower[i]
  bin_upper <- ords$Zupper[i]

    # Vector of random values
  z_matrix[,i] <- (bin_lower + (runif(500, min = 0, max = 1))*(bin_upper - bin_lower))
}

# Fill the matrix with flow quantiles from a bestfit parameter set =============
# (or any LP3 or GEV parameters of your choosing)
# LP3
meanlog <- 3.550399
sdlog <- 0.3717982
skewlog <- 0.7555138

Q_matrix <- 10^qp3(pnorm(z_matrix), meanlog, sdlog, skewlog)

# GEV
# xi <- 2369.6709
# alfa <- 1658.1753
# k <- -0.6567
# gev_params <- c(xi,alfa,k)
#
# Q_matrix <- lmom::quagev(pnorm(z_matrix), gev_params)

# Define the number of simulations =============================================
# Should be the total number of events (20x500)
Nsims <- nrow(Q_matrix) * ncol(Q_matrix)

# Sample the Seasonality =======================================================
# Vector of sampled months that is Nsims long
InitMonths <- sample(1:12, size = Nsims, replace = TRUE, prob = jmd_seasonality$relative_frequency)

# Sample the starting stage, based on the seasonality sample ===================
InitStages <- numeric(Nsims)

# JMD stage for this example
stage_ts <- jmd_wy1980_stage
stage_ts$months <- lubridate::month(lubridate::mdy(stage_ts$date))

# Extract unique months
UniqMonths <- sort(unique(InitMonths))

# Create starting stage sample =================================================
for (i in 1:length(UniqMonths)) {
  sampleID <- which(InitMonths == UniqMonths[i])
  InitStages[sampleID] <- sample(stage_ts$stage[stage_ts$months %in% UniqMonths[i]],
                                 size = sum(InitMonths == UniqMonths[i]), replace = TRUE)
}

# RFA Starting Stage Results - If you're interested in a 1:1 sampling with RFA,
# use the sample sequence from the RFA tabular results
# InitStages <- jmd_rfa_median_starting_stage$starting_stage

# Sample the hydrograph shapes =================================================
hydrographs <- hydrograph_setup(jmd_hydro_apr1999,
                                jmd_hydro_jun1921,
                                jmd_hydro_jun1965,
                                jmd_hydro_jun1965_15min,
                                jmd_hydro_may1955,
                                jmd_hydro_pmf,
                                jmd_hydro_sdf,
                                critical_duration = 2,
                                routing_days = 10)

# Sample based on the probs (normalized weights)
hydroSamps <- sample(1:length(hydrographs), size = Nsims, replace = TRUE,
                     prob = attr(hydrographs, "probs"))

# Empty Peak Stage (and for this example - Peak Discharge) results matrix ======
peakStage <- peakFlow <- matrix(NA, nrow = nrow(Q_matrix), ncol = ncol(Q_matrix))

# Simulations ==================================================================
# This is the bulk of the computation process
# Grab the starting stage and hydrograph for each sample (pre-allocated) and
# Route them with the sampled inflow volume

realiz <- 0

cli::cli_progress_bar("Expected Only Simulation", total = Nsims)

for (i in 1:nrow(Q_matrix)) {
  for (j in 1:ncol(Q_matrix)) {
    # Keeps the indices lined up - necessary for the weights later
    realiz <- (i - 1) * ncol(Q_matrix) + j

    # Hydrograph shape
    hydrograph_shape <- hydrographs[[hydroSamps[realiz]]][,2:3]

    # Hydrograph observed volume
    obs_hydrograph_vol <- attr(hydrographs[[hydroSamps[realiz]]],"obs_vol")

    # Scale hydrograph to sample volume
    scaled_hydrograph <- scale_hydrograph(hydrograph_shape,
                                          obs_hydrograph_vol,
                                          Q_matrix[i,j])

    # Route scaled hydrograph
    tmpResults <- mod_puls_routing(resmodel_df = jmd_resmodel,
                                   inflow_df = scaled_hydrograph,
                                   initial_elev = InitStages[realiz],
                                   full_results = FALSE)

    # Record results
    peakStage[i, j] <- tmpResults[1]
    peakFlow[i, j] <- tmpResults[2]

    cli::cli_progress_update()
  }
}

cli::cli_progress_done()
cli::cli_alert_success("{Nsims} simulations Complete")

# Post Process =================================================================
# Min/Max Stages
min_stage <- min(peakStage)
max_stage <- max(peakStage)

# Min/Max Discharge
min_flow <- min(peakFlow)
max_flow <- max(peakFlow)

# Set up exceedance values ====
stage_vect <- rep(NA,ords$Mevents)
flow_vect <- rep(NA,ords$Mevents)

# Vector of Stages/flow to use for exceedance calcs
for (b in 1:(ords$Mevents)){
  if(b < 2){
    stage <- min_stage
    flow <- min_flow
  }else{
    stage <- stage_vect[b-1] + (max_stage - min_stage)/(ords$Mevents)
    flow <- flow_vect[b-1] + (max_flow - min_flow)/(ords$Mevents)
  }
  stage_vect[b] <- stage
  flow_vect[b] <- flow
}

# Count number of exceedances in each bin (full of events) ===
stage_exceedance_matrix <- matrix(nrow = length(stage_vect), ncol = ncol(Q_matrix))
flow_exceedance_matrix <- matrix(nrow = length(flow_vect), ncol = ncol(Q_matrix))

# Now determine exceedances (and probs) ====
for(m in 1:ncol(peakStage)){
  # Get Bin of routed stages & flows
  mstages <- peakStage[,m]
  mflows <- peakFlow[,m]

  for(n in 1:length(stage_vect)){
    # Get Stage & Flow
    exceedance_stage <- stage_vect[n]
    exceedance_flow <- flow_vect[n]

    # How many exceedances in Bin
    stage_exceed_count <- sum(mstages > exceedance_stage)
    flow_exceed_count <- sum(mflows > exceedance_flow)

    # Exceedance Prob in Bin
    stage_exceed_prob <- stage_exceed_count/length(mstages)
    flow_exceed_prob <- flow_exceed_count/length(mflows)

    # Save to exceedance matrix - I know the n,m index seems backwards. I set
    # the loop up to "travel" vertically down the bins to compare with the min/max
    stage_exceedance_matrix[n,m] <- stage_exceed_prob
    flow_exceedance_matrix[n,m] <- flow_exceed_prob
  }
}

# Estimate AEPs using bin weights ====
stage_aep_vect <- rep(NA,length(stage_vect))
flow_aep_vect <- rep(NA,length(flow_vect))

for(m in 1:nrow(stage_exceedance_matrix)){
  # Grab row of exceedances
  stage_exceedance_probs <- stage_exceedance_matrix[m,]
  flow_exceedance_probs <- flow_exceedance_matrix[m,]

  # Sum the dot product of exceedances and weights
  stage_aep <- sum(stage_exceedance_probs*ords$Weights)
  flow_aep <- sum(flow_exceedance_probs*ords$Weights)

  #save to vector
  stage_aep_vect[m] <- stage_aep
  flow_aep_vect[m] <- flow_aep
}

# Tibbles for nice plotting and organization ===================================
stage_result <- tibble(
  AEP = stage_aep_vect,
  Z_var = qnorm(1-stage_aep_vect),
  Gumb = -log(-log(1 - AEP)),
  Stage = stage_vect
)

flow_result <- tibble(
  AEP = flow_aep_vect,
  Z_var = qnorm(1-flow_aep_vect),
  Gumb = -log(-log(1 - AEP)),
  Discharge = flow_vect
)

# Plot set up ==================================================================
# Boring!
library(ggsci)
theme_set(theme_bw())
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

# Stage Info
crit_elevs <- tibble(
  Name = c("Upper PMF", "Recommended PMF", "Top of Dam", "Record Pool", "Flood Control Pool", "Spillway Crest"),
  Elev = c(3893.8,3890.9,3881.8,3862.2,3871.8,3841.8))

crit_elevs <- crit_elevs %>%
  mutate(label_text = paste(Name,"=",Elev))

jmd_empirical_stage_wy1980_pt$Gumb <- -log(-log(1 - jmd_empirical_stage_wy1980_pt$plot_posit))
jmd_empirical_stage_wy1980_pt$Z_var <- qnorm(1 - jmd_empirical_stage_wy1980_pt$plot_posit)

# Plot Discharge (standalone) ==================================================
# ggplot() +
#   geom_line(data = flow_result,
#             aes(x = Z_AEP, y = Discharge),
#             color = "#008280FF",
#             linewidth = 0.85) +
#   scale_x_continuous(breaks = z_breaks,
#                      minor_breaks = z_breaks_minor,
#                      labels = aep_breaks) +
#   scale_y_log10(breaks = scales::log_breaks(n = 5, base = 10),
#                 minor_breaks = scales::minor_breaks_log(detail = 1)) +
#   coord_cartesian(xlim = c(qnorm(1-0.01),qnorm(1-1e-6))) +
#   labs(x = "AEP",
#        y = "Discharge (cfs)",
#        title = "JMD Discharge Frequency Curve",
#        subtitle = "Point Estimation Only (one parameter set)") +
#   theme(title = element_text(size = 10,face = "bold"),
#         plot.subtitle = element_text(size = 10,face = "italic"))

# Plot Stage-Frequency =========================================================
# These won't match up because if you use the random GEV parameters for the Inflow
# They wont line up on the lower end with LP3 either, this is a difference in starting
# stage sampling between rfaR and RFA.
jmd_rfa_median <- jmd_rfa_median |> mutate(Gumb = -log(-log(1 - AEP)),
                                           Z_var = qnorm(1-AEP))

jmd_rfa_expected <- jmd_rfa_expected |> mutate(Gumb = -log(-log(1 - AEP)),
                                               Z_var = qnorm(1-AEP))
ggplot() +
  geom_line(data = stage_result,
            aes(x = Z_var, y = Stage, color = "rfaR"),
            linewidth = 0.85) +
  geom_line(data = jmd_rfa_median,
            aes(x = Z_var, y = Median, color = "RFA"),
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
  annotate("text",x = -log(-log(1 - 0.999)), y = crit_elevs$Elev,
           label = crit_elevs$label_text,
           size = 2.5, hjust = 0, vjust = -.5) +
  # Theme
  theme_bw() +
  theme(legend.position = "inside",
        legend.justification = c(0.95, 0.1),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7, face = "bold"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.key.width = unit(1, "cm"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 11, face = "bold"))+
  coord_cartesian(xlim = c(z_limit1, z_limit2), ylim = c(3800,3900))

# Feel free to mess around
# This is meant to be educational for both RFA and the rfaR computation process
cat(castle)

