#' Hydrograph Scaling
#'
#' Scales inflow hydrograph given a sampled inflow volume from the volume-frequency
#' curve
#'
#' Scale factor is defined by the sampled inflow volume and the maximum volume of the corresponding duration
#'

# Function----

scale_hydrograph <- function(hydrograph_shape, sampled_volume, critical_duration, routing_days){
  # ============================================================================
  # CONVERT TO DF (TIBBLE HANDLING)
  # ============================================================================
  hydrograph_shape <- as.data.frame(hydrograph_shape)

  # ============================================================================
  # ENUSRE NUMERIC DATA (NOT INTERGER)
  # ============================================================================
  hydrograph_shape[] <- lapply(hydrograph_shape, as.numeric)

  # ============================================================================
  # CONSTANTS
  # ============================================================================
  # Hard-coding for hourly routing
  routing_timestep_hrs <- 1

  # Add functionality for dt (for non-hourly inflow timeseries)
  timestep_days <- 1/24
  critdur_hrs <- critical_duration/timestep_days

  routing_timestep_days <- routing_timestep_hrs/24
  routing_hrs <- routing_days/routing_timestep_days

  # ============================================================================
  # SCALE FACTOR
  # ============================================================================
  # Use rollmeanr (r for "right")
  max_hydrograph_vol <- max(zoo::rollmeanr(hydrograph_shape[,2], k = critdur_hrs))

  # Ratio to Sampled 2-day vol
  scale_factor <- sampled_volume/max_hydrograph_vol

  # ============================================================================
  # SCALE HYDROGRAPH SHAPE
  # ============================================================================
  # Preallocate scaled hydrograph time
  scaled_hydro_time <- seq(1,routing_hrs,routing_timestep_hrs)

  # Scale hydro
  scaled_inflow <- hydrograph_shape[,2]*scale_factor

  # Extend hydro to time series length
  # Should this be a recession formula instead of just 0?
  scaled_inflow <- c(scaled_inflow, numeric(length(scaled_hydro_time) - length(scaled_inflow)))

  # ============================================================================
  # RETURN SCALED HYDROGRAPH
  # ============================================================================
  scaled_hydrograph <- data.frame(time_hrs = scaled_hydro_time, inflow_cfs = scaled_inflow)

  return(scaled_hydrograph)

}

# Inflow Hydro ---
jmd_1955 <- read.csv("D:/0.RMC/Reefer/rfaR/data-raw/JMD/hydrographs/May_1955.csv")

# critical duration (days) ---
crit_dur <- 2

# Max 2-day inflow volume --- (make sure this works, there might be a better way)
# Future dev - add dt here
timestep_days <- 1/24
critdur_hrs <- crit_dur/timestep_days

jmd_1955_max <- max(zoo::rollmean(jmd_1955[,2], k = critdur_hrs,align = "right"))

# Ratio to Sampled 2-day vol ---
sampled_vol <- 129434.53

ratio_1955 <- sampled_vol/jmd_1955_max

# Scale to sampled 2-day vol & length of routing ---
# Routing length
routing_days <- 10
routing_timestep_hrs <- 1
routing_timestep_days <- routing_timestep_hrs/24
routing_hrs <- routing_days/routing_timestep_days

# Preallocate scaled hydrograph time
scaled_hydro_time <- seq(1,routing_hrs,routing_timestep_hrs)

# Scale hydro ---
scaled_hydro_1955 <- jmd_1955[,2]*ratio_1955

# Extend hydro to time series length ---
# Should this be a recession formula instead of just 0?
scaled_hydro_1955 <- c(scaled_hydro_1955, numeric(length(scaled_hydro_time) - length(scaled_hydro_1955)))

# export DF
scaled_1955 <- data.frame(time_hrs = scaled_hydro_time, inflow_cfs = scaled_hydro_1955)

jmd_1955_test <- scale_hydrograph(jmd_1955, 129434.53, 2, 10)

scaled_1955$Source <- "Base"
jmd_1955_test$Source <- "Function"

comp_df <- rbind(scaled_1955,jmd_1955_test)

ggplot(comp_df) +
  geom_line(aes(x = time_hrs, y = inflow_cfs, color = Source))+
  facet_wrap(vars(Source))
