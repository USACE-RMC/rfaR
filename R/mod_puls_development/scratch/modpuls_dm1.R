# =============================================================================
# MODIFIED PULS ROUTING - DM VERSION 1
# =============================================================================
# Package Management -----------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, scales, patchwork, fs, ggsci, lubridate, zoo, janitor, gt)
theme_set(theme_bw())

# Load Data --------------------------------------------------------------------
inflow <- dir_ls("data/mod_puls/", glob = "*cherry_creek_inflow.csv", recurse = TRUE) %>%
  read_csv()

res_model <- dir_ls("data/mod_puls/", glob = "*cherry_creek_stage_stor_dis.csv", recurse = TRUE) %>%
  read_csv()

hms_results <- dir_ls("data/mod_puls/", glob = "*cherry_creek_hms_results.csv", recurse = TRUE) %>%
  read_csv()

# Constants --------------------------------------------------------------------
# Time step - One Hour converted to seconds for CFS calculations
dt <- 1 * 60 * 60  # 3600 seconds

# Storage conversion - acre-ft to cubic feet
sqft_acre <- 43560

# Starting Elevation (feet)
h_1 <- 5565

# Storage Indicator Calculation ------------------------------------------------
# Add storage indicator column: (2*S/dt) + O
res_model <- res_model %>%
  mutate(
    stor_cuft = stor_acft * sqft_acre,
    storage_indicator = (2 * stor_cuft / dt) + outflow_cfs
  )

# Set up Routing Table ---------------------------------------------------------
# Initialize with renamed columns (no special characters)
modpuls <- inflow %>%
  mutate(
    I_sum = rollsum(flow_cfs, k = 2, align = "right", fill = NA),  # I[t-1] + I[t]
    SI_minus = 0,      # (2*S[t-1]/dt) - O[t-1] : carry forward term, storage indicator at time t-1 - Outflow at t-1
    SI_plus = 0,       # (2*S[t]/dt) + O[t] : storage indicator at time t
    O_t = 0,           # Outflow at time t
    S_t = 0,           # Storage at time t (cubic feet)
    Elev_t = 0         # Elevation at time t (feet)
  )

# Routing Loop -----------------------------------------------------------------
for (i in 1:nrow(modpuls)) {
  
  # First timestep - Initialize from starting elevation
  if (i == 1) {
    # Set initial elevation
    modpuls$Elev_t[i] <- h_1
    
    # Use elevation to get storage (interpolate from reservoir model)
    modpuls$S_t[i] <- approx(
      x = res_model$elev_ft,
      y = res_model$stor_cuft,
      xout = modpuls$Elev_t[i]
    )$y
    
    # Use elevation to get outflow (interpolate from reservoir model)
    modpuls$O_t[i] <- approx(
      x = res_model$elev_ft,
      y = res_model$outflow_cfs,
      xout = modpuls$Elev_t[i]
    )$y
    
    # Calculate initial storage indicator
    modpuls$SI_plus[i] <- (2 * modpuls$S_t[i] / dt) + modpuls$O_t[i]
    modpuls$SI_minus[i] <- (2 * modpuls$S_t[i] / dt) - modpuls$O_t[i]
    
  } else {
    # Subsequent timesteps - Apply Modified Puls routing
    
    # Carry forward term from previous timestep: (2*S[t-1]/dt) - O[t-1]
    modpuls$SI_minus[i] <- 2 * modpuls$S_t[i - 1] / dt - modpuls$O_t[i - 1]
    
    # New storage indicator: (I[t-1] + I[t]) + (2*S[t-1]/dt - O[t-1])
    modpuls$SI_plus[i] <- modpuls$SI_minus[i] + modpuls$I_sum[i]
    
    # Look up outflow from storage indicator
    modpuls$O_t[i] <- approx(
      x = res_model$storage_indicator,
      y = res_model$outflow_cfs,
      xout = modpuls$SI_plus[i]
    )$y
    
    # Look up storage from storage indicator
    modpuls$S_t[i] <- approx(
      x = res_model$storage_indicator,
      y = res_model$stor_cuft,
      xout = modpuls$SI_plus[i]
    )$y
    
    # Look up elevation from storage
    modpuls$Elev_t[i] <- approx(
      x = res_model$stor_cuft,
      y = res_model$elev_ft,
      xout = modpuls$S_t[i]
    )$y
  }
}

# Results Summary --------------------------------------------------------------
cat("Peak Inflow:", max(modpuls$flow_cfs), "cfs\n")
cat("Peak Outflow:", max(modpuls$O_t, na.rm = TRUE), "cfs\n")
cat("Peak Reduction:", round((1 - max(modpuls$O_t, na.rm = TRUE) / max(modpuls$flow_cfs)) * 100, 1), "%\n")
cat("Max Elevation:", max(modpuls$Elev_t, na.rm = TRUE), "ft\n")

# Results ----------------------------------------------------------------------
# Do the answers make sense?
colors_i_like <- c("#3B4992FF","#EE0000FF","#008B45FF","#631879FF","#008280FF","#BB0021FF","#5F559BFF","#A20056FF","#808180FF","#1B1919FF")
# colour_vector <- palettes::as_color(pal_aaas("default")(10))

ggplot(modpuls) + 
  geom_line(aes(x = time_hour, y = flow_cfs),color = "#3B4992FF", linewidth = 0.75) + 
  geom_line(aes(x = time_hour, y = O_t),color = "#EE0000FF", linewidth = 0.75) + 
  scale_x_continuous(breaks = seq(0,960,24)) + 
  coord_cartesian(xlim = c(0,144))

ggplot(modpuls) + 
  geom_line(aes(x = time_hour, y = Elev_t),color = "#008B45FF", linewidth = 0.75) + 
  scale_x_continuous(breaks = seq(0,960,24)) + 
  coord_cartesian(xlim = c(0,500))
