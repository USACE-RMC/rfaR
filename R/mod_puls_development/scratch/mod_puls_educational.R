# =============================================================================
# MODIFIED PULS ROUTING - EDUCATIONAL VERSION
# =============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# If these packages aren't installed, pacman automatically installs them from CRAN before loading them.
# https://www.r-bloggers.com/2016/04/let-pacman-eat-up-library-and-require-2/

pacman::p_load(tidyverse, scales, 
               patchwork, fs, ggsci, 
               lubridate, zoo, janitor, gt)

# --- Load your data ---
inflow <- dir_ls("data/mod_puls/",glob = "*cherry_creek_inflow.csv",recurse = T) %>%
  read_csv()

res_model <- dir_ls("data/mod_puls/",glob = "*cherry_creek_stage_stor_dis.csv", recurse = T) %>%
  read_csv()

hms_results <- dir_ls("data/mod_puls/",glob = "*cherry_creek_hms_results.csv",recurse = T) %>%
  read_csv()

# --- Constants ---
dt <- 3600            # time step in seconds (1 hour)
sqft_per_acre <- 43560
initial_elev <- 5565  # starting water surface elevation (ft)

# =============================================================================
# STEP 1: Build the Storage Indicator Table
# =============================================================================
# The "storage indicator" is: (2*S/dt) + O
# We'll look up O (and S) given a value of the storage indicator

res_model <- res_model %>%
  mutate(
    stor_cuft = stor_acft * sqft_per_acre,  # convert acre-ft to cubic ft
    storage_indicator = (2 * stor_cuft / dt) + outflow_cfs
  )

# Quick plot to visualize the relationship
ggplot(res_model, aes(x = storage_indicator, y = outflow_cfs)) +
  geom_line() +
  labs(title = "Storage Indicator vs Outflow",
       x = "Storage Indicator (2S/dt + O)",
       y = "Outflow (cfs)")

# =============================================================================
# STEP 2: Initialize the routing table
# =============================================================================
n <- nrow(inflow)

routing <- tibble(
  time_hour = inflow$time_hour,
  I = inflow$flow_cfs,      # Inflow at this timestep
  O = NA_real_,             # Outflow (to be computed)
  S = NA_real_,             # Storage in cubic feet (to be computed)
  elev = NA_real_,          # Water surface elevation (to be computed)
  SI = NA_real_             # Storage indicator (2S/dt + O)
)

# =============================================================================
# STEP 3: Set Initial Conditions (t = 1)
# =============================================================================
# Given: initial elevation
# Find: initial storage, outflow, and storage indicator

routing$elev[1] <- initial_elev

routing$S[1] <- approx(
  x = res_model$elev_ft, 
  y = res_model$stor_cuft, 
  xout = routing$elev[1]
)$y

routing$O[1] <- approx(
  x = res_model$elev_ft, 
  y = res_model$outflow_cfs, 
  xout = routing$elev[1]
)$y

routing$SI[1] <- (2 * routing$S[1] / dt) + routing$O[1]

# =============================================================================
# STEP 4: Route the Hydrograph (t = 2 to n)
# =============================================================================
# The core equation:
#   SI[t] = (I[t-1] + I[t]) + (2*S[t-1]/dt - O[t-1])
#         = (I[t-1] + I[t]) + SI[t-1] - 2*O[t-1]

for (t in 2:n) {
  
  # Average inflow (actually sum, since the 2's cancel)
  I_sum <- routing$I[t-1] + routing$I[t]
  
  # Previous storage indicator minus twice previous outflow
  # This equals (2*S[t-1]/dt - O[t-1])
  carry_forward <- routing$SI[t-1] - 2 * routing$O[t-1]
  
  # New storage indicator
  SI_new <- I_sum + carry_forward
  routing$SI[t] <- SI_new
  
  # Look up outflow from storage indicator
  routing$O[t] <- approx(
    x = res_model$storage_indicator,
    y = res_model$outflow_cfs,
    xout = SI_new,
    rule = 2  # extrapolate if needed
  )$y
  
  # Look up storage from storage indicator
  routing$S[t] <- approx(
    x = res_model$storage_indicator,
    y = res_model$stor_cuft,
    xout = SI_new,
    rule = 2
  )$y
  
  # Look up elevation from storage
  routing$elev[t] <- approx(
    x = res_model$stor_cuft,
    y = res_model$elev_ft,
    xout = routing$S[t],
    rule = 2
  )$y
}

# =============================================================================
# STEP 5: View Results
# =============================================================================
routing %>%
  select(time_hour, I, O, elev) %>%
  pivot_longer(cols = c(I, O), names_to = "type", values_to = "flow_cfs") %>%
  ggplot(aes(x = time_hour, y = flow_cfs, color = type)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("I" = "#3B4992FF", "O" = "#EE0000FF"),
                     labels = c("Inflow", "Outflow")) +
  labs(title = "Modified Puls Reservoir Routing",
       x = "Time (hours)",
       y = "Flow (cfs)",
       color = "") +
  theme_minimal()

# Peak attenuation
cat("Peak Inflow:", max(routing$I), "cfs\n")
cat("Peak Outflow:", max(routing$O, na.rm = TRUE), "cfs\n")
cat("Peak Reduction:", round((1 - max(routing$O, na.rm = TRUE)/max(routing$I)) * 100, 1), "%\n")
cat("Max Elevation:", max(routing$elev, na.rm = TRUE), "ft\n")

