# =============================================================================
# MODIFIED PULS ROUTING - DM VERSION 2 (Function-Based)
# =============================================================================
# This version uses functions to encapsulate the routing logic.
# 
# Q: Are functions more efficient/faster than loops?
# A: In R, functions themselves don't make code faster - the loop is still there.
#    However, functions provide:
#      1. Reusability - call the same logic with different inputs
#      2. Testability - easier to unit test individual components
#      3. Readability - main code becomes cleaner
#      4. Maintainability - change logic in one place
#    
#    For SPEED improvements in R, you need:
#      - Vectorization (avoid loops entirely)
#      - Pre-allocation (don't grow vectors in loops)
#      - apply family functions (slightly faster than for loops)
#      - Rcpp (C++ code for hot loops)
#    
#    See modpuls_claude.R for the optimized version.

# Package Management -----------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, scales, patchwork, fs, ggsci, lubridate, zoo, janitor, gt)
theme_set(theme_bw())

# =============================================================================
# FUNCTIONS
# =============================================================================

#' Interpolate a value from reservoir model
#' @param x_vec Vector of x values to interpolate from
#' @param y_vec Vector of y values to interpolate from
#' @param x_out The x value to interpolate at
#' @param extrapolate If TRUE, use rule = 2 to extrapolate beyond range
#' @return Interpolated y value
interpolate_value <- function(x_vec, y_vec, x_out, extrapolate = TRUE) {
  rule <- if (extrapolate) 2 else 1
  approx(x = x_vec, y = y_vec, xout = x_out, rule = rule)$y
}

#' Calculate storage indicator: (2*S/dt) + O
#' @param storage Storage in cubic feet
#' @param outflow Outflow in cfs
#' @param dt Time step in seconds
#' @return Storage indicator value
calc_storage_indicator <- function(storage, outflow, dt) {
  (2 * storage / dt) + outflow
}

#' Calculate carry forward term: (2*S/dt) - O
#' @param storage Storage in cubic feet
#' @param outflow Outflow in cfs
#' @param dt Time step in seconds
#' @return Carry forward term value
calc_carry_forward <- function(storage, outflow, dt) {
  (2 * storage / dt) - outflow
}

#' Initialize routing at first timestep from elevation
#' @param elev Initial elevation in feet
#' @param res_model Reservoir model data frame
#' @param dt Time step in seconds
#' @return Named list with S, O, SI_plus, SI_minus
initialize_from_elevation <- function(elev, res_model, dt) {
  
  # Get storage from elevation
  S <- interpolate_value(res_model$elev_ft, res_model$stor_cuft, elev)
  
  # Get outflow from elevation
  O <- interpolate_value(res_model$elev_ft, res_model$outflow_cfs, elev)
  
  # Calculate storage indicators
  SI_plus <- calc_storage_indicator(S, O, dt)
  SI_minus <- calc_carry_forward(S, O, dt)
  
  list(
    S = S,
    O = O,
    SI_plus = SI_plus,
    SI_minus = SI_minus,
    elev = elev
  )
}

#' Route one timestep using Modified Puls
#' @param I_sum Sum of inflows: I[t-1] + I[t]
#' @param S_prev Previous storage in cubic feet
#' @param O_prev Previous outflow in cfs
#' @param res_model Reservoir model data frame
#' @param dt Time step in seconds
#' @return Named list with S, O, SI_plus, SI_minus, elev
route_timestep <- function(I_sum, S_prev, O_prev, res_model, dt) {
  

  # Carry forward term from previous timestep
  SI_minus <- calc_carry_forward(S_prev, O_prev, dt)
  
  # New storage indicator
  SI_plus <- SI_minus + I_sum
  
  # Look up outflow from storage indicator
  O <- interpolate_value(res_model$storage_indicator, res_model$outflow_cfs, SI_plus)
  
  # Look up storage from storage indicator
  S <- interpolate_value(res_model$storage_indicator, res_model$stor_cuft, SI_plus)
  
  # Look up elevation from storage
  elev <- interpolate_value(res_model$stor_cuft, res_model$elev_ft, S)
  
  list(
    S = S,
    O = O,
    SI_plus = SI_plus,
    SI_minus = SI_minus,
    elev = elev
  )
}

#' Run complete Modified Puls routing
#' @param inflow Data frame with time_hour and flow_cfs columns
#' @param res_model Reservoir model with elev_ft, stor_cuft, outflow_cfs, storage_indicator
#' @param initial_elev Starting water surface elevation in feet
#' @param dt Time step in seconds
#' @return Data frame with routing results
run_modpuls_routing <- function(inflow, res_model, initial_elev, dt) {
  
  n <- nrow(inflow)
  
  # Pre-allocate results vectors (important for performance!)
  results <- tibble(
    time_hour = inflow$time_hour,
    I = inflow$flow_cfs,
    I_sum = rollsum(inflow$flow_cfs, k = 2, align = "right", fill = NA),
    SI_minus = numeric(n),
    SI_plus = numeric(n),
    O_t = numeric(n),
    S_t = numeric(n),
    Elev_t = numeric(n)
  )
  
  # Initialize first timestep
  init <- initialize_from_elevation(initial_elev, res_model, dt)
  results$S_t[1] <- init$S
  results$O_t[1] <- init$O
  results$SI_plus[1] <- init$SI_plus
  results$SI_minus[1] <- init$SI_minus
  results$Elev_t[1] <- init$elev
  
  # Route remaining timesteps
 for (i in 2:n) {
    step <- route_timestep(
      I_sum = results$I_sum[i],
      S_prev = results$S_t[i - 1],
      O_prev = results$O_t[i - 1],
      res_model = res_model,
      dt = dt
    )
    
    results$S_t[i] <- step$S
    results$O_t[i] <- step$O
    results$SI_plus[i] <- step$SI_plus
    results$SI_minus[i] <- step$SI_minus
    results$Elev_t[i] <- step$elev
  }
  
  results
}

#' Prepare reservoir model with storage indicator
#' @param res_model Raw reservoir model with elev_ft, stor_acft, outflow_cfs
#' @param dt Time step in seconds
#' @param sqft_per_acre Conversion factor (default 43560)
#' @return Reservoir model with stor_cuft and storage_indicator columns added
prepare_reservoir_model <- function(res_model, dt, sqft_per_acre = 43560) {
  res_model %>%
    mutate(
      stor_cuft = stor_acft * sqft_per_acre,
      storage_indicator = calc_storage_indicator(stor_cuft, outflow_cfs, dt)
    )
}

#' Print routing summary
#' @param results Routing results data frame
print_routing_summary <- function(results) {
  cat("=== Modified Puls Routing Summary ===\n")
  cat("Peak Inflow:", max(results$I), "cfs\n")
  cat("Peak Outflow:", max(results$O_t, na.rm = TRUE), "cfs\n")
  cat("Peak Reduction:", round((1 - max(results$O_t, na.rm = TRUE) / max(results$I)) * 100, 1), "%\n")
  cat("Max Elevation:", max(results$Elev_t, na.rm = TRUE), "ft\n")
  cat("Time to Peak Inflow:", results$time_hour[which.max(results$I)], "hours\n")
  cat("Time to Peak Outflow:", results$time_hour[which.max(results$O_t)], "hours\n")
  cat("Lag:", results$time_hour[which.max(results$O_t)] - results$time_hour[which.max(results$I)], "hours\n")
}

# =============================================================================
# MAIN SCRIPT
# =============================================================================

# Load Data --------------------------------------------------------------------
inflow <- dir_ls("data/mod_puls/", glob = "*cherry_creek_inflow.csv", recurse = TRUE) %>%
  read_csv()

res_model <- dir_ls("data/mod_puls/", glob = "*cherry_creek_stage_stor_dis.csv", recurse = TRUE) %>%
  read_csv()

# Constants --------------------------------------------------------------------
dt <- 3600          # 1 hour in seconds
initial_elev <- 5565 # Starting elevation in feet

# Prepare reservoir model ------------------------------------------------------
res_model <- prepare_reservoir_model(res_model, dt)

# Run routing ------------------------------------------------------------------
results <- run_modpuls_routing(inflow, res_model, initial_elev, dt)

# Print summary ----------------------------------------------------------------
print_routing_summary(results)

# Optional: Plot results -------------------------------------------------------
# results %>%
#   select(time_hour, I, O_t) %>%
#   pivot_longer(cols = c(I, O_t), names_to = "type", values_to = "flow") %>%
#   ggplot(aes(x = time_hour, y = flow, color = type)) +
#   geom_line(linewidth = 1) +
#   labs(title = "Modified Puls Routing Results",
#        x = "Time (hours)", y = "Flow (cfs)")