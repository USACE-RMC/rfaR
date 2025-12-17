# =============================================================================
# MODIFIED PULS ROUTING
# =============================================================================
# 
# OPTIMIZATION STRATEGIES USED:
# 
# 1. PRE-ALLOCATION: All vectors are created at full size before the loop.
#    R is slow when you grow vectors incrementally (e.g., x <- c(x, new_val))
#    because it copies the entire vector each time.
#
# 2. VECTORIZED INTERPOLATION FUNCTION: Using approxfun() instead of approx()
#    creates a function that's faster for repeated calls because it only
#    builds the interpolation structure once.
#
# 3. MINIMAL OBJECT CREATION IN LOOP: Avoid creating lists/data frames inside
#    the loop. Just update pre-allocated vectors directly.
#
# 4. DIRECT VECTOR ACCESS: Using vectors instead of data frame columns in the
#    hot loop avoids the overhead of data frame indexing.
#
# 5. AVOID UNNECESSARY CALCULATIONS: Don't compute things you don't need.
#
# WHY WE CAN'T FULLY VECTORIZE:
# The Modified Puls method is inherently sequential - each timestep depends
# on the previous timestep's results. This means we MUST use a loop.
# However, we make each iteration as fast as possible.
#
# FOR EVEN MORE SPEED:
# - Rcpp: Write the loop in C++ (10-100x faster)
# - data.table: Faster than tidyverse for large datasets
# - parallel: If routing multiple scenarios, parallelize across scenarios
#
# =============================================================================

# Package Management -----------------------------------------------------------
# Using base R where possible for speed; tidyverse only for I/O and plotting
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, zoo, fs)

# =============================================================================
# LOAD DATA
# =============================================================================

inflow <- dir_ls("data/mod_puls/", glob = "*cherry_creek_inflow.csv", recurse = TRUE) %>%
  read_csv(show_col_types = FALSE)

res_model <- dir_ls("data/mod_puls/", glob = "*cherry_creek_stage_stor_dis.csv", recurse = TRUE) %>%
  read_csv(show_col_types = FALSE)

hms_results <- dir_ls("data/mod_puls/", glob = "*cherry_creek_hms_results.csv", recurse = TRUE) %>%
  read_csv()

# =============================================================================
# CONSTANTS
# =============================================================================

dt <- 3600L           # Time step in seconds (L makes it integer - slightly faster)
SQFT_PER_ACRE <- 43560L
INITIAL_ELEV <- 5565  # Starting elevation in feet

# =============================================================================
# PREPARE RESERVOIR MODEL
# =============================================================================

# Convert storage to cubic feet and calculate storage indicator ONCE
# These are the lookup tables we'll interpolate from
res_elev <- res_model$elev_ft
res_stor_cuft <- res_model$stor_acft * SQFT_PER_ACRE
res_outflow <- res_model$outflow_cfs

# Storage indicator
res_SI <- (2 * res_stor_cuft / dt) + res_outflow

# =============================================================================
# CREATE INTERPOLATION FUNCTIONS (KEY OPTIMIZATION!)
# =============================================================================
# approxfun() creates a function that can be called repeatedly.
# This is MUCH faster than calling approx() in a loop because:
#   - approx() rebuilds the interpolation structure every call
#   - approxfun() builds it once, then just evaluates
#
# rule = 2 means: extrapolate using the nearest value if outside range

# For initialization: elevation -> storage, elevation -> outflow
interp_elev_to_stor <- approxfun(res_elev, res_stor_cuft, rule = 2)
interp_elev_to_outflow <- approxfun(res_elev, res_outflow, rule = 2)

# For routing: storage indicator -> outflow, storage indicator -> storage
interp_SI_to_outflow <- approxfun(res_SI, res_outflow, rule = 2)
interp_SI_to_stor <- approxfun(res_SI, res_stor_cuft, rule = 2)

# For getting elevation from storage (for output)
interp_stor_to_elev <- approxfun(res_stor_cuft, res_elev, rule = 2)

# =============================================================================
# PREPARE INFLOW DATA
# =============================================================================

n <- nrow(inflow)

# Extract as vector (faster than data frame column access in loop)
I_vec <- inflow$flow_cfs
time_vec <- inflow$time_hour

# Calculate I[t-1] + I[t] for all timesteps at once (VECTORIZED!)
# This avoids calculating it inside the loop
# For t=1, this will be NA (no previous value)
I_sum <- c(NA_real_, I_vec[-n] + I_vec[-1])

# =============================================================================
# PRE-ALLOCATE OUTPUT VECTORS
# =============================================================================
# This is CRITICAL for performance in R!
# Pre-allocating with numeric(n) is much faster than growing vectors

O_vec <- numeric(n)      # Outflow at each timestep
S_vec <- numeric(n)      # Storage at each timestep  
elev_vec <- numeric(n)   # Elevation at each timestep

# =============================================================================
# INITIALIZE FIRST TIMESTEP
# =============================================================================

# Set initial elevation
elev_vec[1] <- INITIAL_ELEV

# Look up initial storage and outflow from elevation
S_vec[1] <- interp_elev_to_stor(INITIAL_ELEV)
O_vec[1] <- interp_elev_to_outflow(INITIAL_ELEV)

# =============================================================================
# MAIN ROUTING LOOP
# =============================================================================
# This is as lean as possible:
#   - No function calls except the pre-built interpolation functions
#   - No object creation (just updating pre-allocated vectors)
#   - Direct vector indexing
#   - Minimal arithmetic operations

# Store dt_factor to avoid repeated division (multiplication is faster)
two_over_dt <- 2 / dt

# Max Stage & Max Discharge
peak_stage <- 0
peak_dis <- 0

for (t in 2:n) {
  # Calculate carry-forward term: (2*S[t-1]/dt) - O[t-1]
  # Then add inflow sum to get new storage indicator
  SI_new <- (S_vec[t - 1] * two_over_dt) - O_vec[t - 1] + I_sum[t]
  
  # Look up outflow and storage from storage indicator
  O_vec[t] <- interp_SI_to_outflow(SI_new)
  S_vec[t] <- interp_SI_to_stor(SI_new)
  
  # Look up elevation from storage
  elev_vec[t] <- interp_stor_to_elev(S_vec[t])
  
  # Record Peak Stage & Discharge
  if (elev_vec[t] > peak_stage) peak_stage <- elev_vec[t]
  if (O_vec[t] > peak_dis) peak_dis <- O_vec[t]

}

# =============================================================================
# ASSEMBLE RESULTS
# =============================================================================
# Only create the data frame AFTER the loop completes

results <- tibble(
  time_hour = time_vec,
  inflow_cfs = I_vec,
  outflow_cfs = O_vec,
  storage_cuft = S_vec,
  elevation_ft = elev_vec
)

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

peak_in <- max(I_vec)
peak_out <- max(O_vec)
peak_elev <- max(elev_vec)
time_peak_in <- time_vec[which.max(I_vec)]
time_peak_out <- time_vec[which.max(O_vec)]

cat("\n")
cat("========================================\n")
cat("   MODIFIED PULS ROUTING RESULTS\n
")
cat("========================================\n")
cat(sprintf("Peak Inflow:      %,.0f cfs at hour %d\n", peak_in, time_peak_in))
cat(sprintf("Peak Outflow:     %,.0f cfs at hour %d\n", peak_out, time_peak_out))
cat(sprintf("Peak Reduction:   %.1f%%\n", (1 - peak_out / peak_in) * 100))
cat(sprintf("Lag Time:         %d hours\n", time_peak_out - time_peak_in))
cat(sprintf("Max Elevation:    %.2f ft\n", peak_elev))
cat("========================================\n")

# =============================================================================
# OPTIONAL: BENCHMARK COMPARISON
# =============================================================================
# Uncomment to compare timing with the loop-based version
#
# library(microbenchmark)
# 
# # Wrap the routing in a function for benchmarking
# route_optimized <- function() {
#   O <- numeric(n)
#   S <- numeric(n)
#   S[1] <- interp_elev_to_stor(INITIAL_ELEV)
#   O[1] <- interp_elev_to_outflow(INITIAL_ELEV)
#   for (t in 2:n) {
#     SI <- (S[t-1] * two_over_dt) - O[t-1] + I_sum[t]
#     O[t] <- interp_SI_to_outflow(SI)
#     S[t] <- interp_SI_to_stor(SI)
#   }
#   O
# }
#
# microbenchmark(route_optimized(), times = 100)

# =============================================================================
# OPTIONAL: QUICK PLOT
# =============================================================================
# Uncomment to generate a plot

# ggplot(results, aes(x = time_hour)) +
#   geom_line(aes(y = inflow_cfs, color = "Inflow"), linewidth = 1) +
#   geom_line(aes(y = outflow_cfs, color = "Outflow"), linewidth = 1) +
#   scale_color_manual(values = c("Inflow" = "steelblue", "Outflow" = "firebrick")) +
#   labs(
#     title = "Modified Puls Reservoir Routing",
#     subtitle = sprintf("Peak reduction: %.1f%% | Lag: %d hours", 
#                        (1 - peak_out/peak_in) * 100, time_peak_out - time_peak_in),
#     x = "Time (hours)",
#     y = "Flow (cfs)",
#     color = NULL
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom")