#' Modified Puls Reservoir Routing
#'
#' Performs Modified Puls (level pool) routing of inflow hydrograph given
#' a defined reservoir geometry (Stage (ft), Storage (ac-ft), Discharge (cfs))
#'
#' @param resmodel_df Data frame with three columns: elevation/stage (ft),
#'   storage (acre-feet), and discharge (cfs). Must be in that order.
#' @param inflow_df Data frame with two columns: time (hours - for now) and
#'   inflow (cfs). Must be in that order.
#' @param initial_elev Starting water surface elevation in feet.
#' @param full_results Logical. If `FALSE` (default), returns only peak
#'   stage and discharge. If `TRUE`, returns the complete routing result.
#'
#' @return If `full_results = FALSE`, a named numeric vector with
#'   `peak_stage_ft` and `peak_discharge_cfs`. If `full_results = TRUE`,
#'   a data frame with columns: `time_hour`, `inflow_cfs`, `elevation_ft`,
#'   `storage_acft`, and `outflow_cfs`.
#'
#' @export
#'
#' @examples
#' # Peak values only
#' mod_puls_routing(example_resmodel, example_inflow, initial_elev = 5565)
#'
#' # Full routing table
#' mod_puls_routing(example_resmodel, example_inflow, initial_elev = 5565, full_results = TRUE)
#' @references
#' Chow, V.T. (1959). Open-Channel Hydraulics. McGraw-Hill.
mod_puls_routing <- function(resmodel_df,inflow_df, initial_elev, full_results = FALSE){
  # ============================================================================
  # CONVERT TO DF (TIBBLE HANDLING)
  # ============================================================================
  resmodel_df <- as.data.frame(resmodel_df)
  inflow_df <- as.data.frame(inflow_df)

  # ============================================================================
  # ENUSRE NUMERIC DATA (NOT INTERGER)
  # ============================================================================
  resmodel_df[] <- lapply(resmodel_df, as.numeric)
  inflow_df[] <- lapply(inflow_df, as.numeric)

  # ============================================================================
  # CONSTANTS
  # ============================================================================
  # Time step in seconds
  dt <- 3600L

  # Ac-ft to cubic feet
  SQFT_PER_ACRE <- 43560L

  # Starting elevation in feet
  INITIAL_ELEV <- initial_elev

  # =============================================================================
  # PREPARE RESERVOIR MODEL - STAGE, STOR, DISCHARGE
  # =============================================================================
  # Convert storage to cubic feet and calculate storage indicator lookup
  res_elev <- resmodel_df[,1]
  res_stor_cuft <- resmodel_df[,2] * SQFT_PER_ACRE
  res_outflow <- resmodel_df[,3]

  # Storage indicator
  res_SI <- (2 * res_stor_cuft / dt) + res_outflow

  # =============================================================================
  # CREATE INTERPOLATION FUNCTIONS
  # =============================================================================
  # approxfun() creates a function that can be called repeatedly.
  # rule 2: extrapolate using the nearest value if outside range

  # Initiation (t1): starting elevation -> starting storage, starting outflow
  interp_elev_to_stor <- approxfun(res_elev, res_stor_cuft, rule = 2)
  interp_elev_to_outflow <- approxfun(res_elev, res_outflow, rule = 2)

  # Routing (t2 - tINF): storage indicator -> outflow, storage indicator -> storage
  interp_SI_to_outflow <- approxfun(res_SI, res_outflow, rule = 2)
  interp_SI_to_stor <- approxfun(res_SI, res_stor_cuft, rule = 2)

  # Elevation from storage (output)
  interp_stor_to_elev <- approxfun(res_stor_cuft, res_elev, rule = 2)

  # =============================================================================
  # PREPARE INFLOW DATA
  # =============================================================================
  # Length of timeseries
  n <- nrow(inflow_df)

  # Extract as vector (faster than data frame column access in loop)
  time_vec <-inflow_df[,1]
  I_vec <- inflow_df[,2]

  # Calculate I[t-1] + I[t] for all timesteps at once
  # For t=1, this will be NA (no previous value)
  I_sum <- c(NA_real_, I_vec[-n] + I_vec[-1])

  # =============================================================================
  # PRE-ALLOCATE OUTPUT VECTORS
  # =============================================================================
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
  # ROUTING LOOP
  # =============================================================================
  # Store dt_factor to avoid repeated division (multiplication is faster)
  two_over_dt <- 2 / dt

  # Max Stage & Max Discharge
  peak_stage <- 0
  peak_dis <- 0

  # Routing
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
  # FULL ROUTING RESULTS
  # =============================================================================
  # If full routing results desired
  if (full_results) {
    # Return the full data frame
    results <- data.frame(
      time_hour = time_vec,
      inflow_cfs = I_vec,
      elevation_ft = elev_vec,
      storage_acft = S_vec/SQFT_PER_ACRE,
      outflow_cfs = O_vec
    )
    return(results)
  } else {
    # Return just the peak values
    return(c(
      peak_stage_ft = peak_stage,
      peak_discharge_cfs = peak_dis
    ))
  }
}
