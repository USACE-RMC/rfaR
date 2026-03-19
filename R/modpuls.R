#' Modified Puls Reservoir Routing
#'
#' Performs Modified Puls (level pool) routing of inflow hydrograph given
#' a defined reservoir geometry (Stage (ft), Storage (ac-ft), Discharge (cfs)).
#' The computation interval is inferred automatically from the time column of
#' the inflow hydrograph — use \code{routable_hydrograph()} to resample to the
#' desired computation interval before routing.
#'
#' @param resmodel_df Data frame with three columns: elevation/stage (ft),
#'   storage (acre-feet), and discharge (cfs). Must be in that order.
#' @param inflow_df Data frame with two columns: time (hours) and
#'   inflow (cfs). Must be in that order.
#' @param initial_elev Starting water surface elevation in feet.
#' @param full_results Logical. If `FALSE` (default), returns only peak
#'   stage and discharge. If `TRUE`, returns the complete routing result.
#'
#' @return If `full_results = FALSE`, a named numeric vector with
#'   `peak_stage_ft` and `peak_discharge_cfs`.
#'
#'   If `full_results = TRUE`, a data frame with columns: `time_hr`,
#'   `inflow_cfs`, `elevation_ft`, `storage_acft`, and `outflow_cfs`.
#'
#' @export
#'
#' @examples
#'
#' # Example hydrograph. Requires pre-processing
#' hydro_example <- hydrograph_setup(jmd_hydro_jun1965_15min, critical_duration = 2, routing_days = 10)
#' hydrograph_shape <- hydro_example[[1]][, 2:3]
#' scaled_hydrograph <- scale_hydrograph(hydrograph_shape,
#'                                       observed_volume = 50000,
#'                                       sampled_volume = 55000)
#'
#' # Peak values only
#' mod_puls_routing(jmd_resmodel, scaled_hydrograph, initial_elev = 3830)
#'
#' # Full routing table
#' mod_puls_routing(jmd_resmodel, scaled_hydrograph, initial_elev = 3830, full_results = TRUE)
#'
#' @references
#' Chow, V.T. (1959). Open-Channel Hydraulics. McGraw-Hill.
mod_puls_routing <- function(resmodel_df, inflow_df, initial_elev, full_results = FALSE) {

  # CONVERT TO DF (TIBBLE HANDLING) ============================================
  resmodel_df <- as.data.frame(resmodel_df)
  inflow_df   <- as.data.frame(inflow_df)

  # ENSURE NUMERIC DATA (NOT INTEGER) ==========================================
  resmodel_df[] <- lapply(resmodel_df, as.numeric)
  inflow_df[]   <- lapply(inflow_df, as.numeric)

  # CONSTANTS ==================================================================
  SQFT_PER_ACRE <- 43560L
  INITIAL_ELEV  <- initial_elev

  # INFER COMPUTATION INTERVAL FROM TIME COLUMN ================================
  # Time column is in hours; convert to seconds for routing math.
  # routable_hydrograph() guarantees a uniform timestep before this is called.
  # hours
  dt_hr <- inflow_df[2, 1] - inflow_df[1, 1]

  # seconds
  dt    <- dt_hr * 3600

  # PREPARE RESERVOIR MODEL ====================================================
  res_elev      <- resmodel_df[, 1]
  res_stor_cuft <- resmodel_df[, 2] * SQFT_PER_ACRE
  res_outflow   <- resmodel_df[, 3]

  # Storage indicator lookup table
  res_SI <- (2 * res_stor_cuft / dt) + res_outflow

  # CREATE INTERPOLATION FUNCTIONS =============================================
  interp_elev_to_stor    <- approxfun(res_elev,      res_stor_cuft, rule = 2)
  interp_elev_to_outflow <- approxfun(res_elev,      res_outflow,   rule = 2)
  interp_SI_to_outflow   <- approxfun(res_SI,        res_outflow,   rule = 2)
  interp_SI_to_stor      <- approxfun(res_SI,        res_stor_cuft, rule = 2)
  interp_stor_to_elev    <- approxfun(res_stor_cuft, res_elev,      rule = 2)

  # PREPARE INFLOW DATA ========================================================
  n        <- nrow(inflow_df)
  time_vec <- inflow_df[, 1]
  I_vec    <- inflow_df[, 2]

  # Pre-compute I[t-1] + I[t] for all timesteps (NA for t=1)
  I_sum <- c(NA_real_, I_vec[-n] + I_vec[-1])

  # PRE-ALLOCATE OUTPUT VECTORS ================================================
  O_vec    <- numeric(n)
  S_vec    <- numeric(n)
  elev_vec <- numeric(n)

  # INITIALIZE FIRST TIMESTEP ==================================================
  elev_vec[1] <- INITIAL_ELEV
  S_vec[1]    <- interp_elev_to_stor(INITIAL_ELEV)
  O_vec[1]    <- interp_elev_to_outflow(INITIAL_ELEV)

  # ROUTING LOOP ===============================================================
  two_over_dt <- 2 / dt
  peak_stage  <- elev_vec[1]
  peak_dis    <- O_vec[1]

  for (t in 2:n) {
    SI_new      <- (S_vec[t - 1] * two_over_dt) - O_vec[t - 1] + I_sum[t]
    O_vec[t]    <- interp_SI_to_outflow(SI_new)
    S_vec[t]    <- interp_SI_to_stor(SI_new)
    elev_vec[t] <- interp_stor_to_elev(S_vec[t])

    if (elev_vec[t] > peak_stage) peak_stage <- elev_vec[t]
    if (O_vec[t]    > peak_dis)   peak_dis   <- O_vec[t]
  }

  # RETURN RESULTS =============================================================
  if (full_results) {
    return(data.frame(
      time_hr      = time_vec,
      inflow_cfs   = I_vec,
      elevation_ft = elev_vec,
      storage_acft = S_vec / SQFT_PER_ACRE,
      outflow_cfs  = O_vec
    ))
  } else {
    return(c(
      peak_stage_ft      = peak_stage,
      peak_discharge_cfs = peak_dis
    ))
  }
}
