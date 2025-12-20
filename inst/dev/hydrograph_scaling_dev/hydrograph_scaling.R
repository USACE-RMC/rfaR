#' Hydrograph Scaling
#'
#' Scales inflow hydrograph given a sampled inflow volume from the volume-frequency
#' curve. Scale factor is defined by the sampled inflow volume and the maximum
#' volume of the corresponding duration.
#'
#' @param hydrograph_shape Data frame with two columns: time (hours) and
#'   inflow (cfs). Must be in that order.
#' @param sampled_volume Sampled inflow volume from volume-frequency curve.
#' @param critical_duration Critical duration in days.
#' @param routing_days Desired length of routing simulation in days.
#'
#' @return A data frame with two columns: `time_hrs` and `inflow_cfs`.
#'
#' @export
#'
#' @examples
#' # Scale example hydrograph
#' scaled <- scale_hydrograph(cc_inflowhydro, sampled_volume = 5000,
#'                            critical_duration = 2, routing_days = 10)

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
