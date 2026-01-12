#' Scale Hydrograph
#'
#' Scales inflow hydrograph given a sampled inflow volume from the volume-frequency
#' curve. Scale factor is defined by the sampled inflow volume and the maximum
#' volume of the corresponding duration.
#'
#' @param hydrograph_shape Data frame with two columns: time (hours) and
#'   inflow (cfs). Must be in that order.
#' @param observed_volume Maximum n-day inflow volume from input hydrograph shape.
#' @param sampled_volume Sampled inflow volume from volume-frequency curve.
#'
#' @return A data frame with two columns: `time_hrs` and `inflow_cfs`.
#'
#' @export
#'
#' @examples
#' # Scale example hydrograph
#' scaled <- scale_hydrograph(cc_inflowhydro, observed_vol = 2500, sampled_volume = 5000,
#'                            critical_duration = 2, routing_days = 10)

scale_hydrograph <- function(hydrograph_shape, observed_volume, sampled_volume){

  # CONVERT TO DF (TIBBLE HANDLING) ============================================
  hydrograph_shape <- as.data.frame(hydrograph_shape)

  # ENUSRE NUMERIC DATA (NOT INTERGER) =========================================
  hydrograph_shape[] <- lapply(hydrograph_shape, as.numeric)

  # CONSTANTS ==================================================================
  # Hard-coding for hourly routing
  routing_timestep_hrs <- 1

  # Add future functionality for dt (for non-hourly inflow timeseries)
  timestep_days <- 1/24

  # SCALE FACTOR ===============================================================
  scale_factor <- sampled_volume/observed_volume

  # SCALE HYDROGRAPH SHAPE =====================================================
  # Preallocate scaled hydrograph time
  scaled_hydro_time <- seq(1,routing_hrs,routing_timestep_hrs)

  # Scale hydro
  scaled_inflow <- hydrograph_shape[,2]*scale_factor
  scaled_hydrograph <- data.frame(time_hrs = seq(1,length(scaled_inflow),routing_timestep_hrs),
                                  inflow_cfs = scaled_inflow)

  # RETURN SCALED HYDROGRAPH ===================================================
  return(scaled_hydrograph)

}
