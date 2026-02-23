#' Scale Hydrograph
#'
#' Scales inflow hydrograph given a sampled inflow volume from the volume-frequency
#' curve. Scale factor is defined by the sampled inflow volume and the maximum
#' volume of the corresponding duration. If the native hydrograph timestep differs
#' from the target routing timestep, resampling is applied prior to scaling:
#' finer-to-coarser uses block averaging; coarser-to-finer uses linear interpolation.
#'
#' @param hydrograph_shape Data frame with two columns: time (hours) and
#'   inflow (cfs). Must be in that order.
#' @param observed_volume Maximum n-day inflow volume from input hydrograph shape.
#' @param sampled_volume Sampled inflow volume from volume-frequency curve.
#' @param routing_dt Target routing timestep in hours. Supported values are
#'   \code{0.25} (15-min), \code{1} (1-hour, default), \code{6} (6-hour),
#'   and \code{24} (24-hour).
#'
#' @return A data frame with two columns: \code{time_hrs} and \code{inflow_cfs},
#'   at the target routing timestep, with time starting at hour 0.
#'
#' @export
#'
#' @examples
#' # Default 1-hour routing timestep
#' scaled <- scale_hydrograph(cc_inflowhydro, observed_volume = 2500, sampled_volume = 5000)
#'
#' # 15-min input hydrograph, 1-hour routing timestep
#' scaled <- scale_hydrograph(cc_inflowhydro_15min, observed_volume = 2500,
#'                            sampled_volume = 5000, routing_dt = 1)
#'
#' # 6-hour input hydrograph, 1-hour routing timestep
#' scaled <- scale_hydrograph(cc_inflowhydro_6hr, observed_volume = 2500,
#'                            sampled_volume = 5000, routing_dt = 1)
scale_hydrograph <- function(hydrograph_shape, observed_volume, sampled_volume, routing_dt = 1) {

  # CONVERT TO DF (TIBBLE HANDLING) ============================================
  hydrograph_shape <- as.data.frame(hydrograph_shape)

  # ENSURE NUMERIC DATA (NOT INTEGER) ==========================================
  hydrograph_shape[] <- lapply(hydrograph_shape, as.numeric)

  # INFER NATIVE TIMESTEP FROM TIME COLUMN =====================================
  dt_native <- hydrograph_shape[2, 1] - hydrograph_shape[1, 1]

  inflow <- hydrograph_shape[, 2]

  # RESAMPLE TO ROUTING TIMESTEP ===============================================

  if (dt_native == routing_dt) {
    # PASS-THROUGH: no resampling needed
    resampled_inflow <- inflow
    n_out <- length(resampled_inflow)

  } else if (dt_native < routing_dt) {
    # BLOCK AVERAGE: native is finer than routing (e.g., 15-min -> 1-hr)
    block_size <- round(routing_dt / dt_native)

    # Trim to a complete number of blocks
    n_complete <- floor(length(inflow) / block_size) * block_size
    trimmed    <- inflow[1:n_complete]

    # Each column of the matrix is one block; colMeans gives block averages
    block_matrix     <- matrix(trimmed, nrow = block_size)
    resampled_inflow <- colMeans(block_matrix)
    n_out            <- length(resampled_inflow)

  } else {
    # LINEAR INTERPOLATION: native is coarser than routing (e.g., 6-hr -> 1-hr)
    n_coarse    <- length(inflow)
    time_coarse <- seq(0, by = dt_native, length.out = n_coarse)
    time_fine   <- seq(0, max(time_coarse), by = routing_dt)

    resampled_inflow <- approx(
      x      = time_coarse,
      y      = inflow,
      xout   = time_fine,
      method = "linear",
      rule   = 2
    )$y
    n_out <- length(resampled_inflow)
  }

  # SCALE FACTOR ===============================================================
  scale_factor <- sampled_volume / observed_volume

  # SCALE RESAMPLED HYDROGRAPH =================================================
  scaled_inflow <- resampled_inflow * scale_factor

  # RETURN SCALED HYDROGRAPH ===================================================
  return(data.frame(
    time_hrs   = seq(0, by = routing_dt, length.out = n_out),
    inflow_cfs = scaled_inflow
  ))
}
