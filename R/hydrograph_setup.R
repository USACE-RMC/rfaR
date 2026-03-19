#' Hydrograph Setup for RFA Simulation
#'
#' Prepares hydrograph data frames copied from RMC-RFA for use in RFA simulation.
#' Converts date/time columns and adds sequential hour and hydrograph ID columns.
#'
#' @param ... Data frame representing an input hydrograph with columns: Ord, Date, Time, Flow (cfs).
#'   Copied directly from RMC-RFA.
#' @param critical_duration Critical duration in days.
#' @param routing_days Desired length of routing simulation in days.
#' @param weights Optional numeric vector of sampling weights for each hydrograph.
#'   Must be the same length as the number of input hydrographs. Weights are
#'   normalized to probabilities internally. If NULL (default), all hydrographs
#'   are weighted equally.
#'
#' @return A list of formatted hydrograph data frames, each containing:
#' \describe{
#'   \item{datetime}{Date-time (POSIXct)}
#'   \item{hour}{Hours from start of event}
#'   \item{inflow}{Inflow (cfs)}
#'   \item{hydrograph_num}{Hydrograph ID number for sampling}
#'   \item{obs_vol}{Max n-day inflow volume (stored as an attribute of each dataframe)}
#'   \item{dt}{Hydrograph timestep (delta time, dt) in hours (stored as an attribute of each dataframe)}
#' }
#' The returned list also has a \code{probs} attribute containing the normalized
#' sampling probabilities derived from \code{weights}.
#'
#' @export
#'
#' @examples
#' # Setup with equal weights (default)
#' hydros <- hydrograph_setup(jmd_hydro_apr1999,
#'                            jmd_hydro_jun1965,
#'                            jmd_hydro_pmf,
#'                            critical_duration = 2,
#'                            routing_days = 10)
#'
#' # Setup with custom weights (PMF 3x more likely to be sampled)
#' hydros <- hydrograph_setup(jmd_hydro_apr1999,
#'                            jmd_hydro_jun1965,
#'                            jmd_hydro_pmf,
#'                            critical_duration = 2,
#'                            routing_days = 10,
#'                            weights = c(1, 1, 3))
#'
#' # view normalized probabilities
#' attr(hydros, "probs")
hydrograph_setup <- function(..., critical_duration = NULL, routing_days = NULL, weights = NULL){

  # Collect Hydrographs ========================================================
  input_hydrographs <- list(...)
  hydrograph_ID <- seq(1,length(input_hydrographs),1)
  hydrograph_names <- sapply(substitute(list(...))[-1], deparse)

  # Set Sample Weights =========================================================
  if(!is.null(weights) && length(weights) != length(hydrograph_ID)){
    cli::cli_abort("Length of {.arg weights} must match number of input hydrographs.")
  }

  if(is.null(weights)){
    # Defaults is 1
    weights <- rep(1,length = length(hydrograph_ID))
  }

  sample_prob <- weights/sum(weights)

  # Detect timesteps ===========================================================
  timesteps <- sapply(input_hydrographs, function(h) {
    as.numeric(difftime(
      lubridate::mdy_hm(paste0(h[2, 2], " ", h[2, 3])),
      lubridate::mdy_hm(paste0(h[1, 2], " ", h[1, 3])),
      units = "hours"))
  })

  # Create hydrograph from RFA input ===========================================
  export_list <- list()
  for(i in 1:length(input_hydrographs)) {
    df <- data.frame(datetime = lubridate::mdy_hm(paste0(input_hydrographs[[i]][,2]," ",input_hydrographs[[i]][,3])),
                     hour = NA,
                     inflow = input_hydrographs[[i]][,4],
                     hydrograph_num = hydrograph_ID[i])

    # Add to df as hour ++++
    df$hour <- seq(0, (nrow(df)-1) * timesteps[i], by = timesteps[i])

    # Ensure inflow is numeric (remove 1k commas) ++++
    if(!is.numeric(df$inflow)){
      df$inflow <- as.numeric(gsub(",", "", df$inflow))
    }

    # Max n-day inflow (critical duration, in hours) ++++
    critdur_hrs <- critical_duration*24
    k_steps <- critdur_hrs / timesteps[i]
    max_hydrograph_vol <- max(zoo::rollmeanr(df$inflow, k = k_steps))

    # Has a routing duration been specified ++++
    if(!is.null(routing_days)){
      routing_days_hrs <- routing_days*24

      # If so - extend the hydrograph shape +++
      if(routing_days_hrs <= max(df$hour)){
        full_hydrograph <- df
      } else {
        hour_diff <- routing_days_hrs - max(df$hour)
        ts_minutes <- timesteps[i] * 60
        extended_df <- data.frame(
          datetime = seq(df$datetime[nrow(df)] + lubridate::minutes(ts_minutes),
                         df$datetime[nrow(df)] + lubridate::minutes(hour_diff * 60),
                         by = paste0(ts_minutes, " min")),
          hour = seq(max(df$hour) + timesteps[i], max(df$hour) + hour_diff, by = timesteps[i]),
          inflow = 0,
          hydrograph_num = hydrograph_ID[i])
        full_hydrograph <- rbind(df,extended_df)
      }
    # If there was no routing time, just leave it ++++
    } else {
      full_hydrograph <- df
    }

    # Set an attribute of the observed max n-day inflow volume =================
    attr(full_hydrograph,"obs_vol") <- max_hydrograph_vol

    # Set an attribute of delta time (dt) ======================================
    attr(full_hydrograph, "dt") <- timesteps[i]

    # Save name of the input hydrograph ========================================
    attr(full_hydrograph, "name") <- hydrograph_names[i]

    # Add to list
    export_list[[i]] <- full_hydrograph
  }
  # Set an attribute of the weights ============================================
  attr(export_list, "probs") <- sample_prob

  return(export_list)
}
