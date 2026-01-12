#' Hydrograph Setup for RFA Simulation
#'
#' Prepares hydrograph data frames copied from RMC-RFA for use in RFA simulation.
#' Converts date/time columns and adds sequential hour and hydrograph ID columns.
#'
#' @param ... Data frame representing an input hydrograph with columns: Ord, Date, Time, Flow (cfs).
#'   Copied directly from RMC-RFA.
#' @param critical_duration Critical duration in days.
#' @param routing_days Desired length of routing simulation in days.
#'
#' @return A list of formatted hydrograph data frames, each containing:
#' \describe{
#'   \item{dt}{Date-time (POSIXct)}
#'   \item{hour}{Hours from start of event}
#'   \item{inflow}{Inflow (cfs)}
#'   \item{hydrograph_num}{Hydrograph ID number for sampling}
#'   \item{obs_vol}{Max n-day inflow volume (stored as an attribute of each dataframe)}
#' }
#'
#' @export
#'
#' @examples
#' # Setup multiple hydrographs for RFA
#' # hydros <- hydrograph_setup(hydro_apr1999, hydro_jun1965, hydro_pmf)
hydrograph_setup <- function(..., critical_duration = NULL, routing_days = NULL){

  # Collect Hydrographs ========================================================
  input_hydrographs <- list(...)
  hydrograph_ID <- seq(1,length(input_hydrographs),1)

  # Create hydrograph from RFA input ===========================================
  export_list <- list()
  for(i in 1:length(input_hydrographs)) {
    df <- data.frame(dt = lubridate::mdy_hm(paste0(input_hydrographs[[i]][,2]," ",input_hydrographs[[i]][,3])),
                     hour = NA,
                     inflow = input_hydrographs[[i]][,4],
                     hydrograph_num = hydrograph_ID[i])

    # Calculate Timestep ++++
    timestep <- as.numeric(difftime(df[2,1],df[1,1], units = "hours"))

    # Add to df as hour ++++
    df$hour <- seq(0,nrow(df)-1,timestep)

    # Ensure inflow is numeric (remove 1k commas) ++++
    if(!is.numeric(df$inflow)){
      df$inflow <- as.numeric(gsub(",", "", df$inflow))
    }

    # Max n-day inflow (critical duration, in hours) ++++
    critdur_hrs <- critical_duration*24
    max_hydrograph_vol <- max(zoo::rollmeanr(df$inflow, k = critdur_hrs))

    # Has a routing duration been speficied ++++
    if(!is.null(routing_days)){
      routing_days_hrs <- routing_days*24

      # If so - extend the hydrograph shape +++
      if(routing_days_hrs <= max(df$hour)){
        full_hydrograph <- df
      } else {
        hour_diff <- routing_days_hrs - max(df$hour)
        extended_df <- data.frame(dt = seq(df$dt[nrow(df)] + lubridate::hours(timestep),df$dt[nrow(df)] + lubridate::hours(hour_diff),by = "hour"),
                                  hour = seq(max(df$hour) + timestep, max(df$hour) + hour_diff, by = timestep),
                                  inflow = rep(0,hour_diff),
                                  hydrograph_num = 1)
        full_hydrograph <- rbind(df,extended_df)
      }
    # If there was no routing time, just leave it ++++
    } else {
      full_hydrograph <- df
    }

    # Set an attribute to the observed max n-day inflow volume =================
    attr(full_hydrograph,"obs_vol") <- max_hydrograph_vol

    # Add to list ==============================================================
    export_list[[i]] <- full_hydrograph
  }
  return(export_list)
}
