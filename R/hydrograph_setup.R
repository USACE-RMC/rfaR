#' Hydrograph Setup for RFA Simulation
#'
#' Prepares hydrograph data frames copied from RMC-RFA for use in RFA simulation.
#' Converts date/time columns and adds sequential hour and hydrograph ID columns.
#'
#' @param ... Data frame representing an input hydrograph with columns: Ord, Date, Time, Flow (cfs).
#'   Copied directly from RMC-RFA.
#'
#' @return A list of formatted hydrograph data frames, each containing:
#' \describe{
#'   \item{dt}{Date-time (POSIXct)}
#'   \item{hour}{Hours from start of event}
#'   \item{inflow}{Inflow (cfs)}
#'   \item{hydrograph_num}{Hydrograph ID number for sampling}
#' }
#'
#' @export
#'
#' @examples
#' # Setup multiple hydrographs for RFA
#' # hydros <- hydrograph_setup(hydro_apr1999, hydro_jun1965, hydro_pmf)
hydrograph_setup <- function(...){

  # Collect Hydrographs
  input_hydrographs <- list(...)
  hydrograph_ID <- seq(1,length(input_hydrographs),1)

  # Create hydrograph from RFA input
  export_list <- list()
  for(i in 1:length(input_hydrographs)) {
    df <- data.frame(dt = lubridate::mdy_hm(paste0(input_hydrographs[[i]][,2]," ",input_hydrographs[[i]][,3])),
                     hour = NA,
                     inflow = input_hydrographs[[i]][,4],
                     hydrograph_num = hydrograph_ID[i])
    # Calculate Timestep
    timestep <- as.numeric(difftime(df[2,1],df[1,1], units = "hours"))

    # Add to df as hour
    df$hour <- seq(0,nrow(df)-1,timestep)

    # Ensure inflow is numeric (remove 1k commas)
    if(!is.numeric(df$inflow)){
      df$inflow <- as.numeric(gsub(",", "", df$inflow))
    }

    # Add to list
    export_list[[i]] <- df
  }

  return(export_list)

}
