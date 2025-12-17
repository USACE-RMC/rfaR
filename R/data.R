#' Cherry Creek Reservoir Model
#'
#' Elevation-storage-discharge relationship for Cherry Creek Reservoir. Reservoir models must be monotonic.
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{elev_ft}{Reservoir stage/elevation data (FT-NAVD88)}
#'   \item{stor_acft}{Cumulative reservoir storage (ACRE-FT) corresponding to reservoir elevation (elev_ft).}
#'   \item{outflow_cfs}{Cumulative discharge/outflow (CFS) corresponding to reservoir elevation (elev_ft).}
#' }
"cc_resmodel"

#' Cherry Creek Inflow Hydrograph
#'
#' Inflow hydrograph for an example flood event at Cherry Creek Dam.
#'
#' @format A data frame with 2 columns:
#' \describe{
#'   \item{time_hr}{Time in hours (integer)}
#'   \item{inflow_cfs}{Inflow in cubic-feet per second (CFS)}
#' }
"cc_inflowhydro"

#' Cherry Creek Initial Elevation
#'
#' Initial/starting elevation (FT-NAVD88) for Cherry Creek Dam reservoir routing.
#' This elevation was used in HEC-HMS.
#'
#' @format A single numeric value representing the initial reservoir elevation (FT-NAVD88).
"cc_init_elev"

#' Cherry Creek HEC-HMS Routing Results
#'
#' Routing results from HEC-HMS using the same reservoir model/geometry, inflow hydrograph, and initial reservoir elevation.
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{time_hr}{Time in hours (integer)}
#'   \item{inflow_cfs}{Inflow in cubic-feet per second (CFS)}
#'   \item{elev_ft}{Routed reservoir stage/elevation data in FT-NAVD88}
#'   \item{stor_acft}{Routed reservoir storage (ACRE-FT) corresponding to reservoir elevation (elev_ft).}
#'   \item{outflow_cfs}{Routed discharge/outflow (CFS) corresponding to reservoir elevation (elev_ft).}
#' }
"cc_hms_results"
