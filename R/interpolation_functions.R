#' Interpolate AEPs at Target Stages Across Realizations
#'
#' For each realization's stage-frequency curve, interpolates the AEP at
#' every target stage value. Interpolation is performed in standard normal
#' (z-variate) space.
#'
#' @param results_list List of realization data frames, each with columns
#'   \code{stage} and \code{AEP}.
#' @param target_stages Numeric vector of stage values to interpolate at.
#'
#' @return Matrix of AEP values with rows = target stages and columns = realizations.
#'
#' @keywords internal
interpolate_aep_matrix <- function(results_list, target_stages) {
  Nrealizations <- length(results_list)
  aep_matrix <- matrix(NA, nrow = length(target_stages), ncol = Nrealizations)

  for (j in 1:Nrealizations) {
    z_aep <- qnorm(1 - results_list[[j]]$AEP)
    aep_matrix[, j] <- 1 - pnorm(approx(x = results_list[[j]]$stage,
                                        y = z_aep,
                                        xout = target_stages,
                                        rule = 2)$y)
  }

  aep_matrix
}

#' Interpolate Stages at Target AEPs Across Realizations
#'
#' For each realization's stage-frequency curve, interpolates the stage at
#' every target AEP value. Interpolation is performed in standard normal
#' (z-variate) space.
#'
#' @param results_list List of realization data frames, each with columns
#'   \code{stage} and \code{AEP}.
#' @param target_aeps Numeric vector of AEP values to interpolate at.
#'
#' @return Matrix of stage values with rows = target AEPs and columns = realizations.
#'
#' @keywords internal
interpolate_stage_matrix <- function(results_list, target_aeps) {
  Nrealizations <- length(results_list)
  stage_matrix <- matrix(NA, nrow = length(target_aeps), ncol = Nrealizations)
  for (j in 1:Nrealizations) {
    valid <- results_list[[j]]$AEP > 0 & results_list[[j]]$AEP < 1
    z_aep <- qnorm(1 - results_list[[j]]$AEP[valid])
    stage_matrix[, j] <- approx(x = z_aep,
                                y = results_list[[j]]$stage[valid],
                                xout = qnorm(1 - target_aeps),
                                rule = 2)$y
  }
  stage_matrix
}

#' Interpolate Stage from AEP
#'
#' Given a stage-frequency curve, interpolates stage values at target AEPs.
#' Interpolation is performed in standard normal (z-variate) space for
#' better behavior across orders of magnitude in AEP.
#'
#' @param aep Numeric vector of annual exceedance probabilities from the
#'   known curve.
#' @param stage Numeric vector of stage values corresponding to \code{aep}.
#' @param interp_aep Numeric vector of target AEPs to interpolate at.
#'
#' @return Numeric vector of interpolated stage values (same length as
#'   \code{interp_aep}).
#'
#' @export
#'
#' @seealso \code{\link{stage2aep}} for the inverse operation.
#'
#' @examples
#' # Get stage at the 1.12% and 0.175% AEP
#' aep2stage(jmd_rfa_expected$AEP, jmd_rfa_expected$Expected, c(0.0112,1.75E-3))
aep2stage <- function(aep, stage, interp_aep) {
  z_aep <- qnorm(1 - aep)
  interp_stage <- approx(x = z_aep, y = stage,
                         xout = qnorm(1 - interp_aep),
                         rule = 2)$y
  interp_stage
}

#' Interpolate AEP from Stage
#'
#' Given a stage-frequency curve, interpolates AEP values at target stages.
#' Interpolation is performed in standard normal (z-variate) space for
#' better behavior across orders of magnitude in AEP.
#'
#' @details
#' Stage-frequency curves typically flatten at very rare AEPs once
#' the reservoir reaches the reservoir model discharge capacity.
#' This produces tied stage values.
#' \code{approx()} handles these by averaging the
#' corresponding AEPs, which may emit a "collapsing to unique 'x'
#' values" warning. This is expected behavior.
#'
#' @param aep Numeric vector of annual exceedance probabilities from the
#'   known curve.
#' @param stage Numeric vector of stage values corresponding to \code{aep}.
#' @param interp_stage Numeric vector of target stage values to interpolate at.
#'
#' @return Numeric vector of interpolated AEP values (same length as
#'   \code{interp_stage}).
#'
#' @export
#'
#' @seealso \code{\link{aep2stage}} for the inverse operation.
#'
#' @examples
#' # Get AEP at stage 3862.0-ft and 3872-ft
#' stage2aep(jmd_rfa_expected$AEP, jmd_rfa_expected$Expected, c(3862.0, 3872))
stage2aep <- function(aep, stage, interp_stage) {
  z_aep <- qnorm(1 - aep)
  z_interp <- approx(x = stage, y = z_aep,
                     xout = interp_stage,
                     rule = 2)$y
  1 - pnorm(z_interp)
}
