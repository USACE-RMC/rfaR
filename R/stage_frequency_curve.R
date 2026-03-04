#' Compute Stage-Frequency Curve
#'
#' Converts a matrix of routed peak stages into a stage-frequency curve using
#' the law of total probability across stratified bins. For each candidate stage,
#' the conditional exceedance probability within each bin is weighted by the bin
#' probability and summed to produce the unconditional annual exceedance
#' probability (AEP).
#'
#' @param peakStage Matrix of peak stages `[Mevents x Nbins]` from routing.
#' @param weights Numeric vector of bin weights from [stratified_sampler()].
#'   Must have length equal to `ncol(peakStage)`.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{stage}{Evaluated stage values}
#'   \item{AEP}{Annual exceedance probability at each stage}
#' }
#'
#' @seealso [stratified_sampler()], [flow_frequency_sampler()], [rfa_simulate()]
#'
#' @keywords internal
stage_frequency_curve <- function(peakStage, weights, stage_bins = 1000) {
  min_stage <- min(peakStage)
  max_stage <- max(peakStage)
  # nrow of peak stage represents mevents
  #n <- nrow(peakStage)-1
  #n <- nrow(peakStage)
  n <-stage_bins
  delta <- (max_stage - min_stage) / (n)

  # Stage Vect
  stage_vect <- seq(min_stage, by = delta, length.out = n)

  aep <- numeric(n)
  for (i in 1:n) {
    exceed_prop <- colMeans(peakStage > stage_vect[i])
    aep[i] <- sum(exceed_prop * weights)
  }

  data.frame(stage = stage_vect, AEP = aep)
}
