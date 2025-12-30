#' Create Stratified Bins in EVI (Gumbel) Space
#'
#' @param min_aep Minimum annual exceedance probability (e.g., 1e-8)
#' @param max_aep Maximum annual exceedance probability (e.g., 0.99)
#' @param n_bins Number of bins (e.g., 50)
#' @return Vector of bin boundaries (length n_bins + 1)
create_bins = function(min_aep, max_aep, n_bins) {
  # Convert AEP to EVI reduced variate (y)
  y_min = -log(-log(1 - max_aep))  # ~-2.25 for AEP=0.99
  y_max = -log(-log(1 - min_aep))  # ~18.4 for AEP=1e-8

  # Create equal-width bins in EVI space
  y_boundaries = seq(y_min, y_max, length.out = n_bins + 1)

  # Convert back to AEP
  aep_boundaries = 1 - exp(-exp(-y_boundaries))

  # Return in ASCENDING order so runif() works correctly
  return(sort(aep_boundaries))
}
