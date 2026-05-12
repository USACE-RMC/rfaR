#' Power Function
#'
#' Raises 10 to the 1st through the nth power
#' @param x Highest power of 10 to use
#' @return Vector of 10 raised sequentially from the 1st to the nth power.
#' @keywords internal
power_function <- function(x) {
   10^(1:x)
}

#' Thin samples
#' Thin a large sample of sorted z-scores and stages for efficiency.
#' Retains 5,000 evenly-spaced points across the full range plus the last
#' 10,000 points for dense coverage at the extreme tail.
#'
#' @param n            Total number of samples
#' @param z_sorted     Sorted z-scores (ascending), length n
#' @param stage_sorted Sorted stage values (ascending), length n
#' @return A tibble with columns z_aep and stage
thin_samples <- function(n, z_sorted, stage_sorted) {

  thin_idx <- unique(c(
    round(seq(1, n, length.out = 5000)),
    (n - 10000):n
  ))
  thin_idx <- sort(unique(thin_idx))
  thin_idx <- thin_idx[thin_idx >= 1 & thin_idx <= n]

  tibble(
    z_aep = z_sorted[thin_idx],
    stage = stage_sorted[thin_idx]
  )
}
