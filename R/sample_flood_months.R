#' Sample Flood Event Months
#'
#' Generates random samples of flood event months based on seasonality
#' probabilities from historical flood frequency analysis.
#'
#' @param n_samples Number of desired samples for Monte Carlo simulation.
#' @param monthly_probs Data frame with at least 3 columns: month name (column 1),
#'   any value (column 2), and relative frequency/probability (column 3).
#'   Must be in that order.
#' @param month_number Logical. If `TRUE` (default), returns numeric month values
#'   (1-12). If `FALSE`, returns month names.
#' @param seed Optional seed for reproducibility. If `NULL` (default), no seed is set.
#'
#' @return A vector of length `n_samples` containing either numeric month values
#'   (1-12) or month names, depending on `month_number`.
#'
#' @export
#'
#' @examples
#' # Sample 100 flood months based on JMD seasonality
#' sample_flood_months(100, jmd_seasonality)
#'
#' # Return month names instead of numbers
#' sample_flood_months(100, jmd_seasonality, month_number = FALSE)

sample_flood_months <- function(n_samples, monthly_probs, month_number = TRUE, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  # ============================================================================
  # CONVERT TO DF (TIBBLE HANDLING)
  # ============================================================================
  monthly_probs <- as.data.frame(monthly_probs)

  month_names <- monthly_probs[,1]
  month_probs <- monthly_probs[,3]

  # Sample months based on their probabilities
  sampled_month_names <- sample(month_names, size = n_samples, replace = TRUE, prob = month_probs)

  # If full routing results desired
  if (month_number) {
    # Return Month numbers
    sampled_month_nums <- match(sampled_month_names, month.name)
    return(sampled_month_nums)

  } else {
    # Return Month names
    return(sampled_month_names)
  }
}
