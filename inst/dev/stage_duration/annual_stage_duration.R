#' Calculate Annual Stage Duration Curve
#'
#' @param date Vector of dates (Date class or character in a parseable format)
#' @param stage Vector of stage values (numeric)
#' @param year Optional specific year to analyze. If NULL, uses all data.
#' @param probs Exceedance probabilities to calculate (default 0-100% by 1%)
#'
#' @return A data.frame with columns:
#'   - exceedance_pct: Percent of time stage is exceeded
#'   - stage: Stage value at that exceedance level
#'
#' @examples
#' sdc <- stage_duration_curve(daily_data$date, daily_data$stage)
#' plot(sdc$exceedance_pct, sdc$stage, type = "l")
#'
stage_duration_curve <- function(date, stage, year = NULL, probs = seq(0, 100, by = 1)) {


  # Input validation
  if (length(date) != length(stage)) {
    stop("date and stage must be the same length")
  }

  # Convert dates if needed
  if (!inherits(date, "Date")) {
    date <- as.Date(date)
  }

  # Filter to specific year if requested
  if (!is.null(year)) {
    year_idx <- as.integer(format(date, "%Y")) == year
    stage <- stage[year_idx]

    if (length(stage) == 0) {
      stop(paste("No data found for year", year))
    }
  }

  # Remove NA values
  stage <- stage[!is.na(stage)]

  if (length(stage) == 0) {
    stop("No non-NA stage values found")
  }

  # Calculate exceedance quantiles
  # For exceedance: P(Stage > x) = p means we want the (1-p) quantile
  exceed_probs <- probs / 100
  quantile_probs <- 1 - exceed_probs

  stages_at_exceedance <- quantile(stage, probs = quantile_probs, type = 7, names = FALSE)

  # Build output
  result <- data.frame(
    exceedance_pct = probs,
    stage = stages_at_exceedance
  )

  return(result)
}
