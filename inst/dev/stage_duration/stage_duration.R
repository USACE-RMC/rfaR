#' Calculate Stage Duration Curve
#'
#' @param date Vector of dates (Date class or character in a parseable format)
#' @param stage Vector of stage values (numeric)
#' @param by Grouping option: "all" (single curve), "year", or "month"
#' @param probs Exceedance probabilities to calculate (default 0-100% by 1%)
#'
#' @return A data.frame with columns:
#'   - exceedance_pct: Percent of time stage is exceeded
#'   - stage: Stage value at that exceedance level
#'   - year: (if by = "year" or "month")
#'   - month: (if by = "month")
#'
stage_duration_curve <- function(date, stage, by = "all", probs = seq(0, 100, by = 1)) {

  # Input validation
  if (length(date) != length(stage)) {
    stop("date and stage must be the same length")
  }

  by <- match.arg(by, choices = c("all", "year", "month"))

  # Convert dates if needed
  if (!inherits(date, "Date")) {
    date <- as.Date(date)
  }

  # Build data frame
  df <- data.frame(
    date = date,
    stage = stage,
    year = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m"))
  )

  # Remove NA stages
  df <- df[!is.na(df$stage), ]

  if (nrow(df) == 0) {
    stop("No non-NA stage values found")
  }

  # Helper function to calculate exceedance curve for a vector of stages
  calc_exceedance <- function(stages) {
    exceed_probs <- probs / 100
    quantile_probs <- 1 - exceed_probs
    quantile(stages, probs = quantile_probs, type = 7, names = FALSE)
  }

  # Calculate based on grouping
  if (by == "all") {

    result <- data.frame(
      exceedance_pct = probs,
      stage = calc_exceedance(df$stage)
    )

  } else if (by == "year") {

    unique_years <- sort(unique(df$year))
    results <- vector("list", length(unique_years))

    for (i in seq_along(unique_years)) {
      yr <- unique_years[i]
      stages_yr <- df$stage[df$year == yr]

      results[[i]] <- data.frame(
        year = yr,
        exceedance_pct = probs,
        stage = calc_exceedance(stages_yr)
      )
    }

    result <- do.call(rbind, results)

  } else if (by == "month") {

    # Get unique year-month combinations
    df$year_month <- paste(df$year, df$month, sep = "-")
    unique_ym <- unique(df[, c("year", "month")])
    unique_ym <- unique_ym[order(unique_ym$year, unique_ym$month), ]

    results <- vector("list", nrow(unique_ym))

    for (i in seq_len(nrow(unique_ym))) {
      yr <- unique_ym$year[i]
      mo <- unique_ym$month[i]
      stages_mo <- df$stage[df$year == yr & df$month == mo]

      results[[i]] <- data.frame(
        year = yr,
        month = mo,
        month_name = month.abb[mo],
        exceedance_pct = probs,
        stage = calc_exceedance(stages_mo)
      )
    }

    result <- do.call(rbind, results)
  }

  rownames(result) <- NULL
  return(result)
}
