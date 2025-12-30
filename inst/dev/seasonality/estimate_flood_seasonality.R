# Read daily inflow data
daily_por <- jmd_por_inflow

# Set threshold
thresh <- 5000

# Critical Duration
crit_dir <- 3 # this was 3 for some reason, either an error or handjam way for preferred seasonality

# Set days apart
min_days <- 7

# Max events per year
max_events <- 5

# Filter into PDS --------------------------------------------------------------
# Date info
daily_por$date <- lubridate::mdy(daily_por$date)
daily_por$year <- lubridate::year(daily_por$date)
daily_por$water_year <- ifelse(lubridate::month(daily_por$date) >9,lubridate::year(daily_por$date)+1,lubridate::year(daily_por$date))
daily_por$month <- lubridate::month(daily_por$date)
daily_por$day <- lubridate::day(daily_por$date)

# Critical Duration above the threshold
daily_por$Kday_flow <- zoo::rollmean(daily_por$flow_cfs, k = crit_dir, fill = NA, align = "right")

# Create Event "clusters"
threshold_events <- daily_por

# Filter to threshold
threshold_events$above_thresh <- ifelse(threshold_events$Kday_flow > thresh,1,0)
threshold_events <- threshold_events[!is.na(threshold_events$Kday_flow),]
threshold_events <- threshold_events[threshold_events$above_thresh >= 1,]

# day gaps - First row is always a new event
threshold_events$days_gap <- c(Inf,diff(threshold_events$date))
threshold_events$new_event <- threshold_events$days_gap >= min_days
threshold_events$event_id <- cumsum(threshold_events$new_event)

# Filter max from each event
independent_events <- dplyr::ungroup(
  dplyr::slice_max(
    dplyr::group_by(threshold_events, event_id),
    Kday_flow, n = 1, with_ties = FALSE
  )
)

# Max Events per water year
events_per_year <- dplyr::ungroup(
  dplyr::slice_max(
    dplyr::group_by(independent_events, water_year),
    Kday_flow, n = max_events, with_ties = FALSE
  )
)

# Relative Frequency
monthly_counts <- dplyr::count(events_per_year, month)
monthly_complete <- tidyr::complete(monthly_counts, month = 1:12, fill = list(n = 0))

monthly_freq <- data.frame(dplyr::mutate(
  monthly_complete,
  rel_freq = n / sum(n),
  month_name = month.abb[month]))

# Cumulative Frequency
monthly_freq$cume_freq <- cumsum(monthly_freq$rel_freq)

jmd_seasonality
