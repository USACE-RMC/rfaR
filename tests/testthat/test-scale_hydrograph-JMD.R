# SETUP ========================================================================
# Use hydrographs from JMD example & hydrograph set up
hydrographs <- hydrograph_setup(jmd_hydro_apr1999,
                                jmd_hydro_jun1921,
                                jmd_hydro_jun1965,
                                jmd_hydro_jun1965_15min,
                                jmd_hydro_may1955,
                                jmd_hydro_pmf,
                                jmd_hydro_sdf,
                                critical_duration = 2,
                                routing_days = 10)

hydro_1hr   <- hydrographs[[1]]   # jmd_hydro_apr1999 - 1-hour
hydro_15min <- hydrographs[[4]]   # jmd_hydro_jun1965_15min - 15-min

# Observed Volumes
obs_vol    <- attr(hydro_1hr,   "obs_vol")
obs_vol_15 <- attr(hydro_15min, "obs_vol")

# Scaled Test Volumes
sampled_vol_2x <- obs_vol * 2
sampled_vol_15_2x <- obs_vol_15 * 2

# ==============================================================================
# Straight Forward 1-hour input, 1-hour routing timestep
# ==============================================================================

test_that("scale_hydrograph returns a data frame with correct column names", {
  scaled <- scale_hydrograph(
    hydrograph_shape = hydro_1hr[, c("hour", "inflow")],
    observed_volume  = obs_vol,
    sampled_volume   = sampled_vol_2x,
    routing_dt       = 1
  )
  expect_s3_class(scaled, "data.frame")
  expect_named(scaled, c("time_hrs", "inflow_cfs"))
})

test_that("scale_hydrograph time vector starts at 0 for 1-hour input", {
  scaled <- scale_hydrograph(
    hydrograph_shape = hydro_1hr[, c("hour", "inflow")],
    observed_volume  = obs_vol,
    sampled_volume   = sampled_vol_2x,
    routing_dt       = 1
  )
  expect_equal(scaled$time_hrs[1], 0)
})

test_that("scale_hydrograph returns correct number of rows for 1-hour input", {
  scaled <- scale_hydrograph(
    hydrograph_shape = hydro_1hr[, c("hour", "inflow")],
    observed_volume  = obs_vol,
    sampled_volume   = sampled_vol_2x,
    routing_dt       = 1
  )
  expect_equal(nrow(scaled), nrow(hydro_1hr))
})

test_that("scale_hydrograph applies scale factor correctly for 1-hour input", {
  scaled <- scale_hydrograph(
    hydrograph_shape = hydro_1hr[, c("hour", "inflow")],
    observed_volume  = obs_vol,
    sampled_volume   = sampled_vol_2x,
    routing_dt       = 1
  )
  # Scale factor is 2x - every ordinate should be doubled
  expect_equal(scaled$inflow_cfs, hydro_1hr$inflow * 2)
})

# ==============================================================================
# TESTS - 15-minute input hydro, 1-hour routing timestep (block average)
# ==============================================================================

test_that("scale_hydrograph returns correct number of rows for 15-min input", {
  scaled <- scale_hydrograph(
    hydrograph_shape = hydro_15min[, c("hour", "inflow")],
    observed_volume  = obs_vol_15,
    sampled_volume   = sampled_vol_15_2x,
    routing_dt       = 1
  )
  # Block averaging 15-min to 1-hour reduces rows by factor of 4
  # Trim to complete blocks first
  expected_rows <- floor(nrow(hydro_15min) / 4)
  expect_equal(nrow(scaled), expected_rows)
})

test_that("scale_hydrograph returns 1-hour timestep for 15-min input", {
  scaled <- scale_hydrograph(
    hydrograph_shape = hydro_15min[, c("hour", "inflow")],
    observed_volume  = obs_vol_15,
    sampled_volume   = sampled_vol_15_2x,
    routing_dt       = 1
  )
  # Difference between consecutive time steps should be 1 hour
  dt_out <- scaled$time_hrs[2] - scaled$time_hrs[1]
  expect_equal(dt_out, 1)
})

test_that("scale_hydrograph time vector starts at 0 for 15-min input", {
  scaled <- scale_hydrograph(
    hydrograph_shape = hydro_15min[, c("hour", "inflow")],
    observed_volume  = obs_vol_15,
    sampled_volume   = sampled_vol_15_2x,
    routing_dt       = 1
  )
  expect_equal(scaled$time_hrs[1], 0)
})

test_that("scale_hydrograph block average then scale matches manual calculation", {
  scaled <- scale_hydrograph(
    hydrograph_shape = hydro_15min[, c("hour", "inflow")],
    observed_volume  = obs_vol_15,
    sampled_volume   = sampled_vol_15_2x,
    routing_dt       = 1
  )
  # First output ordinate should equal mean of first 4 native ordinates * scale factor
  expected_first <- mean(hydro_15min$inflow[1:4]) * 2
  expect_equal(scaled$inflow_cfs[1], expected_first)
})

# ==============================================================================
# Scale Factor - Edge Cases
# ==============================================================================

test_that("scale_hydrograph with scale factor of 1 returns unchanged ordinates", {
  scaled <- scale_hydrograph(
    hydrograph_shape = hydro_1hr[, c("hour", "inflow")],
    observed_volume  = obs_vol,
    sampled_volume   = obs_vol,   # same volume -> scale factor = 1
    routing_dt       = 1
  )
  expect_equal(scaled$inflow_cfs, hydro_1hr$inflow)
})

test_that("scale_hydrograph produces non-negative flows for positive scale factor", {
  scaled <- scale_hydrograph(
    hydrograph_shape = hydro_1hr[, c("hour", "inflow")],
    observed_volume  = obs_vol,
    sampled_volume   = sampled_vol_2x,
    routing_dt       = 1
  )
  expect_true(all(scaled$inflow_cfs >= 0))
})
