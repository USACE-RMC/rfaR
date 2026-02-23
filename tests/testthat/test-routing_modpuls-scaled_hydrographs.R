# Validates mod_puls_routing() against HEC-HMS for the May 1955 hydrograph
# scaled at 1x, 1.5x, 5x, and 12x volume multipliers at John Martin Dam.
#
# Tolerance hierarchy (tightest to loosest):
#   1. Inflow       — must match exactly (same scaled hydrograph)
#   2. Elevation    — tight absolute tolerance
#   3. Storage      — moderate relative tolerance (affected by table offset)
#   4. Outflow      — loosest tolerance (amplified by steep rating curve)

# SETUP ========================================================================

# Build hydrographs
hydrographs <- hydrograph_setup(#jmd_hydro_apr1999,
                                #jmd_hydro_jun1921,
                                #jmd_hydro_jun1965,
                                #jmd_hydro_jun1965_15min,
                                jmd_hydro_may1955,
                                #jmd_hydro_pmf,
                                #jmd_hydro_sdf,
                                critical_duration = 2,
                                routing_days = 10)

hydrograph_shape <- hydrographs[[1]][, 2:3]
obs_vol <- attr(hydrographs[[1]], "obs_vol")

# Scale factors and labels (must match levels in hms_scaled_may1955$scale)
scale_factors <- c(1, 1.5, 5, 12)
scale_labels  <- c("1x", "1.5x", "5x", "12x")

# Route all four conditions
routes <- lapply(scale_factors, function(sf) {
  scaled <- scale_hydrograph(hydrograph_shape, obs_vol, obs_vol * sf,
                             routing_dt = 1)
  mod_puls_routing(resmodel_df = jmd_resmodel,
                   inflow_df   = scaled,
                   initial_elev = 3830.0,
                   full_results = TRUE)
})
names(routes) <- scale_labels

# Split HMS benchmark data by scale factor
hms_scales <- split(hms_scaled_may1955, hms_scaled_may1955$scale)

# Test 1: Row counts match (returns the correct timeseries length) =============
test_that("row counts match between rfaR and HMS for all scale factors", {
  for (s in scale_labels) {
    n_rfar <- nrow(routes[[s]])
    n_hms  <- nrow(hms_scales[[s]])
    expect_true(abs(n_rfar - n_hms) <= 0,
                label = paste("Row count for", s,
                              "- rfaR:", n_rfar, "HMS:", n_hms))
  }
})

# Test 2: Inflows match (checks scale_hydrograph() and HMS scaling)
test_that("inflows match HMS for all scale factors", {
  for (s in scale_labels) {
    hms  <- hms_scales[[s]]
    rfar <- routes[[s]]
    n    <- min(nrow(rfar), nrow(hms))
    max_diff <- max(abs(rfar$inflow_cfs[1:n] - hms$inflow_cfs[1:n]))
    expect_lt(max_diff, 1.0,
              label = paste("Inflow match for", s,
                            "- max diff:", round(max_diff, 4), "cfs"))
  }
})

# Test 3: Elevation from routing is within 0.05-ft of HMS
test_that("elevation within 0.05-ft of HMS results for all scale factors", {
  for (s in scale_labels) {
    hms  <- hms_scales[[s]]
    rfar <- routes[[s]]
    n    <- min(nrow(rfar), nrow(hms))
    max_diff <- max(abs(rfar$elevation_ft[1:n] - hms$elevation_ft[1:n]))

    # Tolerance (in feet)
    tol <- 0.05

    expect_lt(max_diff, tol,
              label = paste("Elevation match for", s,
                            "- max diff:", round(max_diff, 4), "ft",
                            "- tolerance:", tol, "ft"))
  }
})

# Test 4: Storage from routing is within 1% of HMS
test_that("storage within 1% of HMS results for all scale factors", {
  for (s in scale_labels) {
    hms  <- hms_scales[[s]]
    rfar <- routes[[s]]
    n    <- min(nrow(rfar), nrow(hms))
    max_pct_diff    <- max((abs(rfar$storage_acft[1:n] - hms$storage_acft[1:n])/hms$storage_acft[1:n]))*100

    expect_lt(max_pct_diff, 1.0,
              label = paste("Storage match for", s, "- max pct diff:", round(max_pct_diff, 4), "%"))
  }
})

# Test 5: Outflow is within 1 cfs of HMS results
# Outflow is the most sensitive variable due to rating curve transitions (outlet threshold, spillway crest)
# and background computations in HMS.
# This is expected and not indicative of a routing algorithm error.
test_that("outflow is within 1 cfs of HMS results for all scale factors", {
  for (s in scale_labels) {
    hms  <- hms_scales[[s]]
    rfar <- routes[[s]]
    n    <- min(nrow(rfar), nrow(hms))
    outflow_ts_diff <- abs(rfar$outflow_cfs[1:n] - hms$outflow_cfs[1:n])
    max_diff <- max(outflow_ts_diff[!is.na(outflow_ts_diff)])

    expect_lt(max_diff, 1,
              label = paste("Outflow Difference for", s,
                            "- max diff:", round(max_diff, 4), "cfs"))
  }
})

# Test 6: Peak elevation comparison - verify that rfaR peak stage is within
# 0.05-ft of HMS peak stage
test_that("peak elevation within 0.05-ft of HMS results for all scale factors", {
  for (s in scale_labels) {
    hms  <- hms_scales[[s]]
    rfar <- routes[[s]]
    peak_diff <- abs(max(rfar$elevation_ft) - max(hms$elevation_ft))

    # Peak elevation tolerance: 0.1 ft for all scales
    expect_lt(peak_diff, 0.05,
              label = paste("Peak elevation for", s,
                            "- rfaR:", round(max(rfar$elevation_ft), 4),
                            "- HMS:", round(max(hms$elevation_ft), 4),
                            "- diff:", round(peak_diff, 4), "ft"))
  }
})
