test_that("Cherry Creek mod-puls routing matches HEC-HMS results", {
  cc_routing <- mod_puls_routing(resmodel_df = cc_resmodel,
                                 inflow_df = cc_inflowhydro,
                                 initial_elev = cc_init_elev,
                                 full_results = TRUE)

  max_diff_elev <- max(abs(cc_routing$elevation_ft - cc_hms_results$elevation_ft))
  max_diff_outflow <- max(abs(cc_routing$outflow_cfs - cc_hms_results$outflow_cfs))

  expect_lt(max_diff_elev, 0.1)
  expect_lt(max_diff_outflow, 1.0)
})
