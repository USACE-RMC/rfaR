test_that("hydrograph sampling matches input weights", {
  set.seed(42)

  hydrographs <- hydrograph_setup(jmd_hydro_apr1999,
                                  jmd_hydro_jun1921,
                                  jmd_hydro_jun1965,
                                  jmd_hydro_jun1965_15min,
                                  jmd_hydro_may1955,
                                  jmd_hydro_pmf,
                                  jmd_hydro_sdf,
                                  critical_duration = 2,
                                  routing_days = 10,
                                  weights = c(0.5,1,1,2,2,2,4))
  Nsims <- 100000

  # Sample Order with probs (normlized weights)
  hydro_probs <- attr(hydrographs, "probs")
  hydroSamps <- sample(1:length(hydrographs), size = Nsims, replace = TRUE,
                       prob = hydro_probs)

  sample_freq <- tabulate(hydroSamps, nbins = length(hydro_probs)) / Nsims

  # Expect Equal
  expect_equal(sample_freq, hydro_probs, tolerance = 0.01)
})
