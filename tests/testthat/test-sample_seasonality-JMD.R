test_that("sampled seasonality matches input distribution", {
  set.seed(42)

  seasonality_prob <- jmd_seasonality$relative_frequency
  Nsims <- 1e6

  InitMonths <- sample(1:12, size = Nsims, replace = TRUE, prob = seasonality_prob)
  sample_freq <- tabulate(InitMonths, nbins = 12) / Nsims

  # Expect Equal
  expect_equal(sample_freq, seasonality_prob, tolerance = 0.01)
})
