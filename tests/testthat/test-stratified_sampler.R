test_that("stratified sampler z-lower bins match validation data", {
  ev1_idx <- which(example_stratified$distribution == "ev1")
  normal_idx <- which(example_stratified$distribution == "normal")
  uniform_idx <- which(example_stratified$distribution == "uniform")

  z_lower_valid <- numeric(nrow(example_stratified))
  z_lower_valid[uniform_idx] <- qnorm(1 - example_stratified$lower[uniform_idx])
  z_lower_valid[normal_idx] <- example_stratified$lower[normal_idx]
  z_lower_valid[ev1_idx] <- qnorm(exp(-exp(-example_stratified$lower[ev1_idx])))

  ev1_test <- stratified_sampler()
  normal_test <- stratified_sampler(dist = "Normal")
  uniform_test <- stratified_sampler(dist = "Uniform")

  expect_equal(ev1_test$Zlower, z_lower_valid[ev1_idx], tolerance = 1e-6)
  expect_equal(normal_test$Zlower, z_lower_valid[normal_idx], tolerance = 1e-6)
  expect_equal(uniform_test$Zlower, z_lower_valid[uniform_idx], tolerance = 1e-6)
})

test_that("stratified sampler weights match validation data", {
  ev1_idx <- which(example_stratified$distribution == "ev1")
  normal_idx <- which(example_stratified$distribution == "normal")
  uniform_idx <- which(example_stratified$distribution == "uniform")

  ev1_test <- stratified_sampler()
  normal_test <- stratified_sampler(dist = "Normal")
  uniform_test <- stratified_sampler(dist = "Uniform")

  expect_equal(ev1_test$Weights, example_stratified$weight[ev1_idx], tolerance = 1e-6)
  expect_equal(normal_test$Weights, example_stratified$weight[normal_idx], tolerance = 1e-6)
  expect_equal(uniform_test$Weights, example_stratified$weight[uniform_idx], tolerance = 1e-6)
})

test_that("stratified sampler weights sum to 1", {
  ev1_test <- stratified_sampler()
  normal_test <- stratified_sampler(dist = "Normal")
  uniform_test <- stratified_sampler(dist = "Uniform")

  expect_equal(sum(ev1_test$Weights), 1, tolerance = 1e-10)
  expect_equal(sum(normal_test$Weights), 1, tolerance = 1e-10)
  expect_equal(sum(uniform_test$Weights), 1, tolerance = 1e-10)
})

test_that("stratified sampler returns correct dimensions", {
  test <- stratified_sampler(Nbins = 10, Mevents = 100)

  expect_equal(test$Nbins, 10)
  expect_equal(test$Mevents, 100)
  expect_length(test$normOrd, 10 * 100)
  expect_length(test$Zlower, 10)
  expect_length(test$Zupper, 10)
  expect_length(test$Weights, 10)
})

test_that("stratified sampler bins are ordered (Zlower < Zupper)", {
  ev1_test <- stratified_sampler()
  normal_test <- stratified_sampler(dist = "Normal")
  uniform_test <- stratified_sampler(dist = "Uniform")

  expect_true(all(ev1_test$Zlower < ev1_test$Zupper))
  expect_true(all(normal_test$Zlower < normal_test$Zupper))
  expect_true(all(uniform_test$Zlower < uniform_test$Zupper))
})

test_that("invalid distribution defaults to EV1 with warning", {
  expect_warning(result <- stratified_sampler(dist = "garbage"))
  ev1_test <- stratified_sampler(dist = "EV1")

  expect_equal(result$Zlower, ev1_test$Zlower)
  expect_equal(result$Weights, ev1_test$Weights)
})
