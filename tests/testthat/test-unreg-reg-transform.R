test_that("unreg_reg_transform interpolates and handles out-of-range values", {
  transform <- unreg_reg_transform(
    unreg_inflow = c(0, 100, 200),
    reg_inflow = c(0, 80, 150)
  )

  # Interpolates within the provided inflow range
  expect_equal(transform(50), 40)
  expect_equal(transform(150), 115)

  # Returns original values outside the range when extend = TRUE
  expect_equal(transform(-10), -10)
  expect_equal(transform(250), 250)
})

test_that("unreg_reg_transform returns NA outside range when extend is FALSE", {
  transform <- unreg_reg_transform(
    unreg_inflow = c(0, 100, 200),
    reg_inflow = c(0, 80, 150),
    extend = FALSE
  )

  expect_equal(transform(50), 40)
  expect_true(is.na(transform(-10)))
  expect_true(is.na(transform(250)))
})
