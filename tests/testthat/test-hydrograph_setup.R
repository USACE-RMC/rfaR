# Build a padded version of a hydrograph for testing trailing-row cleanup.
# Simulates the Excel-export artifact where read.csv carries trailing rows
# with NA in numeric columns and "" in character columns.
make_padded_hydro <- function(hydro, n_pad = 5) {
  padding_row <- hydro[1, ]
  padding_row[] <- NA
  char_cols <- vapply(padding_row, is.character, logical(1))
  padding_row[char_cols] <- ""
  rbind(hydro, padding_row[rep(1, n_pad), ])
}

test_that("hydrograph_setup truncates trailing empty rows", {
  padded <- make_padded_hydro(jmd_hydro_apr1999, n_pad = 10)

  result <- suppressMessages(
    hydrograph_setup(padded,
                     critical_duration = 2,
                     routing_days = 10)
  )

  # obs_vol should be finite (not NA from rollmean over NAs)
  expect_true(is.finite(attr(result[[1]], "obs_vol")))

  # Should match the result from clean input
  clean_result <- hydrograph_setup(jmd_hydro_apr1999,
                                   critical_duration = 2,
                                   routing_days = 10)
  expect_equal(attr(result[[1]], "obs_vol"),
               attr(clean_result[[1]], "obs_vol"))
})

test_that("hydrograph_setup emits a message for trailing rows", {
  padded <- make_padded_hydro(jmd_hydro_apr1999, n_pad = 5)

  expect_message(
    hydrograph_setup(padded, critical_duration = 2, routing_days = 10),
    "Dropped 5 trailing rows"
  )
})
