test_that("observed quantiles match jmd_stage_duration dataset (from RFA)", {
  stage_ts <- jmd_wy1980_stage
  stage_ts$months <- lubridate::month(lubridate::mdy(jmd_wy1980_stage$date))
  stage_ts <- stage_ts[!is.na(stage_ts$months), ]

  probs <- 1 - jmd_stage_duration$Probability

  obs_quantiles <- sapply(1:12, function(m) {
    unname(quantile(stage_ts$stage_ft[stage_ts$months == m],
                    probs, na.rm = TRUE))
  })

  sd_matrix <- as.matrix(jmd_stage_duration[, -1])

  max_diff <- max(abs(obs_quantiles - sd_matrix), na.rm = TRUE)

  # Expect less than 0.01-ft difference
  expect_lt(max_diff, 0.01)
})

test_that("sampled stage quantiles within 5% of observed quantiles", {
  set.seed(42)

  seasonality_prob <- jmd_seasonality$relative_frequency
  Nsims <- 10000

  InitMonths <- sample(1:12, size = Nsims, replace = TRUE, prob = seasonality_prob)
  InitStages <- numeric(Nsims)

  stage_ts <- jmd_wy1980_stage
  stage_ts$months <- lubridate::month(lubridate::mdy(jmd_wy1980_stage$date))
  stage_ts <- stage_ts[!is.na(stage_ts$months), ]

  UniqMonths <- sort(unique(InitMonths))
  for (i in 1:length(UniqMonths)) {
    sampleID <- which(InitMonths == UniqMonths[i])
    InitStages[sampleID] <- sample(stage_ts$stage_ft[stage_ts$months %in% UniqMonths[i]],
                                   size = sum(InitMonths == UniqMonths[i]), replace = TRUE)
  }

  sample_stages <- data.frame(months = InitMonths, stage_ft = InitStages)

  probs <- 1 - jmd_stage_duration$Probability

  obs_quantiles <- sapply(1:12, function(m) {
    unname(quantile(stage_ts$stage_ft[stage_ts$months == m],
                    probs, na.rm = TRUE))
  })

  sample_quantiles <- sapply(1:12, function(m) {
    unname(quantile(sample_stages$stage_ft[sample_stages$months == m],
                    probs, na.rm = TRUE))
  })

  sampled_months <- which(!is.na(sample_quantiles[1, ]))

  pct_diff <- max(abs((sample_quantiles[, sampled_months] -
                         obs_quantiles[, sampled_months]) /
                        obs_quantiles[, sampled_months]) * 100, na.rm = TRUE)

  # Expect less than 5% difference
  expect_lt(pct_diff, 5)
})
