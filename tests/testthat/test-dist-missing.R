test_that("Missing distribution", {
  dist <- dist_missing()

  expect_equal(format(dist), "NA")

  # quantiles
  expect_equal(quantile(dist, c(0.1, 0.9)), list(c(NA_real_, NA_real_)))

  # pdf
  expect_equal(density(dist, 1:2), list(c(NA_real_, NA_real_)))

  # cdf
  expect_equal(cdf(dist, 1:2), list(c(NA_real_, NA_real_)))

  # stats
  expect_equal(mean(dist), NA_real_)
  expect_equal(variance(dist), NA_real_)
})
