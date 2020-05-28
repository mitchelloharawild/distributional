test_that("Negative Binomial distribution", {
  dist <- dist_normal(0, 1)
  percentiles <- seq(0.01, 0.99, by = 0.01)
  x <- vapply(percentiles, quantile, double(1L), x = dist)
  dist <- dist_percentile(list(x), list(percentiles*100))

  expect_equal(format(dist), "percentile[99]")

  # quantiles
  expect_equal(quantile(dist, 0.6), stats::qnorm(0.6, 0, 1))
  expect_equal(quantile(dist, 0.61), stats::qnorm(0.61, 0, 1))

  # pdf

  # cdf
  expect_equal(cdf(dist, 0), stats::pnorm(0, 0, 1))
  expect_equal(cdf(dist, 1), stats::pnorm(1, 0, 1), tolerance = 1e-3)

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.6)), 0.6, tolerance = 1e-3)

  # stats
})
