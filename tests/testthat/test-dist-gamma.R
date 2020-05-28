test_that("Gamma distribution", {
  dist <- dist_gamma(7.5, 2)

  expect_equal(format(dist), "Gamma(7.5, 2)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qgamma(0.1, 7.5, 2))
  expect_equal(quantile(dist, 0.5), stats::qgamma(0.5, 7.5, 2))

  # pdf
  expect_equal(density(dist, 0), stats::dgamma(0, 7.5, 2))
  expect_equal(density(dist, 3), stats::dgamma(3, 7.5, 2))

  # cdf
  expect_equal(cdf(dist, 0), stats::pgamma(0, 7.5, 2))
  expect_equal(cdf(dist, 3), stats::pgamma(3, 7.5, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 7.5/2)
  expect_equal(variance(dist), 7.5/2^2)
})
