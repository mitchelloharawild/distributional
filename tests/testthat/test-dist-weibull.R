test_that("Weibull distribution", {
  dist <- dist_weibull(1.5, 1)

  expect_equal(format(dist), "Weibull(1.5, 1)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qweibull(0.1, 1.5, 1))
  expect_equal(quantile(dist, 0.5), stats::qweibull(0.5, 1.5, 1))

  # pdf
  expect_equal(density(dist, 0), stats::dweibull(0, 1.5, 1))
  expect_equal(density(dist, 3), stats::dweibull(3, 1.5, 1))

  # cdf
  expect_equal(cdf(dist, 0), stats::pweibull(0, 1.5, 1))
  expect_equal(cdf(dist, 3), stats::pweibull(3, 1.5, 1))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 1 * gamma(1 + 1/1.5))
  expect_equal(variance(dist), 1^2 * (gamma(1 + 2/1.5) - gamma(1 + 1/1.5)^2))
})
