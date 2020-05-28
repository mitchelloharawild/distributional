test_that("Exponential distribution", {
  dist <- dist_exponential(2)

  expect_equal(format(dist), "Exp(2)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qexp(0.1, 2))
  expect_equal(quantile(dist, 0.5), stats::qexp(0.5, 2))

  # pdf
  expect_equal(density(dist, 0), stats::dexp(0, 2))
  expect_equal(density(dist, 3), stats::dexp(3, 2))

  # cdf
  expect_equal(cdf(dist, 0), stats::pexp(0, 2))
  expect_equal(cdf(dist, 3), stats::pexp(3, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 1/2)
  expect_equal(variance(dist), 1/2^2)
})
