test_that("Logistic distribution", {
  dist <- dist_logistic(5, 2)

  expect_equal(format(dist), "Logistic(5, 2)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qlogis(0.1, 5, 2))
  expect_equal(quantile(dist, 0.5), stats::qlogis(0.5, 5, 2))

  # pdf
  expect_equal(density(dist, 0), stats::dlogis(0, 5, 2))
  expect_equal(density(dist, 3), stats::dlogis(3, 5, 2))

  # cdf
  expect_equal(cdf(dist, 0), stats::plogis(0, 5, 2))
  expect_equal(cdf(dist, 3), stats::plogis(3, 5, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 5)
  expect_equal(variance(dist), (2*pi)^2/3)
})
