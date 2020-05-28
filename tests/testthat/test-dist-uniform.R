test_that("Uniform distribution", {
  dist <- dist_uniform(-2, 4)

  expect_equal(format(dist), "U(-2, 4)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qunif(0.1, -2, 4))
  expect_equal(quantile(dist, 0.5), stats::qunif(0.5, -2, 4))

  # pdf
  expect_equal(density(dist, 0), stats::dunif(0, -2, 4))
  expect_equal(density(dist, 3), stats::dunif(3, -2, 4))

  # cdf
  expect_equal(cdf(dist, 0), stats::punif(0, -2, 4))
  expect_equal(cdf(dist, 3), stats::punif(3, -2, 4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.5*(-2 + 4))
  expect_equal(variance(dist), (4+2)^2/12)
})
