test_that("Geometric distribution", {
  dist <- dist_geometric(0.4)

  expect_equal(format(dist), "Geometric(0.4)")

  # quantiles
  expect_equal(quantile(dist, 0.6), stats::qgeom(0.6, 0.4))
  expect_equal(quantile(dist, 0.9), stats::qgeom(0.9, 0.4))

  # pdf
  expect_equal(density(dist, 0), stats::dgeom(0, 0.4))
  expect_equal(density(dist, 5), stats::dgeom(5, 0.4))

  # cdf
  expect_equal(cdf(dist, 0), stats::pgeom(0, 0.4))
  expect_equal(cdf(dist, 10), stats::pgeom(10, 0.4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.64)), 0.64, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 1/0.4 - 1)
  expect_equal(variance(dist), 1/0.4^2 - 1/0.4)
})
