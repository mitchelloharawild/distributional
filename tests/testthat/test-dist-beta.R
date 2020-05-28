test_that("Beta distribution", {
  dist <- dist_beta(3, 4)

  expect_equal(format(dist), "Beta(3, 4)")

  # quantiles
  expect_equal(quantile(dist, 0.1), qbeta(0.1, 3, 4))
  expect_equal(quantile(dist, 0.5), qbeta(0.5, 3, 4))

  # pdf
  expect_equal(density(dist, 0), dbeta(0, 3, 4))
  expect_equal(density(dist, 3), dbeta(3, 3, 4))

  # cdf
  expect_equal(cdf(dist, 0), pbeta(0, 3, 4))
  expect_equal(cdf(dist, 3), pbeta(3, 3, 4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 3/(3+4))
  expect_equal(variance(dist), 3*4/((3+4)^2*(3+4+1)))
})
