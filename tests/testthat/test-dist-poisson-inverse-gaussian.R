test_that("Poisson Inverse Gaussian distribution", {
  dist <- dist_poisson_inverse_gaussian(0.1, 0.8)

  expect_equal(format(dist), "PIG(0.1, 0.8)")

  # quantiles
  expect_equal(quantile(dist, 0.1), 0) # dput(actuar::qpig(0.1, 0.1, 0.8))
  expect_equal(quantile(dist, 0.5), 0) # dput(actuar::qpig(0.5, 0.1, 0.8))

  # pdf
  expect_equal(density(dist, 0), 0.9053961533743) # dput(actuar::dpig(0, 0.1, 0.8))
  expect_equal(density(dist, 3), 0.000205922979598663) # dput(actuar::dpig(3, 0.1, 0.8))

  # cdf
  expect_equal(cdf(dist, 0), 0.9053961533743) # dput(actuar::ppig(0, 0.1, 0.8))
  expect_equal(cdf(dist, 3), 0.999992597387298) # dput(actuar::ppig(3, 0.1, 0.8))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.994)), 0.994, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.1)
  expect_equal(variance(dist), 0.1/0.8*(0.1^2 + 0.8))
})
