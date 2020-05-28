test_that("Inverse Gamma distribution", {
  dist <- dist_inverse_gamma(3, 2)

  expect_equal(format(dist), "InvGamma(3, 0.5)")

  # quantiles
  expect_equal(quantile(dist, 0.1), 0.0939439883852355) # dput(actuar::qinvgamma(0.1, 3, 2))
  expect_equal(quantile(dist, 0.5), 0.186981571595056) # dput(actuar::qinvgamma(0.5, 3, 2))

  # pdf
  expect_equal(density(dist, 0), 0) # dput(actuar::dinvgamma(0, 3, 2))
  expect_equal(density(dist, 3), 0.000653149479082263) # dput(actuar::dinvgamma(3, 3, 2))

  # cdf
  expect_equal(cdf(dist, 0), 0) # dput(actuar::pinvgamma(0, 3, 2))
  expect_equal(cdf(dist, 3), 0.999318702995864) # dput(actuar::pinvgamma(3, 3, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), (1/2) / (3 - 1))
  expect_equal(variance(dist), (1/2)^2/((3-1)^2*(3-2)))
})
