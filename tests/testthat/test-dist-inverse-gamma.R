test_that("Inverse Gamma distribution", {
  dist <- dist_inverse_gamma(3, 2)

  expect_equal(format(dist), "InvGamma(3, 0.5)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qinvgamma(0.1, 3, 2))
  expect_equal(quantile(dist, 0.5), actuar::qinvgamma(0.5, 3, 2))

  # pdf
  expect_equal(density(dist, 0), actuar::dinvgamma(0, 3, 2))
  expect_equal(density(dist, 3), actuar::dinvgamma(3, 3, 2))

  # cdf
  expect_equal(cdf(dist, 0), actuar::pinvgamma(0, 3, 2))
  expect_equal(cdf(dist, 3), actuar::pinvgamma(3, 3, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), (1/2) / (3 - 1))
  expect_equal(median(dist), actuar::qinvgamma(0.5, 3, 2))
  expect_equal(median(dist[[1]]), actuar::qinvgamma(0.5, 3, 2))
  expect_equal(variance(dist), (1/2)^2/((3-1)^2*(3-2)))
})
