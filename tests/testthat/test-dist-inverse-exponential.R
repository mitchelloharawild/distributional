test_that("Inverse Exponential distribution", {
  dist <- dist_inverse_exponential(5)

  expect_equal(format(dist), "InvExp(5)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qinvexp(0.1, 5))
  expect_equal(quantile(dist, 0.5), actuar::qinvexp(0.5, 5))

  # pdf
  expect_equal(density(dist, 0), actuar::dinvexp(0, 5))
  expect_equal(density(dist, 3), actuar::dinvexp(3, 5))

  # cdf
  expect_equal(cdf(dist, 0), actuar::pinvexp(0, 5))
  expect_equal(cdf(dist, 3), actuar::pinvexp(3, 5))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), NA_real_) # dput(actuar::minvexp(1, 5))
  expect_equal(variance(dist), NA_real_) # dput(actuar::minvexp(2, 5) - actuar::minvexp(1, 5)^2)
})
