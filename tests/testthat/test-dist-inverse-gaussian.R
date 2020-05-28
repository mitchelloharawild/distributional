test_that("Inverse Gaussian distribution", {
  dist <- dist_inverse_gaussian(3, .2)

  expect_equal(format(dist), "IG(3, 0.2)")

  # quantiles
  expect_equal(quantile(dist, 0.1), 0.0711412412200721) # dput(actuar::qinvgauss(0.1, 3, .2))
  expect_equal(quantile(dist, 0.5), 0.380739793444906) # dput(actuar::qinvgauss(0.5, 3, .2))

  # pdf
  expect_equal(density(dist, 0), 0) # dput(actuar::dinvgauss(0, 3, .2))
  expect_equal(density(dist, 3), 0.0343354846242835) # dput(actuar::dinvgauss(3, 3, .2))

  # cdf
  expect_equal(cdf(dist, 0), 0) # dput(actuar::pinvgauss(0, 3, .2))
  expect_equal(cdf(dist, 3), 0.845975250363882) # dput(actuar::pinvgauss(3, 3, .2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 3) # dput(actuar::minvgauss(1, 3, .2))
  expect_equal(variance(dist), 135) # dput(actuar::minvgauss(2, 3, .2) - actuar::minvgauss(1, 3, .2)^2)
})
