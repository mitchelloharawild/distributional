test_that("Logarithmic distribution", {
  dist <- dist_logarithmic(0.66)

  expect_equal(format(dist), "Logarithmic(0.66)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.5), actuar::qlogarithmic(0.5, 0.66))
  expect_equal(quantile(dist, 0.99), actuar::qlogarithmic(0.99, 0.66))

  # pdf
  expect_equal(density(dist, 1), actuar::dlogarithmic(1, 0.66))
  expect_equal(density(dist, 9), actuar::dlogarithmic(9, 0.66))

  # cdf
  expect_equal(cdf(dist, 3), actuar::plogarithmic(3, 0.66))
  expect_equal(cdf(dist, 12), actuar::plogarithmic(12, 0.66))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.9963064)), 0.9963064, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), -1/log(1-0.66)*(0.66/(1-0.66)))
  expect_equal(variance(dist), -(0.66^2 + 0.66*log(1-0.66))/((1-0.66)^2*log(1-0.66)^2))
})
