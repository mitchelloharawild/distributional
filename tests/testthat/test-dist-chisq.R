test_that("Chisq distribution", {
  dist <- dist_chisq(9)

  expect_equal(format(dist), "x2(9)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qchisq(0.1, 9))
  expect_equal(quantile(dist, 0.5), stats::qchisq(0.5, 9))

  # pdf
  expect_equal(density(dist, 0), stats::dchisq(0, 9))
  expect_equal(density(dist, 3), stats::dchisq(3, 9))

  # cdf
  expect_equal(cdf(dist, 0), stats::pchisq(0, 9))
  expect_equal(cdf(dist, 3), stats::pchisq(3, 9))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 9)
  expect_equal(variance(dist), 2*9)
})
