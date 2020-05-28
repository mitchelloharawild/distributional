test_that("F distribution", {
  dist <- dist_f(5, 2)

  expect_equal(format(dist), "F(5, 2)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qf(0.1, 5, 2))
  expect_equal(quantile(dist, 0.5), stats::qf(0.5, 5, 2))

  # pdf
  expect_equal(density(dist, 0), stats::df(0, 5, 2))
  expect_equal(density(dist, 3), stats::df(3, 5, 2))

  # cdf
  expect_equal(cdf(dist, 0), stats::pf(0, 5, 2))
  expect_equal(cdf(dist, 3), stats::pf(3, 5, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), NA_real_)
  expect_equal(variance(dist), NA_real_)
  dist <- dist_f(5, 5)
  expect_equal(mean(dist), 5/(5-2))
  expect_equal(variance(dist), 2*5^2*(5+5-2)/(5*(5-2)^2*(5-4)))
})
