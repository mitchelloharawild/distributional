test_that("Gumbel distribution", {
  dist <- dist_gumbel(1, 2)

  expect_equal(format(dist), "Gumbel(1, 2)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qgumbel(0.1, 1, 2))
  expect_equal(quantile(dist, 0.5), actuar::qgumbel(0.5, 1, 2))

  # pdf
  expect_equal(density(dist, 0), actuar::dgumbel(0, 1, 2))
  expect_equal(density(dist, 3), actuar::dgumbel(3, 1, 2))

  # cdf
  expect_equal(cdf(dist, 0), actuar::pgumbel(0, 1, 2))
  expect_equal(cdf(dist, 3), actuar::pgumbel(3, 1, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), actuar::mgumbel(1, 1, 2))
  expect_equal(variance(dist), actuar::mgumbel(2, 1, 2) - actuar::mgumbel(1, 1, 2)^2)
})
