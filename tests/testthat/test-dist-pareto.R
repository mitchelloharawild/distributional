test_that("Pareto distribution", {
  dist <- dist_pareto(10, 1)

  expect_equal(format(dist), "Pareto(10, 1)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qpareto(0.1, 10, 1))
  expect_equal(quantile(dist, 0.5), actuar::qpareto(0.5, 10, 1))

  # pdf
  expect_equal(density(dist, 0), actuar::dpareto(0, 10, 1))
  expect_equal(density(dist, 3), actuar::dpareto(3, 10, 1))

  # cdf
  expect_equal(cdf(dist, 0), actuar::ppareto(0, 10, 1))
  expect_equal(cdf(dist, 3), actuar::ppareto(3, 10, 1))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), actuar::mpareto(1, 10, 1))
  expect_equal(variance(dist), actuar::mpareto(2, 10, 1) - actuar::mpareto(1, 10, 1)^2)
})
