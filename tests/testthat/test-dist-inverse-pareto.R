
test_that("Inverse Pareto distribution", {
  # Define Inverse Pareto distribution parameters
  shape <- 2.5
  scale <- 3
  dist <- dist_inverse_pareto(shape, scale)

  # Check formatting
  expect_equal(format(dist), "InvPareto(shape = 2.5, scale = 3)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # Quantiles
  expect_equal(quantile(dist, 0.1), actuar::qinvpareto(0.1, shape, scale))
  expect_equal(quantile(dist, 0.5), actuar::qinvpareto(0.5, shape, scale))

  # PDF
  expect_equal(density(dist, 2), actuar::dinvpareto(2, shape, scale))
  expect_equal(density(dist, 3), actuar::dinvpareto(3, shape, scale))

  # CDF
  expect_equal(cdf(dist, 2), actuar::pinvpareto(2, shape, scale))
  expect_equal(cdf(dist, 3), actuar::pinvpareto(3, shape, scale))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # Generate random samples
  set.seed(123)
  samples <- generate(dist, 10)
  set.seed(123)
  expect_equal(samples[[1L]], actuar::rinvpareto(10, shape, scale))

  # Mean and variance
  expect_equal(mean(dist), shape * scale / (shape - 1))
  expect_equal(variance(dist), shape * scale^2 / ((shape - 1)^2 * (shape - 2)))
})
