test_that("g-k distribution", {
  # Define g-k distribution parameters
  A <- 0
  B <- 1
  g <- 0
  k <- 0.5
  c <- 0.8
  dist <- dist_gk(A, B, g, k, c)

  # Check formatting
  expect_equal(format(dist), "gk(A = 0, B = 1, g = 0, k = 0.5)")

  # Require package installed
  skip_if_not_installed("gk", "0.1.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), gk::qgk(0.1, A, B, g, k, c))
  expect_equal(quantile(dist, 0.5), gk::qgk(0.5, A, B, g, k, c))

  # pdf
  expect_equal(density(dist, 0), gk::dgk(0, A, B, g, k, c))
  expect_equal(density(dist, 3), gk::dgk(3, A, B, g, k, c))

  # cdf
  expect_equal(cdf(dist, 0), gk::pgk(0, A, B, g, k, c))
  expect_equal(cdf(dist, 3), gk::pgk(3, A, B, g, k, c))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # Generate random samples
  set.seed(123)
  samples <- generate(dist, 10)
  set.seed(123)
  expect_equal(samples[[1L]], gk::rgk(10, A, B, g, k, c))
})
