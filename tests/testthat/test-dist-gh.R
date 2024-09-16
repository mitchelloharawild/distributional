test_that("g-and-h distribution", {
  # Define g-and-h distribution parameters
  A <- 0
  B <- 1
  g <- 0
  h <- 0.5
  c <- 0.8
  dist <- dist_gh(A, B, g, h, c)

  # Check formatting
  expect_equal(format(dist), "gh(A = 0, B = 1, g = 0, h = 0.5)")

  # Require package installed
  skip_if_not_installed("gk", "0.1.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), gk::qgh(0.1, A, B, g, h, c))
  expect_equal(quantile(dist, 0.5), gk::qgh(0.5, A, B, g, h, c))

  # pdf
  expect_equal(density(dist, 0), gk::dgh(0, A, B, g, h, c))
  expect_equal(density(dist, 3), gk::dgh(3, A, B, g, h, c))

  # cdf
  expect_equal(cdf(dist, 0), gk::pgh(0, A, B, g, h, c))
  expect_equal(cdf(dist, 3), gk::pgh(3, A, B, g, h, c))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # Generate random samples
  set.seed(123)
  samples <- generate(dist, 10)
  set.seed(123)
  expect_equal(samples[[1L]], gk::rgh(10, A, B, g, h, c))
})
