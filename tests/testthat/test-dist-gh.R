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

test_that("g-and-h distribution is vectorised over several distributions", {
  skip_if_not_installed("gk", "0.1.0")
  dist <- dist_gh(1:3, 1, c(0, .2, .4), .1, c(0.8, 0.5, 0.8))
  at <- c(0, 1, 2)

  expect_equal(format(dist), c(
    "gh(A = 1, B = 1, g = 0.0, h = 0.1)",
    "gh(A = 2, B = 1, g = 0.2, h = 0.1, c = 0.5)",
    "gh(A = 3, B = 1, g = 0.4, h = 0.1)"
  ))
  expect_equal(density(dist, list(at))[[1]], gk::dgh(at, 1:3, 1, c(0, .2, .4), .1, c(0.8, 0.5, 0.8)))
  expect_equal(cdf(dist, list(at))[[1]], gk::pgh(at, 1:3, 1, c(0, .2, .4), .1, c(0.8, 0.5, 0.8)))
  expect_equal(quantile(dist, 0.3), gk::qgh(0.3, 1:3, 1, c(0, .2, .4), .1, c(0.8, 0.5, 0.8)))
  expect_equal(vec_size(support(dist)), 3L)
  expect_equal(mean(dist), vapply(vec_chop(dist), mean, 1), tolerance = 1e-4)

  set.seed(1)
  draws <- generate(dist, list(c(2L, 0L, 5L)))
  expect_equal(lengths(draws), c(2L, 0L, 5L))
})
