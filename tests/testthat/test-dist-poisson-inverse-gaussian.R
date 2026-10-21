test_that("Poisson Inverse Gaussian distribution", {
  dist <- dist_poisson_inverse_gaussian(0.1, 0.8)

  expect_equal(format(dist), "PIG(0.1, 0.8)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qpig(0.1, 0.1, 0.8))
  expect_equal(quantile(dist, 0.5), actuar::qpig(0.5, 0.1, 0.8))

  # pdf
  expect_equal(density(dist, 0), actuar::dpig(0, 0.1, 0.8))
  expect_equal(density(dist, 3), actuar::dpig(3, 0.1, 0.8))

  # cdf
  expect_equal(cdf(dist, 0), actuar::ppig(0, 0.1, 0.8))
  expect_equal(cdf(dist, 3), actuar::ppig(3, 0.1, 0.8))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.994)), 0.994, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.1)
  expect_equal(variance(dist), 0.1/0.8*(0.1^2 + 0.8))
})

test_that("Poisson inverse Gaussian distribution is vectorised over several distributions", {
  dist <- dist_poisson_inverse_gaussian(1:3, c(2, 3, 4))
  n <- 3
  at <- c(1, 3, 5)

  expect_equal(density(dist, list(at))[[1]], actuar::dpoisinvgauss(at, 1:3, c(2, 3, 4)))
  expect_equal(cdf(dist, list(at))[[1]], actuar::ppoisinvgauss(at, 1:3, c(2, 3, 4)))
  expect_equal(quantile(dist, 0.3), actuar::qpoisinvgauss(0.3, 1:3, c(2, 3, 4)))
  expect_equal(log_density(dist, list(at))[[1]], actuar::dpoisinvgauss(at, 1:3, c(2, 3, 4), log = TRUE))

  # support has one region per distribution
  expect_equal(vec_size(support(dist)), n)
  expect_equal(mean(dist), as.numeric(1:3))

  # generate() returns one draw vector per distribution
  set.seed(1)
  draws <- generate(dist, list(c(2L, 0L, 5L)))
  expect_equal(lengths(draws), c(2L, 0L, 5L))
  expect_true(all(unlist(draws) >= 0))
})
