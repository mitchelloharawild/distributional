test_that("Uniform distribution", {
  dist <- dist_uniform(-2, 4)

  expect_equal(format(dist), "U(-2, 4)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qunif(0.1, -2, 4))
  expect_equal(quantile(dist, 0.5), stats::qunif(0.5, -2, 4))

  # pdf
  expect_equal(density(dist, 0), stats::dunif(0, -2, 4))
  expect_equal(density(dist, 3), stats::dunif(3, -2, 4))

  # cdf
  expect_equal(cdf(dist, 0), stats::punif(0, -2, 4))
  expect_equal(cdf(dist, 3), stats::punif(3, -2, 4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.5*(-2 + 4))
  expect_equal(variance(dist), (4+2)^2/12)
})

test_that("Uniform distribution is vectorised over several distributions", {
  dist <- dist_uniform(c(0, 1, 2), c(2, 4, 8))
  n <- 3
  at <- c(0.5, 2, 5)

  expect_equal(density(dist, list(at))[[1]], stats::dunif(at, c(0, 1, 2), c(2, 4, 8)))
  expect_equal(cdf(dist, list(at))[[1]], stats::punif(at, c(0, 1, 2), c(2, 4, 8)))
  expect_equal(quantile(dist, 0.3), stats::qunif(0.3, c(0, 1, 2), c(2, 4, 8)))
  expect_equal(log_density(dist, list(at))[[1]], stats::dunif(at, c(0, 1, 2), c(2, 4, 8), log = TRUE))

  # support has one region per distribution
  expect_equal(vec_size(support(dist)), n)
  expect_equal(mean(dist), c(1, 2.5, 5))
  expect_equal(skewness(dist), rep(0, n))
  expect_equal(kurtosis(dist), rep(-6/5, n))

  # generate() returns one draw vector per distribution
  set.seed(1)
  draws <- generate(dist, list(c(2L, 0L, 5L)))
  expect_equal(lengths(draws), c(2L, 0L, 5L))
  expect_true(all(unlist(draws) >= 0))
})
