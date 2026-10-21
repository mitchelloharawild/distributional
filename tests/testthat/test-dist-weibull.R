test_that("Weibull distribution", {
  dist <- dist_weibull(1.5, 1)

  expect_equal(format(dist), "Weibull(1.5, 1)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qweibull(0.1, 1.5, 1))
  expect_equal(quantile(dist, 0.5), stats::qweibull(0.5, 1.5, 1))

  # pdf
  expect_equal(density(dist, 0), stats::dweibull(0, 1.5, 1))
  expect_equal(density(dist, 3), stats::dweibull(3, 1.5, 1))

  # cdf
  expect_equal(cdf(dist, 0), stats::pweibull(0, 1.5, 1))
  expect_equal(cdf(dist, 3), stats::pweibull(3, 1.5, 1))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 1 * gamma(1 + 1/1.5))
  expect_equal(variance(dist), 1^2 * (gamma(1 + 2/1.5) - gamma(1 + 1/1.5)^2))
})

test_that("Weibull distribution is vectorised over several distributions", {
  dist <- dist_weibull(c(0.5, 1, 2), c(1, 2, 3))
  n <- 3
  at <- c(0.5, 2, 4)

  expect_equal(density(dist, list(at))[[1]], stats::dweibull(at, c(0.5, 1, 2), c(1, 2, 3)))
  expect_equal(cdf(dist, list(at))[[1]], stats::pweibull(at, c(0.5, 1, 2), c(1, 2, 3)))
  expect_equal(quantile(dist, 0.3), stats::qweibull(0.3, c(0.5, 1, 2), c(1, 2, 3)))
  expect_equal(log_density(dist, list(at))[[1]], stats::dweibull(at, c(0.5, 1, 2), c(1, 2, 3), log = TRUE))

  # support has one region per distribution
  expect_equal(vec_size(support(dist)), n)
  expect_equal(mean(dist), c(1, 2, 3) * gamma(1 + 1 / c(0.5, 1, 2)))
  expect_equal(skewness(dist), vapply(vec_chop(dist), skewness, 1))

  # generate() returns one draw vector per distribution
  set.seed(1)
  draws <- generate(dist, list(c(2L, 0L, 5L)))
  expect_equal(lengths(draws), c(2L, 0L, 5L))
  expect_true(all(unlist(draws) >= 0))
})
