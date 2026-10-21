test_that("Negative Binomial distribution", {
  dist <- dist_negative_binomial(10, 0.4)

  expect_equal(format(dist), "NB(10, 0.4)")

  # quantiles
  expect_equal(quantile(dist, 0.6), stats::qnbinom(0.6, 10, 0.4))
  expect_equal(quantile(dist, 0.61), stats::qnbinom(0.61, 10, 0.4))

  # pdf
  expect_equal(density(dist, 0), stats::dnbinom(0, 10, 0.4))
  expect_equal(density(dist, 1), stats::dnbinom(1, 10, 0.4))

  # cdf
  expect_equal(cdf(dist, 0), stats::pnbinom(0, 10, 0.4))
  expect_equal(cdf(dist, 1), stats::pnbinom(1, 10, 0.4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.6358)), 0.6358, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.6*10/(1-0.6))
  expect_equal(variance(dist), 0.6*10/(1-0.6)^2)
})

test_that("Negative binomial distribution is vectorised over several distributions", {
  dist <- dist_negative_binomial(5:7, c(0.3, 0.4, 0.5))
  n <- 3
  at <- c(1, 3, 6)

  expect_equal(density(dist, list(at))[[1]], stats::dnbinom(at, 5:7, c(0.3, 0.4, 0.5)))
  expect_equal(cdf(dist, list(at))[[1]], stats::pnbinom(at, 5:7, c(0.3, 0.4, 0.5)))
  expect_equal(quantile(dist, 0.3), stats::qnbinom(0.3, 5:7, c(0.3, 0.4, 0.5)))
  expect_equal(log_density(dist, list(at))[[1]], stats::dnbinom(at, 5:7, c(0.3, 0.4, 0.5), log = TRUE))

  # support has one region per distribution
  expect_equal(vec_size(support(dist)), n)
  expect_equal(mean(dist), 5:7 * (1 - c(0.3, 0.4, 0.5)) / c(0.3, 0.4, 0.5))
  expect_equal(skewness(dist), vapply(vec_chop(dist), skewness, 1))

  # generate() returns one draw vector per distribution
  set.seed(1)
  draws <- generate(dist, list(c(2L, 0L, 5L)))
  expect_equal(lengths(draws), c(2L, 0L, 5L))
  expect_true(all(unlist(draws) >= 0))
})
