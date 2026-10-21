test_that("Logistic distribution", {
  dist <- dist_logistic(5, 2)

  expect_equal(format(dist), "Logistic(5, 2)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qlogis(0.1, 5, 2))
  expect_equal(quantile(dist, 0.5), stats::qlogis(0.5, 5, 2))

  # pdf
  expect_equal(density(dist, 0), stats::dlogis(0, 5, 2))
  expect_equal(density(dist, 3), stats::dlogis(3, 5, 2))

  # cdf
  expect_equal(cdf(dist, 0), stats::plogis(0, 5, 2))
  expect_equal(cdf(dist, 3), stats::plogis(3, 5, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 5)
  expect_equal(variance(dist), (2*pi)^2/3)
})

test_that("Logistic methods are vectorised over distributions", {
  x <- dist_logistic(1:3, c(1, 2, 3))
  n <- length(x)

  expect_length(support(x), n)
  expect_length(skewness(x), n)
  expect_length(kurtosis(x), n)
  expect_length(mean(x), n)
  expect_length(hdr(x), n)
  expect_length(density(x, 1, log = TRUE), n)
  expect_length(cdf(x, 2, log = TRUE), n)

  # density, cdf and quantile pair with the distributions
  expect_equal(density(x, 2), vapply(seq_len(n), function(i) density(x[i], 2), numeric(1L)))
  expect_equal(cdf(x, 2), vapply(seq_len(n), function(i) cdf(x[i], 2), numeric(1L)))
  expect_equal(quantile(x, 0.3), vapply(seq_len(n), function(i) quantile(x[i], 0.3), numeric(1L)))

  # generate() returns one draw vector per distribution
  set.seed(1)
  g <- generate(x, list(c(2L, 0L, 5L)))
  expect_length(g, n)
  expect_equal(lengths(g), c(2L, 0L, 5L))
})
