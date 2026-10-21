test_that("Logarithmic distribution", {
  dist <- dist_logarithmic(0.66)

  expect_equal(format(dist), "Logarithmic(0.66)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.5), actuar::qlogarithmic(0.5, 0.66))
  expect_equal(quantile(dist, 0.99), actuar::qlogarithmic(0.99, 0.66))

  # pdf
  expect_equal(density(dist, 1), actuar::dlogarithmic(1, 0.66))
  expect_equal(density(dist, 9), actuar::dlogarithmic(9, 0.66))

  # cdf
  expect_equal(cdf(dist, 3), actuar::plogarithmic(3, 0.66))
  expect_equal(cdf(dist, 12), actuar::plogarithmic(12, 0.66))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.9963064)), 0.9963064, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), -1/log(1-0.66)*(0.66/(1-0.66)))
  expect_equal(variance(dist), -(0.66^2 + 0.66*log(1-0.66))/((1-0.66)^2*log(1-0.66)^2))
})

test_that("Logarithmic methods are vectorised over distributions", {
  x <- dist_logarithmic(c(0, 0.5, 0.8))
  n <- length(x)

  expect_length(support(x), n)
  expect_length(mean(x), n)
  expect_length(hdr(x), n)
  expect_length(density(x, 1, log = TRUE), n)
  expect_length(cdf(x, 2, log = TRUE), n)

  # per-element closed flags
  expect_equal(vec_proxy(support(x))$lim[[1]], c(1, 1))
  expect_equal(vec_proxy(support(x))$closed[[1]], c(TRUE, TRUE))
  expect_equal(vec_proxy(support(x))$closed[[2]], c(TRUE, FALSE))

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
