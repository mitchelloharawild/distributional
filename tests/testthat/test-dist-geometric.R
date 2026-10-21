test_that("Geometric distribution", {
  dist <- dist_geometric(0.4)

  expect_equal(format(dist), "Geometric(0.4)")

  # quantiles
  expect_equal(quantile(dist, 0.6), stats::qgeom(0.6, 0.4))
  expect_equal(quantile(dist, 0.9), stats::qgeom(0.9, 0.4))

  # pdf
  expect_equal(density(dist, 0), stats::dgeom(0, 0.4))
  expect_equal(density(dist, 5), stats::dgeom(5, 0.4))

  # cdf
  expect_equal(cdf(dist, 0), stats::pgeom(0, 0.4))
  expect_equal(cdf(dist, 10), stats::pgeom(10, 0.4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.64)), 0.64, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 1/0.4 - 1)
  expect_equal(variance(dist), 1/0.4^2 - 1/0.4)
})

test_that("Geometric methods are vectorised over distributions", {
  x <- dist_geometric(c(0.2, 1, 0.8))
  n <- length(x)

  expect_length(support(x), n)
  expect_length(skewness(x), n)
  expect_length(kurtosis(x), n)
  expect_length(mean(x), n)
  expect_length(hdr(x), n)
  expect_length(density(x, 1, log = TRUE), n)
  expect_length(cdf(x, 2, log = TRUE), n)

  # per-element closed flags
  expect_equal(vec_proxy(support(x))$lim[[2]], c(0, 0))
  expect_equal(vec_proxy(support(x))$closed[[2]], c(TRUE, TRUE))
  expect_equal(vec_proxy(support(x))$closed[[1]], c(TRUE, FALSE))

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
