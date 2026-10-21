test_that("Inverse Gaussian distribution", {
  dist <- dist_inverse_gaussian(3, .2)

  expect_equal(format(dist), "IG(3, 0.2)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qinvgauss(0.1, 3, .2))
  expect_equal(quantile(dist, 0.5), actuar::qinvgauss(0.5, 3, .2))

  # pdf
  expect_equal(density(dist, 0), actuar::dinvgauss(0, 3, .2))
  expect_equal(density(dist, 3), actuar::dinvgauss(3, 3, .2))

  # cdf
  expect_equal(cdf(dist, 0), actuar::pinvgauss(0, 3, .2))
  expect_equal(cdf(dist, 3), actuar::pinvgauss(3, 3, .2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), actuar::minvgauss(1, 3, .2))
  expect_equal(variance(dist), actuar::minvgauss(2, 3, .2) - actuar::minvgauss(1, 3, .2)^2)
})

test_that("Inverse gaussian methods are vectorised over distributions", {
  x <- dist_inverse_gaussian(1:3, c(1, 2, 3))
  n <- length(x)

  expect_length(support(x), n)
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
