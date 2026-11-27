test_that("Hypergeometric distribution", {
  dist <- dist_hypergeometric(500, 50, 100)

  expect_equal(format(dist), "Hypergeometric(500, 50, 100)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qhyper(0.1, 500, 50, 100))
  expect_equal(quantile(dist, 0.5), stats::qhyper(0.5, 500, 50, 100))

  # pdf
  expect_equal(density(dist, 0), stats::dhyper(0, 500, 50, 100))
  expect_equal(density(dist, 3), stats::dhyper(3, 500, 50, 100))

  # cdf
  expect_equal(cdf(dist, 0), stats::phyper(0, 500, 50, 100))
  expect_equal(cdf(dist, 3), stats::phyper(3, 500, 50, 100))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  p <- 500/(500+50)
  expect_equal(mean(dist), 100*p)
  expect_equal(variance(dist), 100*p*(1-p)*(500+50-100)/(500+50-1))

  # kurtosis against exact fourth central moment from stats::dhyper
  pmf_kurtosis <- function(m, n, k) {
    xs <- max(0L, k - n):min(k, m)
    ps <- stats::dhyper(xs, m, n, k)
    mu <- sum(xs * ps)
    v <- sum((xs - mu)^2 * ps)
    sum(((xs - mu) / sqrt(v))^4 * ps) - 3
  }
  expect_equal(kurtosis(dist), pmf_kurtosis(500, 50, 100))
  expect_equal(
    kurtosis(dist_hypergeometric(c(10, 100, 2), c(15, 1, 2), c(7, 10, 2))),
    c(pmf_kurtosis(10, 15, 7), pmf_kurtosis(100, 1, 10), pmf_kurtosis(2, 2, 2))
  )
})

test_that("Hypergeometric methods are vectorised over distributions", {
  x <- dist_hypergeometric(5:7, 4, 3)
  n <- length(x)

  expect_length(support(x), n)
  expect_length(skewness(x), n)
  expect_length(kurtosis(x), n)
  expect_length(mean(x), n)
  expect_length(hdr(x), n)
  expect_length(density(x, 1, log = TRUE), n)
  expect_length(cdf(x, 2, log = TRUE), n)

  expect_equal(vec_proxy(support(x))$lim, rep(list(c(0, 3)), 3))

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
