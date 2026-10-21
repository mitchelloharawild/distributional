test_that("Beta distribution", {
  dist <- dist_beta(3, 4)

  expect_equal(format(dist), "Beta(3, 4)")

  # quantiles
  expect_equal(quantile(dist, 0.1), qbeta(0.1, 3, 4))
  expect_equal(quantile(dist, 0.5), qbeta(0.5, 3, 4))

  # pdf
  expect_equal(density(dist, 0), dbeta(0, 3, 4))
  expect_equal(density(dist, 3), dbeta(3, 3, 4))

  # cdf
  expect_equal(cdf(dist, 0), pbeta(0, 3, 4))
  expect_equal(cdf(dist, 3), pbeta(3, 3, 4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 3/(3+4))
  expect_equal(variance(dist), 3*4/((3+4)^2*(3+4+1)))
})

test_that("Beta distribution is vectorised over distributions", {
  dist <- dist_beta(c(0.5, 2, 3), c(2, 0.5, 4))
  n <- vctrs::vec_size(dist)

  # support() has one region per distribution
  sup <- support(dist)
  expect_equal(vctrs::vec_size(sup), n)
  expect_equal(vctrs::field(sup, "lim"), rep(list(c(0, 1)), 3))
  expect_equal(vctrs::field(sup, "closed"), list(c(TRUE, FALSE), c(FALSE, TRUE), c(FALSE, FALSE)))

  # generate() gives one set of draws per distribution
  set.seed(1)
  draws <- generate(dist, 4)
  expect_length(draws, n)
  expect_equal(lengths(draws), rep(4L, n))
  expect_equal(lengths(generate(dist, list(c(1L, 0L, 3L)))), c(1L, 0L, 3L))

  # draws match those from each distribution on its own
  set.seed(1)
  single <- lapply(seq_len(n), function(i) generate(dist[i], 4)[[1]])
  expect_equal(draws, single)

  # moments and log-scale methods agree with one distribution at a time
  by_elem <- function(f, ...) unlist(lapply(seq_len(n), function(i) f(dist[i], ...)))
  expect_equal(skewness(dist), by_elem(skewness))
  expect_equal(kurtosis(dist), by_elem(kurtosis))
  at <- 1
  expect_equal(log_density(dist, at), log(density(dist, at)))
  expect_equal(log_cdf(dist, at), log(cdf(dist, at)))
})
