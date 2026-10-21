test_that("Exponential distribution", {
  dist <- dist_exponential(2)

  expect_equal(format(dist), "Exp(2)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qexp(0.1, 2))
  expect_equal(quantile(dist, 0.5), stats::qexp(0.5, 2))

  # pdf
  expect_equal(density(dist, 0), stats::dexp(0, 2))
  expect_equal(density(dist, 3), stats::dexp(3, 2))

  # cdf
  expect_equal(cdf(dist, 0), stats::pexp(0, 2))
  expect_equal(cdf(dist, 3), stats::pexp(3, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 1/2)
  expect_equal(variance(dist), 1/2^2)
})

test_that("Exponential distribution is vectorised over distributions", {
  dist <- dist_exponential(c(0.5, 1, 2))
  n <- vctrs::vec_size(dist)

  # support() has one region per distribution
  sup <- support(dist)
  expect_equal(vctrs::vec_size(sup), n)
  expect_equal(vctrs::field(sup, "lim"), rep(list(c(0, Inf)), 3))
  expect_equal(vctrs::field(sup, "closed"), rep(list(c(TRUE, FALSE)), 3))

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
