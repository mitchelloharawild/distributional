test_that("Bernoulli distribution", {
  dist <- dist_bernoulli(0.4)

  expect_equal(format(dist), "Bernoulli(0.4)")

  # quantiles
  expect_equal(quantile(dist, 0.6), FALSE)
  expect_equal(quantile(dist, 0.61), TRUE)

  # pdf
  expect_equal(density(dist, 0), stats::dbinom(0, 1, 0.4))
  expect_equal(density(dist, 1), stats::dbinom(1, 1, 0.4))

  # cdf
  expect_equal(cdf(dist, 0), stats::pbinom(0, 1, 0.4))
  expect_equal(cdf(dist, 1), stats::pbinom(1, 1, 0.4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.6)), 0.6, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.4)
  expect_equal(variance(dist), 0.4*(1-0.4))
})

test_that("Bernoulli distribution is vectorised over distributions", {
  dist <- dist_bernoulli(c(0, 0.3, 1))
  n <- vctrs::vec_size(dist)

  # support() has one region per distribution
  sup <- support(dist)
  expect_equal(vctrs::vec_size(sup), n)
  expect_equal(vctrs::field(sup, "lim"), list(c(FALSE, FALSE), c(FALSE, TRUE), c(TRUE, TRUE)))
  expect_equal(vctrs::field(sup, "closed"), rep(list(c(TRUE, TRUE)), 3))

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
