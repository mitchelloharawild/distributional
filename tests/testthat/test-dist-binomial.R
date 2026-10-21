

test_that("Binomial distribution is vectorised over distributions", {
  dist <- dist_binomial(5:7, c(0, 0.4, 1))
  n <- vctrs::vec_size(dist)

  # support() has one region per distribution
  sup <- support(dist)
  expect_equal(vctrs::vec_size(sup), n)
  expect_equal(vctrs::field(sup, "lim"), list(c(0L, 0L), c(0L, 6L), c(7L, 7L)))
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
