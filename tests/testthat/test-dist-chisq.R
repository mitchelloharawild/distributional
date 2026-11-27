test_that("Chisq distribution", {
  dist <- dist_chisq(9)

  expect_equal(format(dist), "x2(9)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qchisq(0.1, 9))
  expect_equal(quantile(dist, 0.5), stats::qchisq(0.5, 9))

  # pdf
  expect_equal(density(dist, 0), stats::dchisq(0, 9))
  expect_equal(density(dist, 3), stats::dchisq(3, 9))

  # cdf
  expect_equal(cdf(dist, 0), stats::pchisq(0, 9))
  expect_equal(cdf(dist, 3), stats::pchisq(3, 9))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 9)
  expect_equal(variance(dist), 2*9)
})

test_that("Non-central Chisq skewness and kurtosis use ncp", {
  dist <- dist_chisq(c(5, 5), ncp = c(0, 2))
  mu <- function(k, ncp) {
    integrate(function(x) (x - 5 - ncp)^k * stats::dchisq(x, 5, ncp), 0, Inf)$value
  }
  expect_equal(skewness(dist), c(sqrt(8 / 5), mu(3, 2) / mu(2, 2)^1.5), tolerance = 1e-6)
  expect_equal(kurtosis(dist), c(12 / 5, mu(4, 2) / mu(2, 2)^2 - 3), tolerance = 1e-6)
})

test_that("Chisq distribution is vectorised over distributions", {
  dist <- dist_chisq(c(1, 3, 5))
  n <- vctrs::vec_size(dist)

  # support() has one region per distribution
  sup <- support(dist)
  expect_equal(vctrs::vec_size(sup), n)
  expect_equal(vctrs::field(sup, "lim"), rep(list(c(0, Inf)), 3))
  expect_equal(vctrs::field(sup, "closed"), list(c(TRUE, FALSE), c(FALSE, FALSE), c(FALSE, FALSE)))

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
