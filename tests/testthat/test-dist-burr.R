test_that("Burr distribution", {

  dist <- dist_burr(2, 3)

  expect_equal(format(dist), "Burr12(2, 3, 1)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qburr(0.1, 2, 3))
  expect_equal(quantile(dist, 0.5), actuar::qburr(0.5, 2, 3))

  # pdf
  expect_equal(density(dist, 0), actuar::dburr(0, 2, 3))
  expect_equal(density(dist, 3), actuar::dburr(3, 2, 3))

  # cdf
  expect_equal(cdf(dist, 0), actuar::pburr(0, 2, 3))
  expect_equal(cdf(dist, 3), actuar::pburr(3, 2, 3))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), actuar::mburr(1, 2, 3))
  expect_equal(variance(dist), actuar::mburr(2, 2, 3) - actuar::mburr(1, 2, 3)^2)
})

test_that("Burr distribution is vectorised over distributions", {
  dist <- dist_burr(c(1, 2, 3), c(0.5, 2, 3), 2)
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
  at <- 1
  expect_equal(log_density(dist, at), log(density(dist, at)))
  expect_equal(log_cdf(dist, at), log(cdf(dist, at)))
})
