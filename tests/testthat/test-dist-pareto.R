test_that("Pareto distribution", {
  dist <- dist_pareto(10, 1)

  expect_equal(format(dist), "Pareto(10, 1)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qpareto(0.1, 10, 1))
  expect_equal(quantile(dist, 0.5), actuar::qpareto(0.5, 10, 1))

  # pdf
  expect_equal(density(dist, 0), actuar::dpareto(0, 10, 1))
  expect_equal(density(dist, 3), actuar::dpareto(3, 10, 1))

  # cdf
  expect_equal(cdf(dist, 0), actuar::ppareto(0, 10, 1))
  expect_equal(cdf(dist, 3), actuar::ppareto(3, 10, 1))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), actuar::mpareto(1, 10, 1))
  expect_equal(variance(dist), actuar::mpareto(2, 10, 1) - actuar::mpareto(1, 10, 1)^2)
})

test_that("Pareto distribution is vectorised over several distributions", {
  dist <- dist_pareto(c(3, 4, 5), c(1, 2, 3))
  n <- 3
  at <- c(3, 4, 5)

  expect_equal(density(dist, list(at))[[1]], actuar::dpareto(at, c(3, 4, 5), c(1, 2, 3)))
  expect_equal(cdf(dist, list(at))[[1]], actuar::ppareto(at, c(3, 4, 5), c(1, 2, 3)))
  expect_equal(quantile(dist, 0.3), actuar::qpareto(0.3, c(3, 4, 5), c(1, 2, 3)))
  expect_equal(log_density(dist, list(at))[[1]], actuar::dpareto(at, c(3, 4, 5), c(1, 2, 3), log = TRUE))

  # support has one region per distribution
  expect_equal(vec_size(support(dist)), n)
  expect_equal(mean(dist), vapply(vec_chop(dist), mean, 1))

  # generate() returns one draw vector per distribution
  set.seed(1)
  draws <- generate(dist, list(c(2L, 0L, 5L)))
  expect_equal(lengths(draws), c(2L, 0L, 5L))
  expect_true(all(unlist(draws) >= 0))
})
