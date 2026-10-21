test_that("Studentized Range distribution", {
  dist <- dist_studentized_range(6, 5, 1)

  expect_equal(format(dist), "StudentizedRange(6, 5, 1)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qtukey(0.1, 6, 5, 1))
  expect_equal(quantile(dist, 0.5), stats::qtukey(0.5, 6, 5, 1))

  # pdf

  # cdf
  expect_equal(cdf(dist, 0), stats::ptukey(0, 6, 5, 1))
  expect_equal(cdf(dist, 3), stats::ptukey(3, 6, 5, 1))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)
})

test_that("Studentized Range distribution is vectorised over several distributions", {
  dist <- dist_studentized_range(2:4, 10, 1)
  at <- c(1, 2, 3)

  expect_equal(format(dist), c("StudentizedRange(2, 10, 1)", "StudentizedRange(3, 10, 1)", "StudentizedRange(4, 10, 1)"))
  expect_equal(cdf(dist, list(at))[[1]], stats::ptukey(at, 2:4, 10, 1))
  expect_equal(quantile(dist, 0.3), stats::qtukey(0.3, 2:4, 10, 1))
  expect_equal(vec_size(support(dist)), 3L)
  expect_equal(mean(dist), vapply(vec_chop(dist), mean, 1), tolerance = 1e-4)
  expect_equal(variance(dist), vapply(vec_chop(dist), variance, 1), tolerance = 1e-4)

  # generate() returns one draw vector per distribution
  set.seed(1)
  draws <- generate(dist, list(c(2L, 0L, 5L)))
  expect_equal(lengths(draws), c(2L, 0L, 5L))
  set.seed(1)
  u <- stats::runif(7)
  expect_equal(unlist(draws), stats::qtukey(u, rep(2:4, c(2, 0, 5)), 10, 1))
})
