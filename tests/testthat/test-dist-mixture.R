test_that("quantile(<dist_mixture>)", {
  x <- dist_mixture(dist_degenerate(1), dist_degenerate(2), dist_degenerate(3), weights = c(0.1, 0.2, 0.7))

  expect_equal(mean(x), 2.6)
  expect_equal(density(x, 1:3)[[1]], c(0.1, 0.2, 0.7))
  expect_equal(cdf(x, 1:3)[[1]], c(0.1, 0.3, 1))
  expect_equal(quantile(x, c(0, 0.1, 0.3, 1))[[1]], c(1, 1:3), tolerance = .Machine$double.eps^0.25)
})
