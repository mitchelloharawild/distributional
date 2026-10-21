test_that("Negative Binomial distribution", {
  dist <- dist_normal(0, 1)
  percentiles <- seq(0.01, 0.99, by = 0.01)
  x <- vapply(percentiles, quantile, double(1L), x = dist)
  dist <- dist_percentile(list(x), list(percentiles*100))

  expect_equal(format(dist), "quantile[99]")

  # quantiles
  expect_equal(quantile(dist, 0.6), stats::qnorm(0.6, 0, 1))
  expect_equal(quantile(dist, 0.61), stats::qnorm(0.61, 0, 1))

  # pdf

  # cdf
  expect_equal(cdf(dist, 0), stats::pnorm(0, 0, 1))
  expect_equal(cdf(dist, 1), stats::pnorm(1, 0, 1), tolerance = 1e-3)

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.6)), 0.6, tolerance = 1e-3)

  # stats
})

test_that("Percentile distributions with grids of different lengths", {
  p1 <- seq(1, 99, by = 1)
  dist <- dist_percentile(
    list(stats::qnorm(p1 / 100), c(0, 1, 2)), list(p1, c(0, 50, 100))
  )

  expect_equal(format(dist), c("quantile[99]", "quantile[3]"))
  expect_equal(quantile(dist, list(c(0.6, 0.25)))[[1]], c(stats::qnorm(0.6), 0.5))
  expect_equal(cdf(dist, list(c(0, 1.5)))[[1]], c(0.5, 0.75))
  expect_equal(cdf(dist, 1, tail = "upper"), c(1 - stats::pnorm(1), 0.5), tolerance = 1e-3)
  expect_equal(mean(dist)[2], 1)
  expect_equal(mean(dist[1]), mean(dist)[1])

  set.seed(1)
  g <- generate(dist, list(c(2, 6)))
  expect_equal(lengths(g), c(2, 6))
  expect_true(all(g[[2]] >= 0 & g[[2]] <= 2))
})
