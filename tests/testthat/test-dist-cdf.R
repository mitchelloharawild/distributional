test_that("CDF distribution", {
  at <- seq(-3, 3, by = 0.1)
  p <- stats::pnorm(at)

  dist <- dist_cdf(list(at), list(p))

  expect_equal(format(dist), "cdf[61]")

  # cdf
  expect_equal(cdf(dist, 0), 0.5)
  expect_equal(cdf(dist, 1), stats::pnorm(1), tolerance = 1e-3)

  # quantiles
  expect_equal(quantile(dist, 0.6), stats::qnorm(0.6), tolerance = 1e-3)

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.6)), 0.6, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0, tolerance = 1e-3)
})

test_that("dist_cdf() describes the same distribution as dist_quantile()", {
  at <- seq(-3, 3, by = 0.1)
  p <- stats::pnorm(at)

  dist <- dist_cdf(list(at), list(p))
  qdist <- dist_quantile(list(at), list(p))

  # Linear interpolation is its own inverse, so specifying the values at a set
  # of probabilities and the probabilities at a set of values are equivalent.
  expect_equal(cdf(dist, at), cdf(qdist, at))
  expect_equal(quantile(dist, p), quantile(qdist, p))
  expect_equal(mean(dist), mean(qdist))
  expect_equal(support(dist), support(qdist))
})

test_that("CDF distributions with grids of different lengths", {
  x1 <- seq(-3, 3, by = 0.1)
  x2 <- c(0, 1, 2)
  dist <- dist_cdf(list(x1, x2), list(stats::pnorm(x1), c(0, 0.5, 1)))

  expect_equal(format(dist), c("cdf[61]", "cdf[3]"))
  expect_equal(cdf(dist, list(c(0, 1.5)))[[1]], c(0.5, 0.75))
  expect_equal(cdf(dist, 1, tail = "upper"), c(1 - stats::pnorm(1), 0.5))
  expect_equal(quantile(dist, list(c(0.5, 0.25)))[[1]], c(0, 0.5))
  expect_equal(mean(dist), c(0, 1), tolerance = 1e-3)
  expect_equal(mean(dist[2]), mean(dist)[2])

  set.seed(1)
  g <- generate(dist, list(c(5, 2)))
  expect_equal(lengths(g), c(5, 2))
  expect_true(all(g[[2]] >= 0 & g[[2]] <= 2))

  expect_equal(density(dist, 1)[2] > 0, TRUE)
  expect_equal(
    vctrs::field(support(dist), "lim"), list(c(-3, 3), c(0, 2))
  )
})
