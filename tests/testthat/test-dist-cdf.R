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
