test_that("Hypergeometric distribution", {
  dist <- dist_hypergeometric(500, 50, 100)

  expect_equal(format(dist), "Hypergeometric(500, 50, 100)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qhyper(0.1, 500, 50, 100))
  expect_equal(quantile(dist, 0.5), stats::qhyper(0.5, 500, 50, 100))

  # pdf
  expect_equal(density(dist, 0), stats::dhyper(0, 500, 50, 100))
  expect_equal(density(dist, 3), stats::dhyper(3, 500, 50, 100))

  # cdf
  expect_equal(cdf(dist, 0), stats::phyper(0, 500, 50, 100))
  expect_equal(cdf(dist, 3), stats::phyper(3, 500, 50, 100))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  p <- 500/(500+50)
  expect_equal(mean(dist), 100*p)
  expect_equal(variance(dist), 100*p*(1-p)*(500+50-100)/(500+50-1))
})
