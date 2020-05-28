test_that("Negative Binomial distribution", {
  dist <- dist_negative_binomial(10, 0.4)

  expect_equal(format(dist), "NB(10, 0.4)")

  # quantiles
  expect_equal(quantile(dist, 0.6), stats::qnbinom(0.6, 10, 0.4))
  expect_equal(quantile(dist, 0.61), stats::qnbinom(0.61, 10, 0.4))

  # pdf
  expect_equal(density(dist, 0), stats::dnbinom(0, 10, 0.4))
  expect_equal(density(dist, 1), stats::dnbinom(1, 10, 0.4))

  # cdf
  expect_equal(cdf(dist, 0), stats::pnbinom(0, 10, 0.4))
  expect_equal(cdf(dist, 1), stats::pnbinom(1, 10, 0.4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.6358)), 0.6358, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.6*10/(1-0.6))
  expect_equal(variance(dist), 0.6*10/(1-0.6)^2)
})
