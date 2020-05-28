test_that("Bernoulli distribution", {
  dist <- dist_bernoulli(0.4)

  expect_equal(format(dist), "Bernoulli(0.4)")

  # quantiles
  expect_equal(quantile(dist, 0.6), stats::qbinom(0.6, 1, 0.4))
  expect_equal(quantile(dist, 0.61), stats::qbinom(0.61, 1, 0.4))

  # pdf
  expect_equal(density(dist, 0), stats::dbinom(0, 1, 0.4))
  expect_equal(density(dist, 1), stats::dbinom(1, 1, 0.4))

  # cdf
  expect_equal(cdf(dist, 0), stats::pbinom(0, 1, 0.4))
  expect_equal(cdf(dist, 1), stats::pbinom(1, 1, 0.4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.6)), 0.6, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.4)
  expect_equal(variance(dist), 0.4*(1-0.4))
})
