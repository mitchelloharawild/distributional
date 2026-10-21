test_that("Cauchy distribution", {
  dist <- dist_cauchy(-2, 1)

  expect_equal(format(dist), "Cauchy(-2, 1)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qcauchy(0.1, -2, 1))
  expect_equal(quantile(dist, 0.5), stats::qcauchy(0.5, -2, 1))

  # pdf
  expect_equal(density(dist, 0), stats::dcauchy(0, -2, 1))
  expect_equal(density(dist, 3), stats::dcauchy(3, -2, 1))

  # cdf
  expect_equal(cdf(dist, 0), stats::pcauchy(0, -2, 1))
  expect_equal(cdf(dist, 3), stats::pcauchy(3, -2, 1))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), NA_real_)
  expect_equal(variance(dist), NA_real_)
})

test_that("Cauchy vectorised over several distributions", {
  dist <- dist_cauchy(c(0, 1, 2), c(1, 2, 3))
  expect_equal(mean(dist), rep(NA_real_, 3))
  expect_equal(variance(dist), rep(NA_real_, 3))
  expect_equal(skewness(dist), rep(NA_real_, 3))
  expect_equal(density(dist, 1), stats::dcauchy(1, c(0, 1, 2), c(1, 2, 3)))
  expect_equal(log_density(dist, 1), stats::dcauchy(1, c(0, 1, 2), c(1, 2, 3), log = TRUE))
  expect_equal(quantile(dist, 0.3), stats::qcauchy(0.3, c(0, 1, 2), c(1, 2, 3)))
  expect_equal(cdf(dist, 1), stats::pcauchy(1, c(0, 1, 2), c(1, 2, 3)))
  expect_equal(vctrs::vec_size(support(dist)), 3L)
  expect_length(format(dist), 3L)
  set.seed(1)
  expect_equal(lengths(generate(dist, list(c(1, 2, 3)))), c(1, 2, 3))
})
