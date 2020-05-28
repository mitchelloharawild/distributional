test_that("Degenerate distribution", {
  dist <- dist_degenerate(1)

  expect_equal(
    dist,
    vec_cast(1, new_dist())
  )

  expect_equal(format(dist), "1")

  # quantiles
  expect_equal(quantile(dist, 0), 1)
  expect_equal(quantile(dist, 0.5), 1)
  expect_equal(quantile(dist, 1), 1)

  # pdf
  expect_equal(density(dist, 1), 1)
  expect_equal(density(dist, 0.5), 0)
  expect_equal(density(dist, 0.99999), 0)

  # cdf
  expect_equal(cdf(dist, 0), 0)
  expect_equal(cdf(dist, 1), 1)
  expect_equal(cdf(dist, 0.9999), 0)

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 1)), 1, tolerance = 1e-3)
  expect_equal(cdf(dist, quantile(dist, 0)), 1, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 1)
  expect_equal(variance(dist), 0)
})
