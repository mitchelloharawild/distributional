test_that("Check zero inflation", {
  dist <- dist_inflated(dist_poisson(6), 0.33)

  expect_equal(format(dist), "0+Pois(6)")

  # quantiles
  expect_equal(quantile(dist, 0.1), 0)
  expect_equal(quantile(dist, 0.5), 4)

  # pdf
  expect_equal(density(dist, 0), 0.33 + 0.67*dpois(0, 6))
  expect_equal(density(dist, 3), 0.67*dpois(3, 6))

  # cdf
  expect_equal(cdf(dist, 0), 0.33 + 0.67*ppois(0, 6))
  expect_equal(cdf(dist, 3), 0.33 + 0.67*ppois(3, 6))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.52)), 0.52, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.67*6)
  expect_equal(variance(dist), 0.67*6 + (0.33/0.67)*(0.67*6)^2)
})

test_that("Check non-zero inflation", {
  dist <- dist_inflated(dist_poisson(6), 0.33, 2)

  expect_equal(format(dist), "2+Pois(6)")

  # quantiles
  expect_equal(quantile(dist, 0), 0)
  expect_equal(quantile(dist, 0.1), 2)
  expect_equal(quantile(dist, 0.33), 2)
  expect_equal(quantile(dist, 0.5), 4)

  # pdf
  expect_equal(density(dist, 0), 0.67*dpois(0, 6))
  expect_equal(density(dist, 2), 0.33 + 0.67*dpois(2, 6))
  expect_equal(density(dist, 3), 0.67*dpois(3, 6))

  # cdf
  expect_equal(cdf(dist, 0), 0.67*ppois(0, 6))
  expect_equal(cdf(dist, 2), 0.33 + 0.67*ppois(2, 6))
  expect_equal(cdf(dist, 3), 0.33 + 0.67*ppois(3, 6))

  # stats
  expect_equal(mean(dist), 0.33*2 + 0.67*6)
  # expect_equal(variance(d), ???)
})
