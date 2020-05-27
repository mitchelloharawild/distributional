test_that("Check zero inflation", {
  d <- dist_inflated(dist_poisson(6), 0.33)

  expect_equal(format(d), "0+Pois(6)")

  # quantiles
  expect_equal(quantile(d, 0.1), 0)
  expect_equal(quantile(d, 0.5), 4)

  # pdf
  expect_equal(density(d, 0), 0.33 + 0.67*dpois(0, 6))
  expect_equal(density(d, 3), 0.67*dpois(3, 6))

  # cdf
  expect_equal(cdf(d, 0), 0.33 + 0.67*ppois(0, 6))
  expect_equal(cdf(d, 3), 0.33 + 0.67*ppois(3, 6))

  # stats
  expect_equal(mean(d), 0.67*6)
  expect_equal(variance(d), 0.67*6 + (0.33/0.67)*(0.67*6)^2)
})

test_that("Check non-zero inflation", {
  d <- dist_inflated(dist_poisson(6), 0.33, 2)

  expect_equal(format(d), "2+Pois(6)")

  # quantiles
  expect_equal(quantile(d, 0), 0)
  expect_equal(quantile(d, 0.1), 2)
  expect_equal(quantile(d, 0.33), 2)
  expect_equal(quantile(d, 0.5), 4)

  # pdf
  expect_equal(density(d, 0), 0.67*dpois(0, 6))
  expect_equal(density(d, 2), 0.33 + 0.67*dpois(2, 6))
  expect_equal(density(d, 3), 0.67*dpois(3, 6))

  # cdf
  expect_equal(cdf(d, 0), 0.67*ppois(0, 6))
  expect_equal(cdf(d, 2), 0.33 + 0.67*ppois(2, 6))
  expect_equal(cdf(d, 3), 0.33 + 0.67*ppois(3, 6))

  # stats
  expect_equal(mean(d), 0.33*2 + 0.67*6)
  # expect_equal(variance(d), ???)
})
