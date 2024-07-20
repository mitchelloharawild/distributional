test_that("Categorical distribution", {
  dist <- dist_categorical(list(c(0.4, 0.2, 0.3, 0.1)))

  expect_equal(format(dist), "Categorical[4]")

  # quantiles
  expect_true(all(is.na(quantile(dist, 0.5))))
  expect_true(all(is.na(quantile(dist, 0.2))))

  # pdf
  expect_equal(density(dist, -1), NA_real_)
  expect_equal(density(dist, 0), NA_real_)
  expect_equal(density(dist, 1), 0.4)
  expect_equal(density(dist, 2), 0.2)
  expect_equal(density(dist, 5), NA_real_)

  # cdf
  expect_true(all(is.na(cdf(dist, 1))))

  # stats
  expect_true(all(is.na(mean(dist))))
  expect_true(all(is.na(variance(dist))))
})
