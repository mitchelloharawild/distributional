test_that("Emperical/sample distribution", {
  x <- generate(dist_normal(0, 1), 100)
  dist <- dist_sample(x)

  expect_equal(format(dist), "sample[100]")

  # quantiles
  expect_equal(quantile(dist, 0.6), unname(quantile(x[[1]], 0.6)))
  expect_equal(quantile(dist, 0.24), unname(quantile(x[[1]], 0.24)))

  # pdf

  # cdf

  # F(Finv(a)) ~= a

  # stats
  expect_equal(mean(dist), mean(x[[1]]))
  expect_equal(median(dist), median(x[[1]]))
  expect_equal(median(dist[[1]]), median(x[[1]]))
  expect_equal(variance(dist), var(x[[1]]))

  # transform
  expect_equal(
    dist,
    dist + 1 - 1
  )
})

test_that("CDF of degenerate dist_sample() is correct", {
  expect_equal(cdf(dist_sample(list(2)), 2)[[1]], 1)
})
