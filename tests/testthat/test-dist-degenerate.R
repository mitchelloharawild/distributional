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

test_that("Degenerate distribution is vectorised over distributions", {
  dist <- dist_degenerate(c(1, 2, 3))

  expect_equal(format(dist), c("1", "2", "3"))
  expect_equal(quantile(dist, 0.5), c(1, 2, 3))
  expect_equal(quantile(dist, c(0.2, 2)), list(c(1, NaN), c(2, NaN), c(3, NaN)))
  expect_equal(quantile(dist, 0.5, tail = "upper"), c(1, 2, 3))
  expect_equal(density(dist, 2), c(0, 1, 0))
  expect_equal(density(dist, list(c(1, 1, 3)))[[1]], c(1, 0, 1))
  expect_equal(cdf(dist, 2), c(1, 1, 0))
  expect_equal(cdf(dist, 2, tail = "upper"), c(0, 0, 1))
  expect_equal(mean(dist), c(1, 2, 3))
  expect_equal(variance(dist), c(0, 0, 0))
  expect_equal(skewness(dist), rep(NA_real_, 3))
  expect_equal(kurtosis(dist), rep(NA_real_, 3))
  expect_equal(median(dist), c(1, 2, 3))

  gen <- generate(dist, 2)
  expect_equal(gen, list(c(1, 1), c(2, 2), c(3, 3)))

  supp <- support(dist)
  expect_equal(vctrs::vec_size(supp), 3L)
  expect_equal(vctrs::vec_proxy(support(dist)[2]), vctrs::vec_proxy(support(dist_degenerate(2))))
})

test_that("generate.dist_degenerate() recycles a scalar `times`", {
  x <- dist_degenerate(1:3)
  expect_equal(generate(x@x[[1]], times = 2), list(c(1L, 1L), c(2L, 2L), c(3L, 3L)))
  expect_equal(generate(x@x[[1]], times = c(1, 0, 2)), list(1L, integer(), c(3L, 3L)))
  expect_equal(generate(x, 2), generate(x@x[[1]], times = 2))
})
