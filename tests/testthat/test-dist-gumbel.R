test_that("Gumbel distribution", {
  dist <- dist_gumbel(1, 2)

  expect_equal(format(dist), "Gumbel(1, 2)")

  # quantiles
  expect_equal(quantile(dist, 0.1), -0.668064890495911) # dput(actuar::qgumbel(0.1, 1, 2))
  expect_equal(quantile(dist, 0.5), 1.73302584116333) # dput(actuar::qgumbel(0.5, 1, 2))

  # pdf
  expect_equal(density(dist, 0), 0.158520960538971) # dput(actuar::dgumbel(0, 1, 2))
  expect_equal(density(dist, 3), 0.127323190021791) # dput(actuar::dgumbel(3, 1, 2))

  # cdf
  expect_equal(cdf(dist, 0), 0.192295645547965) # dput(actuar::pgumbel(0, 1, 2))
  expect_equal(cdf(dist, 3), 0.692200627555346) # dput(actuar::pgumbel(3, 1, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 2.15443132980307) # dput(actuar::mgumbel(1, 1, 2))
  expect_equal(variance(dist), 6.57973626739291) # dput(actuar::mgumbel(2, 1, 2) - actuar::mgumbel(1, 1, 2)^2)
})
