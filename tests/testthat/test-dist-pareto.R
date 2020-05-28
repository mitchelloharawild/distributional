test_that("Pareto distribution", {
  dist <- dist_pareto(10, 1)

  expect_equal(format(dist), "Pareto(10, 1)")

  # quantiles
  expect_equal(quantile(dist, 0.1), 0.0105917512032914) # dput(actuar::qpareto(0.1, 10, 1))
  expect_equal(quantile(dist, 0.5), 0.0717734625362931) # dput(actuar::qpareto(0.5, 10, 1))

  # pdf
  expect_equal(density(dist, 0), 10) # dput(actuar::dpareto(0, 10, 1))
  expect_equal(density(dist, 3), 2.38418579101562e-6) # dput(actuar::dpareto(3, 10, 1))

  # cdf
  expect_equal(cdf(dist, 0), 0) # dput(actuar::ppareto(0, 10, 1))
  expect_equal(cdf(dist, 3), 0.999999046325684) # dput(actuar::ppareto(3, 10, 1))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.111111111111111) # dput(actuar::mpareto(1, 10, 1))
  expect_equal(variance(dist), 0.0154320987654321) # dput(actuar::mpareto(2, 10, 1) - actuar::mpareto(1, 10, 1)^2)
})
