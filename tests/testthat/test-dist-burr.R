test_that("Burr distribution", {
  dist <- dist_burr(2, 3)

  expect_equal(format(dist), "Burr12(2, 3, 1)")

  # quantiles
  expect_equal(quantile(dist, 0.1), 0.378192136090135) # dput(actuar::qburr(0.1, 2, 3))
  expect_equal(quantile(dist, 0.5), 0.745432124647256) # dput(actuar::qburr(0.5, 2, 3))

  # pdf
  expect_equal(density(dist, 0), 0) # dput(actuar::dburr(0, 2, 3))
  expect_equal(density(dist, 3), 0.00245991253644315) # dput(actuar::dburr(3, 2, 3))

  # cdf
  expect_equal(cdf(dist, 0), 0) # dput(actuar::pburr(0, 2, 3))
  expect_equal(cdf(dist, 3), 0.998724489795918) # dput(actuar::pburr(3, 2, 3))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.806133050770764) # dput(actuar::mburr(1, 2, 3))
  expect_equal(variance(dist), 0.156282555225785) # dput(actuar::mburr(2, 2, 3) - actuar::mburr(1, 2, 3)^2)
})
