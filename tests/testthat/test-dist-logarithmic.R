test_that("Logarithmic distribution", {
  dist <- dist_logarithmic(0.66)

  expect_equal(format(dist), "Logarithmic(0.66)")

  # quantiles
  expect_equal(quantile(dist, 0.5), 1) # dput(actuar::qlogarithmic(1, 0.66))
  expect_equal(quantile(dist, 0.99), 8) # dput(actuar::qlogarithmic(5, 0.66))

  # pdf
  expect_equal(density(dist, 1), 0.611785399808779) # dput(actuar::dlogarithmic(1, 0.66))
  expect_equal(density(dist, 9), 0.00244741762504557) # dput(actuar::dlogarithmic(9, 0.66))

  # cdf
  expect_equal(cdf(dist, 3), 0.902505821797911) # dput(actuar::plogarithmic(3, 0.66))
  expect_equal(cdf(dist, 12), 0.999160209462647) # dput(actuar::plogarithmic(12, 0.66))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.9963064)), 0.9963064, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), -1/log(1-0.66)*(0.66/(1-0.66)))
  expect_equal(variance(dist), -(0.66^2 + 0.66*log(1-0.66))/((1-0.66)^2*log(1-0.66)^2))
})
