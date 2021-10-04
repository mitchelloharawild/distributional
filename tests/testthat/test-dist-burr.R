test_that("Burr distribution", {

  dist <- dist_burr(2, 3)

  expect_equal(format(dist), "Burr12(2, 3, 1)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qburr(0.1, 2, 3))
  expect_equal(quantile(dist, 0.5), actuar::qburr(0.5, 2, 3))

  # pdf
  expect_equal(density(dist, 0), actuar::dburr(0, 2, 3))
  expect_equal(density(dist, 3), actuar::dburr(3, 2, 3))

  # cdf
  expect_equal(cdf(dist, 0), actuar::pburr(0, 2, 3))
  expect_equal(cdf(dist, 3), actuar::pburr(3, 2, 3))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), actuar::mburr(1, 2, 3))
  expect_equal(variance(dist), actuar::mburr(2, 2, 3) - actuar::mburr(1, 2, 3)^2)
})
