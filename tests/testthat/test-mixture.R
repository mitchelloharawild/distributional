test_that("Mixture of Normals", {
  dist <- dist_mixture(dist_normal(0, 1), dist_normal(10, 4), weights = c(0.5, 0.5))

  # format
  expect_equal(format(dist), "mixture(n=2)")

  # quantiles
  expect_equal(quantile(dist, 0.5), 2, tolerance = 1e-5)
  expect_equal(quantile(dist, 0.1), -0.854, tolerance = 1e-3)

  # pdf
  expect_equal(density(dist, 0), 0.5*dnorm(0) + 0.5*dnorm(0, 10, 4))
  expect_equal(density(dist, 3), 0.5*dnorm(3) + 0.5*dnorm(3, 10, 4))

  # cdf
  expect_equal(cdf(dist, 0), 0.5*pnorm(0) + 0.5*pnorm(0, 10, 4))
  expect_equal(cdf(dist, 3), 0.5*pnorm(3) + 0.5*pnorm(3, 10, 4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.5)), 0.5, tolerance = 1e-6)

  expect_equal(mean(dist), 5)
  expect_equal(variance(dist), 33.5)
})

test_that("Mixture of different distributions", {
  dist <- dist_mixture(dist_normal(0, 1), dist_student_t(10), weights = c(0.3, 0.7))

  # format
  expect_equal(format(dist), "mixture(n=2)")

  # quantiles
  expect_equal(quantile(dist, 0.5), 0, tolerance = 1e-5)
  expect_equal(quantile(dist, 0.1), -1.343, tolerance = 1e-3)

  # pdf
  expect_equal(density(dist, 0), 0.3*dnorm(0) + 0.7*dt(0, 10))
  expect_equal(density(dist, 3), 0.3*dnorm(3) + 0.7*dt(3, 10))

  # cdf
  expect_equal(cdf(dist, 0), 0.3*pnorm(0) + 0.7*pt(0, 10))
  expect_equal(cdf(dist, 3), 0.3*pnorm(3) + 0.7*pt(3, 10))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.5)), 0.5, tolerance = 1e-6)

  expect_equal(mean(dist), 0)
  expect_equal(variance(dist), 1.175)
})

test_that("Mixture of point masses", {
  dist <- dist_mixture(dist_degenerate(1), dist_degenerate(2), dist_degenerate(3), weights = c(0.1, 0.2, 0.7))

  # format
  expect_equal(format(dist), "mixture(n=3)")

  # quantiles
  expect_equal(quantile(dist, c(0, 0.1, 0.3, 1))[[1]], c(1, 1:3), tolerance = .Machine$double.eps^0.25)

  # pmf
  expect_equal(density(dist, 1:3)[[1]], c(0.1, 0.2, 0.7))

  #cdf
  expect_equal(cdf(dist, 1:3)[[1]], c(0.1, 0.3, 1))

  #mean
  expect_equal(mean(dist), 2.6)
})
