test_that("Inverse Exponential distribution", {
  dist <- dist_inverse_exponential(5)

  expect_equal(format(dist), "InvExp(5)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qinvexp(0.1, 5))
  expect_equal(quantile(dist, 0.5), actuar::qinvexp(0.5, 5))

  # pdf
  expect_equal(density(dist, 0), actuar::dinvexp(0, 5))
  expect_equal(density(dist, 3), actuar::dinvexp(3, 5))

  # cdf
  expect_equal(cdf(dist, 0), actuar::pinvexp(0, 5))
  expect_equal(cdf(dist, 3), actuar::pinvexp(3, 5))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), NA_real_) # dput(actuar::minvexp(1, 5))
  expect_equal(variance(dist), NA_real_) # dput(actuar::minvexp(2, 5) - actuar::minvexp(1, 5)^2)
})

test_that("Inverse Exponential vectorised over several distributions", {
  skip_if_not_installed("actuar")
  dist <- dist_inverse_exponential(c(1, 2, 3))
  expect_equal(mean(dist), rep(NA_real_, 3))
  expect_equal(variance(dist), rep(NA_real_, 3))
  expect_equal(density(dist, 2), actuar::dinvexp(2, c(1, 2, 3)))
  expect_equal(log_density(dist, 2), actuar::dinvexp(2, c(1, 2, 3), log = TRUE))
  expect_equal(cdf(dist, 2), actuar::pinvexp(2, c(1, 2, 3)))
  expect_equal(quantile(dist, 0.3), actuar::qinvexp(0.3, c(1, 2, 3)))
  expect_equal(vctrs::vec_size(support(dist)), 3L)
  expect_length(format(dist), 3L)
  set.seed(1)
  expect_equal(lengths(generate(dist, list(c(1, 2, 3)))), c(1, 2, 3))
})
