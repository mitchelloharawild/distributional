test_that("Inverse Gamma distribution", {
  dist <- dist_inverse_gamma(3, 2)

  expect_equal(format(dist), "InvGamma(3, 0.5)")

  # Require package installed
  skip_if_not_installed("actuar", "2.0.0")

  # quantiles
  expect_equal(quantile(dist, 0.1), actuar::qinvgamma(0.1, 3, 2))
  expect_equal(quantile(dist, 0.5), actuar::qinvgamma(0.5, 3, 2))

  # pdf
  expect_equal(density(dist, 0), actuar::dinvgamma(0, 3, 2))
  expect_equal(density(dist, 3), actuar::dinvgamma(3, 3, 2))

  # cdf
  expect_equal(cdf(dist, 0), actuar::pinvgamma(0, 3, 2))
  expect_equal(cdf(dist, 3), actuar::pinvgamma(3, 3, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), (1/2) / (3 - 1))
  expect_equal(median(dist), actuar::qinvgamma(0.5, 3, 2))
  expect_equal(median(dist[[1]]), actuar::qinvgamma(0.5, 3, 2))
  expect_equal(variance(dist), (1/2)^2/((3-1)^2*(3-2)))
})

test_that("Inverse Gamma vectorised over several distributions", {
  skip_if_not_installed("actuar")
  dist <- dist_inverse_gamma(c(1, 2, 3, 4), c(1, 2, 3, 4))
  expect_equal(mean(dist), c(NA, 1/(2*1), 1/(3*2), 1/(4*3)))
  expect_equal(variance(dist), c(NA, NA, 1/(9*4*1), 1/(16*9*2)))
  expect_equal(density(dist, 1), actuar::dinvgamma(1, c(1, 2, 3, 4), c(1, 2, 3, 4)))
  expect_equal(log_density(dist, 1), actuar::dinvgamma(1, c(1, 2, 3, 4), c(1, 2, 3, 4), log = TRUE))
  expect_equal(cdf(dist, 1), actuar::pinvgamma(1, c(1, 2, 3, 4), c(1, 2, 3, 4)))
  expect_equal(quantile(dist, 0.3), actuar::qinvgamma(0.3, c(1, 2, 3, 4), c(1, 2, 3, 4)))
  expect_equal(vctrs::vec_size(support(dist)), 4L)
  expect_length(format(dist), 4L)
  set.seed(1)
  expect_equal(lengths(generate(dist, list(1:4))), 1:4)
})
