test_that("Mixture of Normals", {
  dist <- dist_mixture(dist_normal(0, 1), dist_normal(10, 4), weights = c(0.5, 0.5))

  # format
  expect_equal(format(dist), "mixture(0.5*N(0, 1), 0.5*N(10, 16))")

  # quantiles
  expect_equal(quantile(dist, 0.5), 2, tolerance = 1e-5)
  expect_equal(quantile(dist, 0.1), -0.854, tolerance = 1e-3)

  # pdf
  expect_equal(density(dist, 0), 0.5 * dnorm(0) + 0.5 * dnorm(0, 10, 4))
  expect_equal(density(dist, 3), 0.5 * dnorm(3) + 0.5 * dnorm(3, 10, 4))

  # cdf
  expect_equal(cdf(dist, 0), 0.5 * pnorm(0) + 0.5 * pnorm(0, 10, 4))
  expect_equal(cdf(dist, 3), 0.5 * pnorm(3) + 0.5 * pnorm(3, 10, 4))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.5)), 0.5, tolerance = 1e-6)

  expect_equal(mean(dist), 5)
  expect_equal(variance(dist), 33.5)
})

test_that("Mixture of different distributions", {
  dist <- dist_mixture(dist_normal(0, 1), dist_student_t(10), weights = c(0.3, 0.7))

  # format
  expect_equal(format(dist), "mixture(0.3*N(0, 1), 0.7*t(10, 0, 1))")

  # quantiles
  expect_equal(quantile(dist, 0.5), 0, tolerance = 1e-5)
  expect_equal(quantile(dist, 0.1), -1.343, tolerance = 1e-3)

  # pdf
  expect_equal(density(dist, 0), 0.3 * dnorm(0) + 0.7 * dt(0, 10))
  expect_equal(density(dist, 3), 0.3 * dnorm(3) + 0.7 * dt(3, 10))

  # cdf
  expect_equal(cdf(dist, 0), 0.3 * pnorm(0) + 0.7 * pt(0, 10))
  expect_equal(cdf(dist, 3), 0.3 * pnorm(3) + 0.7 * pt(3, 10))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.5)), 0.5, tolerance = 1e-6)

  expect_equal(mean(dist), 0)
  expect_equal(variance(dist), 1.175)
})

test_that("Mixture of point masses", {
  dist <- dist_mixture(dist_degenerate(1), dist_degenerate(2), dist_degenerate(3), weights = c(0.1, 0.2, 0.7))

  # format
  expect_equal(format(dist, width = 10), "mixture(n=3)")
  expect_equal(format(dist), "mixture(0.1*1, 0.2*2, 0.7*3)")

  # quantiles
  expect_equal(quantile(dist, c(0, 0.1, 0.3, 1))[[1]], c(1, 1:3), tolerance = .Machine$double.eps^0.25)

  # pmf
  expect_equal(density(dist, 1:3)[[1]], c(0.1, 0.2, 0.7))

  # cdf
  expect_equal(cdf(dist, 1:3)[[1]], c(0.1, 0.3, 1))

  # mean
  expect_equal(mean(dist), 2.6)
})

test_that("Mixture of multivariate distributions", {
  mu1 <- c(0, 0)
  mu2 <- c(1, 2)
  mu3 <- c(-10, -10)
  mu4 <- c(10, 10)
  sigma1 <- diag(2)
  sigma2 <- matrix(c(4, 2, 2, 3), 2, 2)
  w1 <- 0.3
  w2 <- 1 - w1
  w3 <- 0.5
  w4 <- 1 - w3
  # Two mixtures of bivariate normals
  dist <- c(
    dist1 = dist_mixture(
      dist_multivariate_normal(mu = list(mu1), sigma = list(sigma1)),
      dist_multivariate_normal(mu = list(mu2), sigma = list(sigma2)),
      weights = c(w1, w2)
    ),
    dist2 = dist_mixture(
      dist_multivariate_normal(mu = list(mu3), sigma = list(sigma1)),
      dist_multivariate_normal(mu = list(mu4), sigma = list(sigma1)),
      weights = c(w3, w4)
    )
  )
  # Mean
  expect_equal(mean(dist), rbind(w1 * mu1 + w2 * mu2, w3 * mu3 + w4 * mu4))
  # Quantile
  expect_error(quantile(dist, 0.5))
  # CDF
  at <- matrix(rnorm(4), 2, 2)
  cdf_dist <- cdf(dist, q = at)
  expect_equal(length(cdf_dist), 2L)
  expect_equal(lengths(cdf_dist), c(2L, 2L))
  # Density
  skip_if_not_installed("mvtnorm")
  expect_equal(
    density(dist, at),
    list(
      c(w1 * mvtnorm::dmvnorm(at, mean = mu1, sigma = sigma1) +
        w2 * mvtnorm::dmvnorm(at, mean = mu2, sigma = sigma2)),
      c(w3 * mvtnorm::dmvnorm(at, mean = mu3, sigma = sigma1) +
        w4 * mvtnorm::dmvnorm(at, mean = mu4, sigma = sigma1))
    )
  )
  # Mixture equivalent to multivariate normal
  dist <- dist_multivariate_normal(mu = list(mu2), sigma = list(sigma2))
  mdist <- dist_mixture(dist, dist, weights = c(w1, w2))
  expect_equal(mean(dist), mean(mdist))
  expect_equal(covariance(dist), covariance(mdist))
  expect_equal(unname(density(dist, rbind(mu1))), density(mdist, rbind(mu1)))
  set.seed(1)
  expect_equal(cdf(dist, rbind(c(0, 0))), cdf(mdist, rbind(c(0, 0))),
    tolerance = 0.001
  )
})
