test_that("Multivariate normal distribution", {
  mu <- c(1,2)
  sigma <- matrix(c(4,2,2,3), ncol=2)
  dist <- dist_multivariate_normal(mu = list(mu), sigma = list(sigma))
  dimnames(dist) <- c("a", "b")

  expect_equal(format(dist), "MVN[2]")

  # stats
  expect_equal(mean(dist), data.frame(a = mu[1], b = mu[2]))
  expect_equal(variance(dist), list(sigma))

  # quantiles
  expect_equal(quantile(dist, 0.1), data.frame(a=qnorm(0.1, mu[1], sqrt(sigma[1,1])), b=qnorm(0.1, mu[2], sqrt(sigma[2,2]))))

  skip_if_not_installed("mvtnorm")
  expect_equal(quantile(dist, 0.1, type = "equicoordinate"), 0.131134182424683) # dput(mvtnorm::qmvnorm(0.1, mean = mu, sigma = sigma))

  # pdf
  expect_equal(density(dist, list(c(1, 2))), 0.0562697697598191) # dput(mvtnorm::dmvnorm(c(1, 2), mean = mu, sigma = sigma))
  expect_equal(density(dist, list(c(-3, 4))), 0.000139478814272667) # dput(mvtnorm::dmvnorm(c(-3, 4), mean = mu, sigma = sigma))

  # cdf
  expect_equal(cdf(dist, list(c(1, 2))), 0.347956638007652) # dput(mvtnorm::pmvnorm(upper = c(1,2), mean = mu, sigma = sigma))
  expect_equal(cdf(dist, list(c(-3, 4))), 0.124078569373413) # dput(mvtnorm::pmvnorm(c(-3, 4), mean = mu, sigma = sigma))

  # F(Finv(a)) ~= a
  # expect_equal(cdf(dist, list(as.numeric(quantile(dist, 0.53)))), 0.53, tolerance = 1e-3)
})
