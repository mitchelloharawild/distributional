test_that("Multivariate normal distribution", {
  mu <- c(1,2)
  sigma <- matrix(c(4,2,2,3), ncol=2)
  dist <- dist_multivariate_normal(mu = list(mu), sigma = list(sigma))
  dimnames(dist) <- c("a", "b")

  expect_equal(format(dist), "MVN[2]")

  # stats
  expect_equal(
    mean(dist),
    matrix(c(1,2), nrow = 1, dimnames = list(NULL, c("a", "b")))
  )
  expect_equal(covariance(dist), list(`colnames<-`(sigma, dimnames(dist))))

  # quantiles
  expect_equal(
    quantile(dist, 0.1),
    matrix(c(qnorm(0.1, mu[1], sqrt(sigma[1,1])), qnorm(0.1, mu[2], sqrt(sigma[2,2]))),
           nrow = 1, dimnames = list(NULL, c("a", "b")))
  )

  skip_if_not_installed("mvtnorm")
  expect_equivalent(quantile(dist, 0.1, type = "equicoordinate"), mvtnorm::qmvnorm(0.1, mean = mu, sigma = sigma)$quantile)

  # pdf
  expect_equal(density(dist, cbind(1, 2)), mvtnorm::dmvnorm(c(1, 2), mean = mu, sigma = sigma))
  expect_equal(density(dist, cbind(-3, 4)), mvtnorm::dmvnorm(c(-3, 4), mean = mu, sigma = sigma))

  # cdf
  expect_equivalent(cdf(dist, cbind(1, 2)), mvtnorm::pmvnorm(upper = c(1,2), mean = mu, sigma = sigma))
  expect_equivalent(cdf(dist, cbind(-3, 4)), mvtnorm::pmvnorm(c(-3, 4), mean = mu, sigma = sigma))

  # F(Finv(a)) ~= a
  # expect_equal(cdf(dist, list(as.numeric(quantile(dist, 0.53)))), 0.53, tolerance = 1e-3)
})
