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
    quantile(dist, 0.1, kind = "marginal"),
    matrix(c(qnorm(0.1, mu[1], sqrt(sigma[1,1])), qnorm(0.1, mu[2], sqrt(sigma[2,2]))),
           nrow = 1, dimnames = list(NULL, c("a", "b")))
  )

  skip_if_not_installed("mvtnorm")
  expect_equivalent(
    quantile(dist, 0.1, kind = "equicoordinate"),
    rep(mvtnorm::qmvnorm(0.1, mean = mu, sigma = sigma)$quantile, 2)
  )

  # pdf
  expect_equal(density(dist, cbind(1, 2)), mvtnorm::dmvnorm(c(1, 2), mean = mu, sigma = sigma))
  expect_equal(density(dist, cbind(-3, 4)), mvtnorm::dmvnorm(c(-3, 4), mean = mu, sigma = sigma))

  # cdf
  expect_equivalent(cdf(dist, cbind(1, 2)), mvtnorm::pmvnorm(upper = c(1,2), mean = mu, sigma = sigma))
  expect_equivalent(cdf(dist, cbind(-3, 4)), mvtnorm::pmvnorm(upper = c(-3, 4), mean = mu, sigma = sigma))

  # F(Finv(a)) ~= a
  # expect_equal(cdf(dist, list(as.numeric(quantile(dist, 0.53)))), 0.53, tolerance = 1e-3)
})

test_that("density() works with named multivariate normal parameters (#150)", {
  skip_if_not_installed("mvtnorm")
  dist <- dist_multivariate_normal(
    list(c(v1 = 0, v2 = 0)),
    list(rbind(v1 = c(v1 = 1, v2 = 0), v2 = c(v1 = 0, v2 = 1)))
  )
  expect_equal(
    density(dist, at = cbind(c(0, 1), c(1, 1))),
    list(mvtnorm::dmvnorm(cbind(c(0, 1), c(1, 1)), mean = c(0, 0), sigma = diag(2)))
  )
})

test_that("Multivariate normal methods are vectorised over distributions", {
  skip_if_not_installed("mvtnorm")
  mu <- list(c(0, 0), c(1, 1), c(2, 0))
  sigma <- list(diag(2), 2 * diag(2), matrix(c(1, .5, .5, 1), 2))
  dist <- dist_multivariate_normal(mu, sigma)

  expect_equal(format(dist), rep("MVN[2]", 3))
  expect_equal(mean(dist), do.call(rbind, mu))
  expect_equal(variance(dist), do.call(rbind, lapply(sigma, diag)))
  expect_equal(covariance(dist), sigma)
  expect_equal(quantile(dist, 0.25), rbind(
    qnorm(.25, c(0, 0), 1), qnorm(.25, c(1, 1), sqrt(2)), qnorm(.25, c(2, 0), 1)
  ))

  # Paired rows, and a single row shared by every distribution
  at <- rbind(c(0, 0), c(1, 0), c(2, 2))
  expect_equal(
    cdf(dist, list(at))[[1]],
    c(mvtnorm::pmvnorm(upper = at[1, ], mean = mu[[1]], sigma = sigma[[1]]),
      mvtnorm::pmvnorm(upper = at[2, ], mean = mu[[2]], sigma = sigma[[2]]),
      mvtnorm::pmvnorm(upper = at[3, ], mean = mu[[3]], sigma = sigma[[3]])),
    tolerance = 1e-4
  )
  expect_equal(
    density(dist, list(at))[[1]],
    vapply(1:3, function(i) mvtnorm::dmvnorm(at[i, ], mu[[i]], sigma[[i]]), 1)
  )
  expect_equal(
    density(dist, cbind(1, 0)),
    vapply(1:3, function(i) mvtnorm::dmvnorm(c(1, 0), mu[[i]], sigma[[i]]), 1)
  )

  # Draws are generated per distribution
  draws <- generate(dist, c(2, 3, 4))
  expect_equal(vapply(draws, nrow, 1L), c(2L, 3L, 4L))
  expect_equal(vapply(draws, ncol, 1L), c(2L, 2L, 2L))
})

test_that("Multivariate normal distributions can differ in dimensionality", {
  dist <- dist_multivariate_normal(
    list(c(0, 0), c(1, 2, 3)), list(diag(2), 2 * diag(3))
  )
  expect_equal(format(dist), c("MVN[2]", "MVN[3]"))
  # Rows of lower dimensional distributions are padded with NA
  expect_equal(mean(dist), rbind(c(0, 0, NA), c(1, 2, 3)))
  expect_equal(variance(dist), rbind(c(1, 1, NA), c(2, 2, 2)))
  expect_equal(covariance(dist), list(diag(2), 2 * diag(3)))
  expect_equal(quantile(dist, 0.5), rbind(c(0, 0, NA), c(1, 2, 3)))
  expect_equal(vapply(generate(dist, c(2, 5)), ncol, 1L), c(2L, 3L))
  expect_equal(vec_size(support(dist)), 2L)
})

test_that("quantiles of mixed univariate and multivariate vectors are not padded", {
  x <- c(dist_multivariate_normal(list(1:3), list(diag(3))), dist_normal())
  q <- quantile(x, c(.25, .75))
  expect_length(q, 2L)
  expect_equal(dim(q[[1]]), c(2L, 3L))
  expect_equal(q[[2]], quantile(x[2], c(.25, .75))[[1]])
})

test_that("moments of mixed univariate and multivariate vectors are not padded", {
  x <- c(dist_multivariate_normal(list(1:3), list(diag(3))), dist_normal())
  expect_equal(median(x), list(matrix(1:3, 1), 0))
  expect_equal(mean(x), list(matrix(1:3, 1), 0))
  expect_equal(variance(x)[[2]], 1)
  expect_equal(quantile(x, .5), list(matrix(1:3, 1), 0))
})
