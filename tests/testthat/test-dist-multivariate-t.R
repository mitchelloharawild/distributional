test_that("Multivariate t distribution", {
  df <- 5
  mu <- c(1, 2)
  sigma <- matrix(c(4, 2, 2, 3), ncol = 2)
  dist <- dist_multivariate_t(df = df, mu = list(mu), sigma = list(sigma))
  dimnames(dist) <- c("a", "b")

  expect_equal(format(dist), "MVT[2](5)")

  # stats
  expect_equal(
    mean(dist),
    matrix(c(1, 2), nrow = 1, dimnames = list(NULL, c("a", "b")))
  )
  
  # Covariance for multivariate t is df/(df-2) * sigma
  expected_cov <- df / (df - 2) * sigma
  expect_equal(covariance(dist), list(`colnames<-`(expected_cov, dimnames(dist))))

  # quantiles - marginal
  # For marginal t distribution: scale = sqrt(diag(sigma))
  expect_equal(
    quantile(dist, 0.1, kind = "marginal"),
    matrix(c(
      qt(0.1, df) * sqrt(sigma[1, 1]) + mu[1],
      qt(0.1, df) * sqrt(sigma[2, 2]) + mu[2]
    ),
    nrow = 1, dimnames = list(NULL, c("a", "b")))
  )

  skip_if_not_installed("mvtnorm")
  
  # quantiles - equicoordinate
  expect_equivalent(
    quantile(dist, 0.1, kind = "equicoordinate"),
    rep(mvtnorm::qmvt(0.1, delta = mu, sigma = sigma, df = df)$quantile, 2),
    tolerance = 1e-3
  )

  # pdf
  expect_equal(
    density(dist, cbind(1, 2)),
    mvtnorm::dmvt(c(1, 2), delta = mu, sigma = sigma, df = df, log = FALSE)
  )
  expect_equal(
    density(dist, cbind(-3, 4)),
    mvtnorm::dmvt(c(-3, 4), delta = mu, sigma = sigma, df = df, log = FALSE)
  )

  # cdf
  expect_equivalent(
    cdf(dist, cbind(1, 2)),
    as.numeric(mvtnorm::pmvt(upper = c(1, 2), delta = mu, sigma = sigma, df = df)),
    tolerance = 1e-3
  )
  expect_equivalent(
    cdf(dist, cbind(-3, 4)),
    as.numeric(mvtnorm::pmvt(upper = c(-3, 4), delta = mu, sigma = sigma, df = df)),
    tolerance = 1e-3
  )
})

test_that("Multivariate t with df = 1 (Cauchy-like)", {
  df <- 1
  mu <- c(0, 0)
  sigma <- diag(2)
  dist <- dist_multivariate_t(df = df, mu = list(mu), sigma = list(sigma))

  # Mean is undefined for df <= 1
  expect_warning(mean(dist), "undefined")
  expect_true(all(is.na(mean(dist))))
})

test_that("Multivariate t with df = 2", {
  df <- 2
  mu <- c(1, 2)
  sigma <- matrix(c(1, 0.5, 0.5, 1), ncol = 2)
  dist <- dist_multivariate_t(df = df, mu = list(mu), sigma = list(sigma))

  # Mean exists for df > 1
  expect_equal(mean(dist), matrix(mu, nrow = 1))
  
  # Covariance is undefined for df <= 2
  expect_warning(covariance(dist), "undefined")
  expect_true(all(is.na(covariance(dist)[[1]])))
})

test_that("Multivariate t with larger df approaches normal", {
  skip_if_not_installed("mvtnorm")
  
  df <- 100  # Large df
  mu <- c(1, 2)
  sigma <- matrix(c(4, 2, 2, 3), ncol = 2)
  
  dist_t <- dist_multivariate_t(df = df, mu = list(mu), sigma = list(sigma))
  dist_normal <- dist_multivariate_normal(mu = list(mu), sigma = list(sigma))
  
  # Densities should be similar for large df
  test_point <- cbind(1, 2)
  expect_equal(
    density(dist_t, test_point),
    density(dist_normal, test_point),
    tolerance = 0.01
  )
})

test_that("Multivariate t parameter validation", {
  expect_error(
    dist_multivariate_t(df = 0, mu = list(c(0, 0)), sigma = list(diag(2))),
    "positive"
  )
  
  expect_error(
    dist_multivariate_t(df = -5, mu = list(c(0, 0)), sigma = list(diag(2))),
    "positive"
  )
  
  # Valid inputs should not error
  expect_silent(
    dist_multivariate_t(df = 5, mu = list(c(0, 0)), sigma = list(diag(2)))
  )
})

test_that("Multivariate t generation", {
  skip_if_not_installed("mvtnorm")
  
  df <- 5
  mu <- c(1, 2)
  sigma <- matrix(c(4, 2, 2, 3), ncol = 2)
  dist <- dist_multivariate_t(df = df, mu = list(mu), sigma = list(sigma))
  
  set.seed(123)
  samples <- generate(dist, 100)
  
  # Check output structure - generate returns a list
  expect_true(is.list(samples))
  expect_equal(length(samples), 1)
  expect_true(is.matrix(samples[[1]]))
  expect_equal(ncol(samples[[1]]), 2)
  expect_equal(nrow(samples[[1]]), 100)
  
  # Sample mean should be close to theoretical mean (for df > 1)
  expect_equal(colMeans(samples[[1]]), mu, tolerance = 0.5)
})

test_that("Multivariate t with different dimensions", {
  # 3-dimensional
  df <- 10
  mu <- c(1, 2, 3)
  sigma <- diag(3)
  dist <- dist_multivariate_t(df = df, mu = list(mu), sigma = list(sigma))
  
  # Test the dim method on the unwrapped element
  expect_equal(dim.dist_mvt(vctrs::vec_data(dist)[[1]]), 3)
  expect_equal(format(dist), "MVT[3](10)")
  
  # 1-dimensional (should still work)
  dist1d <- dist_multivariate_t(df = 5, mu = list(0), sigma = list(matrix(1)))
  expect_equal(dim.dist_mvt(vctrs::vec_data(dist1d)[[1]]), 1)
  expect_equal(format(dist1d), "MVT[1](5)")
})
