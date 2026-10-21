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

test_that("Mixture with bad weights",{

  expect_error(
    dist_mixture(dist_normal(0, 1), dist_normal(10, 4), weights = c(0.6, 0.5))
  )

  expect_error(
    dist_mixture(dist_normal(0, 1), dist_normal(10, 4), weights = c(0.4, 0.5))
  )

  dist <- dist_mixture(dist_normal(0, 1),
               dist_normal(10, 4),
               dist_normal(5, 2),
               weights = c(0.2177682569661155698,
                           0.0491619687405063024,
                           0.7330697742933780514))
  expect_s3_class(dist, "distribution")

})

test_that("Quantile mixture of two normals (equal weights)", {
  # Q(p) = 0.5*qnorm(p) + 0.5*qnorm(p,10,4) = 2.5*qnorm(p) + 5 = N(5, 2.5^2)
  dist <- dist_mixture(dist_normal(0, 1), dist_normal(10, 4), weights = c(0.5, 0.5), type = "quantile")

  # format
  expect_equal(format(dist), "mixture<q>(0.5*N(0, 1), 0.5*N(10, 16))")

  # quantile is the weighted average of component quantiles
  expect_equal(quantile(dist, 0.5), 0.5 * qnorm(0.5) + 0.5 * qnorm(0.5, 10, 4))
  expect_equal(quantile(dist, 0.1), 0.5 * qnorm(0.1) + 0.5 * qnorm(0.1, 10, 4), tolerance = 1e-6)
  expect_equal(quantile(dist, c(0.1, 0.5, 0.9))[[1]], c(
    0.5 * qnorm(0.1) + 0.5 * qnorm(0.1, 10, 4),
    0.5 * qnorm(0.5) + 0.5 * qnorm(0.5, 10, 4),
    0.5 * qnorm(0.9) + 0.5 * qnorm(0.9, 10, 4)
  ), tolerance = 1e-6)

  # cdf inverts quantile
  expect_equal(cdf(dist, quantile(dist, 0.3)), 0.3, tolerance = 1e-5)
  expect_equal(cdf(dist, quantile(dist, 0.7)), 0.7, tolerance = 1e-5)

  # mean equals weighted sum of component means
  expect_equal(mean(dist), 0.5 * 0 + 0.5 * 10)

  # variance: for normals, Var = (sum(w_i * sigma_i))^2 = (0.5*1 + 0.5*4)^2 = 6.25
  expect_equal(variance(dist), 6.25, tolerance = 0.01)

  # density matches N(5, 2.5)
  expect_equal(density(dist, 5), dnorm(5, 5, 2.5), tolerance = 0.001)
})

test_that("Quantile mixture of different distributions", {
  dist <- dist_mixture(dist_normal(0, 1), dist_student_t(10), weights = c(0.3, 0.7), type = "quantile")

  # format
  expect_equal(format(dist), "mixture<q>(0.3*N(0, 1), 0.7*t(10, 0, 1))")

  # quantile
  p <- 0.25
  expect_equal(quantile(dist, p), 0.3 * qnorm(p) + 0.7 * qt(p, 10), tolerance = 1e-6)

  # cdf inverts quantile
  expect_equal(cdf(dist, quantile(dist, 0.5)), 0.5, tolerance = 1e-5)

  # density > 0 at the mean
  expect_gt(density(dist, mean(dist)), 0)

  # mean
  expect_equal(mean(dist), 0.3 * 0 + 0.7 * 0)
})

test_that("Quantile mixture: density integrates to 1", {
  dist <- dist_mixture(dist_normal(0, 1), dist_normal(5, 1), weights = c(0.4, 0.6), type = "quantile")

  # Density integrates to 1 over a wide range
  integral <- stats::integrate(
    function(x) vapply(x, \(xi) density(dist, xi), numeric(1L)),
    lower = -5, upper = 12
  )$value
  expect_equal(integral, 1, tolerance = 0.01)
})

test_that("Quantile mixture rejects multivariate distributions", {
  mu <- c(0, 0)
  sigma <- diag(2)
  expect_error(
    dist_mixture(
      dist_multivariate_normal(mu = list(mu), sigma = list(sigma)),
      dist_multivariate_normal(mu = list(mu), sigma = list(sigma)),
      weights = c(0.5, 0.5),
      type = "quantile"
    ),
    "univariate"
  )
})

test_that("Mixtures with different K and component families in one vector", {
  m2 <- dist_mixture(dist_normal(0, 1), dist_normal(10, 4), weights = c(0.5, 0.5))
  m3 <- dist_mixture(
    dist_normal(0, 1), dist_student_t(10), dist_degenerate(2),
    weights = c(0.2, 0.5, 0.3)
  )
  m1 <- dist_mixture(dist_exponential(2), dist_exponential(3), weights = c(0.1, 0.9))
  x <- c(m2, m3, m1)

  expect_equal(
    format(x),
    c(format(m2), format(m3), format(m1))
  )
  expect_equal(family(x), rep("mixture", 3))
  expect_equal(
    family(x, recursive = TRUE),
    list(
      list(family = "mixture", dist = list("normal", "normal")),
      list(family = "mixture", dist = list("normal", "student_t", "degenerate")),
      list(family = "mixture", dist = list("exponential", "exponential"))
    )
  )

  # Each element matches the same mixture on its own
  each <- function(f, ...) unlist(lapply(seq_along(x), function(i) f(x[i], ...)))
  expect_equal(mean(x), each(mean))
  expect_equal(variance(x), each(variance))
  expect_equal(density(x, 1), each(density, 1))
  expect_equal(cdf(x, 1), each(cdf, 1))
  expect_equal(cdf(x, 1, tail = "upper"), 1 - cdf(x, 1))
  expect_equal(quantile(x, 0.3), each(quantile, 0.3), tolerance = 1e-6)
  expect_equal(
    quantile(x, 0.3, tail = "upper"), quantile(x, 0.7), tolerance = 1e-5
  )
  expect_equal(
    cdf(x, list(c(0, 1, 2)))[[1]],
    c(cdf(x[1], 0), cdf(x[2], 1), cdf(x[3], 2))
  )

  # Support is the union of component supports
  expect_equal(field(support(x), "lim")[[3]], c(0, Inf))
  expect_equal(field(support(x), "lim")[[2]], c(-Inf, Inf))
  expect_equal(field(support(x[c(1, 1)]), "lim")[[2]], c(-Inf, Inf))
  expect_equal(field(support(c(m1, m1)), "lim")[[2]], c(0, Inf))

  # Draws
  set.seed(1)
  g <- generate(x, list(c(5, 3, 4)))
  expect_equal(lengths(g), c(5, 3, 4))
  g <- generate(x, 2000)
  expect_equal(vapply(g, mean, numeric(1)), mean(x), tolerance = 0.2)
  expect_equal(g[[3]] >= 0, rep(TRUE, 2000))
})

test_that("Quantile mixtures with different K in one vector", {
  m2 <- dist_mixture(dist_normal(0, 1), dist_normal(10, 4), weights = c(0.5, 0.5), type = "quantile")
  m3 <- dist_mixture(
    dist_normal(0, 1), dist_normal(5, 1), dist_exponential(1),
    weights = c(0.2, 0.5, 0.3), type = "quantile"
  )
  x <- c(m2, m3)
  each <- function(f, ...) unlist(lapply(seq_along(x), function(i) f(x[i], ...)))

  expect_equal(format(x), c(format(m2), format(m3)))
  expect_equal(mean(x), each(mean))
  expect_equal(variance(x), each(variance))
  expect_equal(quantile(x, 0.3), each(quantile, 0.3))
  expect_equal(cdf(x, 2), each(cdf, 2))
  expect_equal(cdf(x, list(quantile(x, 0.4)))[[1]], c(0.4, 0.4), tolerance = 1e-5)
  expect_equal(cdf(x, 2, tail = "upper"), 1 - cdf(x, 2), tolerance = 1e-6)
  expect_equal(density(x, 2), each(density, 2))
  expect_equal(density(x, 100), c(0, 0))
  expect_equal(field(support(x), "lim")[[1]], c(-Inf, Inf))
  expect_equal(lengths(generate(x, list(c(3, 6)))), c(3, 6))
})

test_that("Multivariate mixtures in one vector", {
  mv <- function(mu, sigma = diag(2)) dist_multivariate_normal(mu = list(mu), sigma = list(sigma))
  x <- c(
    dist_mixture(mv(c(0, 0)), mv(c(1, 2), diag(2) * 2), weights = c(0.3, 0.7)),
    dist_mixture(mv(c(5, 5)), mv(c(1, 1)), mv(c(-1, 0)), weights = c(0.2, 0.3, 0.5))
  )
  expect_equal(mean(x), rbind(c(0.7, 1.4), 0.2 * c(5, 5) + 0.3 * c(1, 1) + 0.5 * c(-1, 0)))
  cv <- covariance(x)
  expect_equal(lengths(cv), c(4L, 4L))
  m <- c(0.7, 1.4)
  expect_equal(
    cv[[1]],
    0.3 * (tcrossprod(c(0, 0) - m) + diag(2)) + 0.7 * (tcrossprod(c(1, 2) - m) + 2 * diag(2))
  )
  at <- rbind(c(0, 0), c(1, 1))
  expect_equal(
    density(x, list(at))[[1]],
    c(density(x[1], at[1, , drop = FALSE]), density(x[2], at[2, , drop = FALSE]))
  )
  expect_equal(
    density(x[1], rbind(c(0, 0))),
    0.3 * mvtnorm::dmvnorm(c(0, 0)) + 0.7 * mvtnorm::dmvnorm(c(0, 0), c(1, 2), 2 * diag(2))
  )
  expect_error(quantile(x, 0.5))
  set.seed(1)
  g <- generate(x, list(c(4, 7)))
  expect_equal(vapply(g, nrow, integer(1)), c(4L, 7L))
  expect_equal(vapply(g, ncol, integer(1)), c(2L, 2L))
  expect_equal(format(field(support(x), "lim")[[1]]), format(matrix(c(-Inf, Inf), 2, 2)))
})
