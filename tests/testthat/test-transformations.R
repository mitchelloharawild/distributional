test_that("hilo of transformed distributions", {
  expect_identical(
    hilo(exp(dist_poisson(3))),
    exp(hilo((dist_poisson(3))))
  )
})

test_that("chains of transformations", {
  expect_identical(
    hilo(dist_student_t(5)),
    hilo(log(exp(dist_student_t(5))))
  )

  expect_output(
    print(exp(dist_student_t(5))-1),
    "t\\(t\\(5, 0, 1\\)\\)"
  )
})

test_that("handling of transformation arguments", {
  expect_identical(
    hilo(logb(dist_normal(5, 1), base = 10)),
    logb(hilo(dist_normal(5, 1)), base = 10)
  )

  expect_identical(
    hilo(10^logb(dist_normal(5, 1), base = 10)),
    10^logb(hilo(dist_normal(5, 1)), base = 10)
  )
})

test_that("LogNormal distributions", {
  dist <- dist_transformed(dist_normal(0, 0.5), exp, log)
  ln_dist <- dist_lognormal(0, 0.5)

  # Test exp() shortcut
  expect_identical(
    exp(dist_normal(0, 0.5)),
    ln_dist
  )
  expect_identical(
    log(ln_dist),
    dist_normal(0, 0.5)
  )

  # Test log() shortcut with different bases
  expect_equal(log(dist_lognormal(0, log(3)), base = 3), dist_normal(0, 1))
  expect_equal(log2(dist_lognormal(0, log(2))), dist_normal(0, 1))
  expect_equal(log10(dist_lognormal(0, log(10))), dist_normal(0, 1))

  # format
  expect_equal(format(dist), sprintf("t(%s)", format(dist_normal(0, 0.5))))

  # quantiles
  expect_equal(
    quantile(dist, c(0.1, 0.5)),
    quantile(ln_dist, c(0.1, 0.5))
  )

  # pdf
  expect_equal(
    density(dist, c(1, 20)),
    density(ln_dist, c(1, 20))
  )

  # cdf
  expect_equal(
    cdf(dist, c(4, 90)),
    cdf(ln_dist, c(4, 90))
  )

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.372)), 0.372, tolerance = 1e-3)

  # stats (approximate due to bias adjustment method)
  expect_equal(mean(dist), exp(0.25/2), tolerance = 0.01)
  expect_equal(variance(dist), (exp(0.25) - 1)*exp(0.25), tolerance = 0.1)
})

test_that("inverses are applied automatically", {
  dist <- dist_gamma(1,1)
  log2dist <- log(dist, base = 2)
  log2dist_t <- dist_transformed(dist, log2, function(x) 2 ^ x)

  expect_equal(density(log2dist, 0.5), density(log2dist_t, 0.5))
  expect_equal(cdf(log2dist, 0.5), cdf(log2dist_t, 0.5))
  expect_equal(quantile(log2dist, 0.5), quantile(log2dist_t, 0.5))

  # test multiple transformations that get stacked together by dist_transformed
  explogdist <- exp(log(dist))
  expect_equal(density(dist, 0.5), density(explogdist, 0.5))
  expect_equal(cdf(dist, 0.5), cdf(explogdist, 0.5))
  expect_equal(quantile(dist, 0.5), quantile(explogdist, 0.5))

  # test multiple transformations created by operators (via Ops)
  explog2dist <- 2 ^ log2dist
  expect_equal(density(dist, 0.5), density(explog2dist, 0.5))
  expect_equal(cdf(dist, 0.5), cdf(explog2dist, 0.5))
  expect_equal(quantile(dist, 0.5), quantile(explog2dist, 0.5))

  # basic set of inverses
  expect_equal(density(sqrt(dist^2), 0.5), density(dist, 0.5))
  expect_equal(density(exp(log(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(10^(log10(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(expm1(log1p(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(cos(acos(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(sin(asin(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(tan(atan(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(cosh(acosh(dist + 1)) - 1, 0.5), density(dist, 0.5))
  expect_equal(density(sinh(asinh(dist)), 0.5), density(dist, 0.5))
  expect_equal(density(tanh(atanh(dist)), 0.5), density(dist, 0.5))

  expect_equal(density(dist + 1 - 1, 0.5), density(dist, 0.5))
  expect_equal(density(dist * 2 / 2, 0.5), density(dist, 0.5))

  # inverting a gamma distribution
  expect_equal(density(1/dist_gamma(4, 3), 0.5), density(dist_inverse_gamma(4, 1/3), 0.5))
  expect_equal(density(1/(1/dist_gamma(4, 3)), 0.5), density(dist_gamma(4, 3), 0.5))

})
