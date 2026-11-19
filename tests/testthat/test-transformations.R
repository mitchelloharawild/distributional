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
    hilo(logb(dist_uniform(0, 100), base = 10)),
    logb(hilo(dist_uniform(0, 100)), base = 10)
  )

  expect_identical(
    hilo(10^logb(dist_uniform(0, 100), base = 10)),
    10^logb(hilo(dist_uniform(0, 100)), base = 10)
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
  skip_if_not_installed("actuar")
  expect_equal(density(1/dist_gamma(4, 3), 0.5), density(dist_inverse_gamma(4, 1/3), 0.5))
  expect_equal(density(1/(1/dist_gamma(4, 3)), 0.5), density(dist_gamma(4, 3), 0.5))

})

test_that("transformed distributions' density is 0 outside of the support region", {
  dist <- dist_wrap('norm')
  expect_equal(density(exp(dist), 0)[[1]], 0)
  expect_equal(density(exp(dist), -1)[[1]], 0)

  dist <- dist_wrap('gamma', shape = 1, rate = 1)
  expect_equal(density(exp(dist), 0)[[1]], 0)
  expect_equal(density(exp(dist), 1)[[1]], 1)
})


test_that("transformed distributions' cdf is 0/1 outside of the support region", {
  dist <- dist_wrap('norm')
  expect_equal(cdf(exp(dist), 0)[[1]], 0)
  expect_equal(cdf(exp(dist), -1)[[1]], 0)
  expect_equal(cdf(-1*exp(dist), 0)[[1]], 1)
  expect_equal(cdf(-1*exp(dist), 2)[[1]], 1)
})

test_that("decreasing transformations swap the closedness of the limits", {
  # The Poisson has a closed lower limit at 0, which becomes a closed upper
  # limit at 0 once negated (so the atom there isn't clamped away).
  d <- -dist_poisson(3)
  expect_equal(field(support(d), "lim")[[1]], c(-Inf, 0))
  expect_equal(field(support(d), "closed")[[1]], c(FALSE, TRUE))
  expect_equal(density(d, 0), density(dist_poisson(3), 0))

  d <- -dist_exponential(1)
  expect_equal(field(support(d), "lim")[[1]], c(-Inf, 0))
  expect_equal(field(support(d), "closed")[[1]], c(FALSE, TRUE))
  expect_equal(density(d, -1), density(dist_exponential(1), 1))
})

test_that("unary negation operator works", {
  dist <- dist_normal(1,1)
  expect_equal(density(-dist, 0.5), density(dist, -0.5))

  dist <- dist_wrap('norm', mean = 1)
  expect_equal(density(-dist, 0.5), density(dist, -0.5))

  dist <- dist_student_t(3, mu = 1)
  expect_equal(density(-dist, 0.5), density(dist, -0.5))
})

test_that("transformed distributions pdf integrates to 1", {
  dist_names <- c('norm', 'gamma', 'beta', 'chisq', 'exp',
                  'logis', 't', 'unif', 'weibull')
  dist_args <- list(list(mean = 1, sd = 1), list(shape = 2, rate = 1),
                    list(shape1 = 3, shape2 = 5), list(df = 5),
                    list(rate = 1),
                    list(location = 1.5, scale = 1), list(df = 10),
                    list(min = 0, max = 1), list(shape = 3, scale = 1))
  names(dist_args) <- dist_names
  dist <- lapply(dist_names, function(x) do.call(dist_wrap, c(x, dist_args[[x]])))
  dist <- do.call(c, dist)
  dfun <- function(x, id, transform) density(get(transform)(dist[id]), x)[[1]]
  twoexp <- function(x) 2^x
  square <- function(x) x^2
  mult2 <- function(x) 2*x
  identity <- function(x) x
  tol <- 1e-5
  for (i in 1:length(dist)) {
    expect_equal(integrate(dfun, -Inf, Inf, id = i, transform = 'identity')$value, 1, tolerance = tol)
    expect_equal(integrate(dfun, -Inf, Inf, id = i, transform = 'exp')$value, 1, tolerance = tol)
    expect_equal(integrate(dfun, -Inf, Inf, id = i, transform = 'twoexp')$value, 1, tolerance = tol)
    expect_equal(integrate(dfun, -Inf, Inf, id = i, transform = 'mult2')$value, 1, tolerance = tol)
    lower_bound <- field(support(dist[[i]]), "lim")[[1]][1]
    if (near(lower_bound, 0)) {
      expect_equal(integrate(dfun, -Inf, 5, id = i, transform = 'log')$value, 1, tolerance = tol)
      expect_equal(integrate(dfun, -Inf, Inf, id = i, transform = 'square')$value, 1, tolerance = tol)
    }
  }
})


test_that("monotonically decreasing transformations (#100)", {
  dist <- dist_lognormal()

  expect_equal(
    quantile(-dist, 0.2), -quantile(dist, 1 - 0.2)
  )
  expect_equal(
    quantile(1/dist, 0.2), 1/quantile(dist, 1 - 0.2)
  )
  expect_equal(
    quantile(-1/dist, 0.7), -1/quantile(dist, 0.7)
  )

  expect_equal(
    cdf(-dist, -2), 1 - cdf(dist, 2)
  )
  expect_equal(
    cdf(1/dist, 2), 1 - cdf(dist, 1/2)
  )
  expect_equal(
    cdf(-1/dist, -2), cdf(dist, 1/2)
  )
})

test_that("monotonic_dir classifies transformation direction (#129)", {
  base <- function(m, s) vec_data(dist_normal(m, s))[[1]]
  expect_identical(monotonic_dir(exp, base(2, 1)), 1L)
  expect_identical(monotonic_dir(function(x) x^2, base(2.3, 0.45)), 1L)
  expect_identical(monotonic_dir(function(x) x^2, base(0.5, 1)), 0L)
  expect_identical(monotonic_dir(function(x) -x, base(0, 1)), -1L)
  expect_identical(monotonic_dir(function(x) x^3, base(0, 1)), 1L)
  # identity shortcut used heavily by ggdist
  expect_identical(monotonic_dir(function(x) x, base(0, 1)), 1L)
})

test_that("sqrt-scale transformed quantiles are ascending (fabletools#442)", {
  d <- dist_transformed(dist_normal(2.3, 0.45), function(x) x^2, function(x) sqrt(x))
  q <- unlist(quantile(d, c(0.001, 0.01, 0.5, 0.99, 0.999)))
  expect_false(is.unsorted(q))
})

test_that("support and cdf of x^2 over a Normal (#129)", {
  d <- dist_transformed(dist_normal(2.3, 0.45), function(x) x^2, function(x) sqrt(x))
  # Monotonicity over the full support cannot be verified, so limits are unknown
  # rather than the wrong (Inf, Inf) that mapping endpoints would give.
  expect_equal(field(support(vec_data(d)[[1]]), "lim")[[1]], c(NA_real_, NA_real_))
  # With clamping disabled, cdf is correct where the mass is.
  expect_equal(unlist(cdf(d, c(1, 5, 10))), pnorm(sqrt(c(1, 5, 10)), 2.3, 0.45))
})

test_that("non-monotonic transformations warn and return NA (#129)", {
  d <- dist_transformed(dist_normal(0.5, 1), function(x) x^2, function(x) sqrt(x))
  expect_warning(q <- quantile(d, c(0.1, 0.5, 0.9)))
  expect_true(all(is.na(unlist(q))))
  expect_warning(p <- cdf(d, c(1, 2)))
  expect_true(all(is.na(unlist(p))))
})
