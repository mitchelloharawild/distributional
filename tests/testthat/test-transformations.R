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
  base <- function(m, s) distributional:::dist_elem(dist_normal(m, s), 1)
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
  expect_equal(field(support(distributional:::dist_elem(d, 1)), "lim")[[1]], c(NA_real_, NA_real_))
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

test_that("non-monotonic transformations of discrete distributions enumerate the support", {
  # (Y - 3)^2 >= 4 when Y <= 1 or Y >= 5
  x <- (dist_poisson(3) - 3)^2
  expect_equal(mean(x >= 4), ppois(1, 3) + ppois(4, 3, lower.tail = FALSE))
  expect_equal(mean(x > 4), ppois(0, 3) + ppois(5, 3, lower.tail = FALSE))
  expect_equal(mean(x < 4), ppois(4, 3) - ppois(1, 3))
  expect_equal(cdf(x, c(0, 1, 4, 4.5, -1))[[1]], c(
    dpois(3, 3), ppois(4, 3) - ppois(1, 3), ppois(5, 3) - ppois(0, 3),
    ppois(5, 3) - ppois(0, 3), 0
  ))
  expect_equal(cdf(x, 4, tail = "upper"), ppois(0, 3) + ppois(5, 3, lower.tail = FALSE))
  expect_equal(point_mass(x, c(0, 1, 2, 4)), c(dpois(3, 3), dpois(2, 3) + dpois(4, 3), 0, dpois(1, 3) + dpois(5, 3)))
  expect_equal(density(x, 4), dpois(1, 3) + dpois(5, 3))
  expect_equal(density(x, c(1, 2))[[1]], c(dpois(2, 3) + dpois(4, 3), 0))

  # Finite support is enumerated exactly, and vectors of distributions
  x <- (dist_binomial(10, c(0.5, 0.3)) - 5)^2
  expect_equal(cdf(x, 4), c(sum(dbinom(3:7, 10, 0.5)), sum(dbinom(3:7, 10, 0.3))))
  expect_equal(cdf(x, 4, tail = "upper"), 1 - c(sum(dbinom(3:7, 10, 0.5)), sum(dbinom(3:7, 10, 0.3))))
  expect_equal(cdf(x, 25), c(1, 1))

  # The far tails of an infinite support
  x <- abs(dist_poisson(1e7) - 1e7)
  expect_equal(cdf(x, 100), ppois(1e7 + 100, 1e7) - ppois(1e7 - 101, 1e7))

  # Too many support points to enumerate
  x <- abs(dist_poisson(1e13) - 1e13)
  expect_warning(p <- cdf(x, 100), "non-monotonic")
  expect_equal(p, NA_real_)
  expect_equal(suppressWarnings(mean(x >= 100)), NA_real_)

  # Continuous distributions are still NA
  expect_warning(p <- cdf((dist_normal() - 1)^2, 4), "non-monotonic")
  expect_equal(p, NA_real_)
  expect_equal(point_mass((dist_normal() - 1)^2, 4), 0)
})

test_that("Ops between unrelated non-transformed distributions error", {
  expect_error(dist_poisson(2) * dist_gamma(2, 1), "not supported")
  expect_error(dist_poisson(2) / dist_gamma(2, 1), "not supported")
  # Transformations of the same underlying distribution are still combined
  x <- dist_gamma(2, 1)
  expect_s3_class(dist_elem(exp(x) * exp(x), 1), "dist_transformed")
})

test_that("transformed distributions are vectorised over n > 1 with one transform", {
  d <- dist_transformed(dist_normal(1:3, c(1, 2, 0.5)), exp, log)
  ref <- dist_lognormal(1:3, c(1, 2, 0.5))
  expect_equal(format(d), paste0("t(", format(dist_normal(1:3, c(1, 2, 0.5))), ")"))
  expect_equal(density(d, 2), density(ref, 2))
  expect_equal(cdf(d, 2), cdf(ref, 2))
  expect_equal(cdf(d, 2, tail = "upper"), cdf(ref, 2, tail = "upper"))
  expect_equal(quantile(d, 0.3), quantile(ref, 0.3))
  expect_equal(quantile(d, 0.3, tail = "upper"), quantile(ref, 0.3, tail = "upper"))
  # Paired and mapped arguments
  expect_equal(density(d, list(c(1, 2, 3)))[[1]], density(ref, list(c(1, 2, 3)))[[1]])
  expect_equal(density(d, c(1, 2)), density(ref, c(1, 2)))
  expect_equal(unlist(field(support(d), "lim")), rep(c(0, Inf), 3))
  # The transformation is shared, so there is a single group
  expect_equal(length(d@x), 1L)
  expect_equal(vapply(generate(d, times = 4), length, 1L), rep(4L, 3))
  expect_true(all(unlist(generate(d, 10)) > 0))
  expect_equal(mean(d), unlist(lapply(1:3, function(i) mean(d[i]))))
  expect_equal(variance(d), unlist(lapply(1:3, function(i) variance(d[i]))))
})

test_that("transformed distributions in one vector with different transforms", {
  d <- c(
    dist_transformed(dist_normal(0, 1), exp, log),
    dist_transformed(dist_normal(1, 2), function(x) -x, function(x) -x),
    dist_transformed(dist_normal(2, 1), exp, log),
    dist_transformed(dist_normal(3, 1), function(x) x + 1, function(x) x - 1)
  )
  one <- function(f, ...) unlist(lapply(seq_along(d), function(i) f(d[i], ...)))
  expect_equal(density(d, 1.5), one(density, 1.5))
  expect_equal(cdf(d, 1.5), one(cdf, 1.5))
  expect_equal(cdf(d, 1.5, tail = "upper"), one(cdf, 1.5, tail = "upper"))
  expect_equal(quantile(d, 0.2), one(quantile, 0.2))
  expect_equal(quantile(d, 0.2, tail = "upper"), one(quantile, 0.2, tail = "upper"))
  expect_equal(
    density(d, 1:4 / 2),
    lapply(1:4, function(i) density(d[i], 1:4 / 2)[[1]])
  )
  expect_equal(density(d, list(1:4 / 2))[[1]], vapply(1:4, function(i) density(d[i], i / 2), 0))
  expect_equal(median(d), one(median))
  expect_equal(mean(d), one(mean))
  expect_equal(variance(d), one(variance))
  expect_equal(vapply(generate(d, times = 3), length, 1L), rep(3L, 4))
  expect_equal(format(d), c("t(N(0, 1))", "t(N(1, 4))", "t(N(2, 1))", "t(N(3, 1))"))
  # Decreasing transform swaps tails alongside increasing ones
  expect_equal(cdf(d[2], 0), pnorm(0, 1, 2, lower.tail = FALSE))
  expect_equal(unlist(field(support(d), "lim")), c(0, Inf, -Inf, Inf, 0, Inf, -Inf, Inf))
})

test_that("transformed distributions of mixed inner families", {
  d <- dist_transformed(c(dist_normal(0, 1), dist_gamma(2, 1), dist_poisson(3)), function(x) 2 * x, function(x) x / 2)
  one <- function(f, ...) unlist(lapply(seq_along(d), function(i) f(d[i], ...)))
  expect_equal(cdf(d, 2), one(cdf, 2))
  expect_equal(quantile(d, 0.4), one(quantile, 0.4))
  expect_equal(density(d, 2), one(density, 2))
  expect_equal(format(d), c("t(N(0, 1))", "t(Gamma(2, 1))", "t(Pois(3))"))
  expect_equal(vapply(generate(d, times = 5), length, 1L), rep(5L, 3))
})

test_that("cdf of decreasing transformations of discrete distributions keeps the mass at the threshold", {
  prob <- function(x) mean(x)
  y <- -dist_poisson(3)
  # P(-Y <= -k) = P(Y >= k)
  expect_equal(cdf(y, -2), ppois(1, 3, lower.tail = FALSE))
  expect_equal(cdf(y, -2, tail = "upper"), ppois(1, 3))
  expect_equal(cdf(y, -(0:6))[[1]], ppois(-1:5, 3, lower.tail = FALSE))
  expect_equal(cdf(y, -(0:6), tail = "upper")[[1]], ppois(-1:5, 3))
  expect_equal(cdf(y, 0), 1)
  expect_equal(cdf(y, 0, tail = "upper"), 0)
  # Between the support points there is no mass
  expect_equal(cdf(y, -2.5), ppois(2, 3, lower.tail = FALSE))
  expect_equal(prob(y >= -2), ppois(2, 3))
  expect_equal(prob(y > -2), ppois(1, 3))
  expect_equal(prob(y < -2), ppois(2, 3, lower.tail = FALSE))
  expect_equal(prob(y <= -2), ppois(1, 3, lower.tail = FALSE))

  # Inexact inverses: -0.3 / -0.1 is 2.9999999999999996
  y <- -0.1 * dist_poisson(3)
  q <- -c(0, 0.1, 0.2, 0.3, 0.7)
  expect_equal(cdf(y, q)[[1]], ppois(c(0, 1, 2, 3, 7) - 1, 3, lower.tail = FALSE))
  expect_equal(prob(y >= q), ppois(c(0, 1, 2, 3, 7), 3))
  expect_equal(prob(y < q), ppois(c(0, 1, 2, 3, 7), 3, lower.tail = FALSE))

  # Other decreasing transformations, and those of mixed distributions
  expect_equal(cdf(1 / dist_poisson(3), 0.5), ppois(1, 3, lower.tail = FALSE))
  x <- -dist_inflated(dist_normal(), 0.2, 0)
  expect_equal(cdf(x, 0), 0.6)
  expect_equal(cdf(x, 0, tail = "upper"), 0.4)

  # The cdf of continuous distributions is unchanged
  expect_equal(cdf(-dist_normal(1), -0.5), pnorm(0.5, 1, lower.tail = FALSE))

  # Increasing and decreasing transformations in one vector, with rep()
  x <- rep(c(-dist_poisson(3), 2 * dist_poisson(3), -dist_normal()), 2)
  q <- c(-2, 4, 1, -3, 6, -1)
  expect_equal(
    cdf(x, list(q))[[1]],
    c(ppois(1, 3, lower.tail = FALSE), ppois(2, 3), pnorm(1),
      ppois(2, 3, lower.tail = FALSE), ppois(3, 3), pnorm(-1))
  )
  expect_equal(
    cdf(x, list(q), tail = "upper")[[1]],
    c(ppois(1, 3), ppois(2, 3, lower.tail = FALSE), pnorm(1, lower.tail = FALSE),
      ppois(2, 3), ppois(3, 3, lower.tail = FALSE), pnorm(-1, lower.tail = FALSE))
  )
  expect_equal(cdf(c(-dist_poisson(3), NA), -2), c(ppois(1, 3, lower.tail = FALSE), NA))
})

test_that("quantiles of transformed discrete distributions invert the cdf", {
  p <- c(0.01, 0.1, 0.3, 0.5, 0.7, 0.9, 0.99)
  # The smallest y with P(-Y <= y) >= p is -k for the largest k with P(Y >= k) >= p
  expect_equal(quantile(-dist_poisson(3), p)[[1]], -qpois(p, 3, lower.tail = FALSE))
  expect_equal(quantile(0.1 * dist_poisson(3), p)[[1]], 0.1 * qpois(p, 3))
  expect_equal(quantile(2 * dist_poisson(3), p)[[1]], 2 * qpois(p, 3))
  expect_equal(quantile(-dist_poisson(3), p, tail = "upper")[[1]], -qpois(p, 3))

  # At the probabilities of the support points, in both tails
  for (y in list(-dist_poisson(3), -0.1 * dist_poisson(3), 0.1 * dist_poisson(3), 2 * dist_poisson(3))) {
    pts <- unique(quantile(y, p)[[1]])
    expect_equal(quantile(y, cdf(y, pts)[[1]])[[1]], pts)
  }
  y <- -dist_poisson(3)
  pts <- -(0:8)
  expect_equal(quantile(y, cdf(y, pts, tail = "upper")[[1]], tail = "upper")[[1]], pts)
  expect_equal(quantile(y, ppois(1, 3, lower.tail = FALSE)), -2)
  expect_equal(quantile(y, ppois(1, 3), tail = "upper"), -2)
  expect_equal(quantile(y, c(0, 1))[[1]], c(-Inf, 0))
  expect_equal(quantile(-dist_normal(), 0.3), -qnorm(0.7))
})

test_that("density of transformed discrete distributions is the mass at the preimage", {
  y <- 2 * dist_poisson(3)
  expect_equal(density(y, 4), dpois(2, 3))
  expect_equal(density(y, c(0, 2, 3, 4, -2))[[1]], c(dpois(0:2, 3)[c(1, 2)], 0, dpois(2, 3), 0))
  expect_equal(density(0.1 * dist_poisson(3), c(0.1, 0.2, 0.3, 0.25))[[1]], c(dpois(1:3, 3), 0))
  expect_equal(density(-dist_poisson(3), -(0:3))[[1]], dpois(0:3, 3))
  expect_equal(density(exp(dist_poisson(3)), exp(2)), dpois(2, 3))
  expect_equal(density(dist_binomial(5, 0.4) + 0.5, 2.5), dbinom(2, 5, 0.4))
  expect_equal(log_density(y, 4), dpois(2, 3, log = TRUE))
  expect_equal(density(y, 4, log = TRUE), dpois(2, 3, log = TRUE))

  # The masses sum to 1 over the image of the support
  expect_equal(sum(density(0.1 * dist_poisson(3), (0:50) / 10)[[1]]), 1)

  # Continuous bases keep the Jacobian, also alongside discrete bases
  d <- dist_transformed(c(dist_normal(0, 1), dist_poisson(3)), function(x) 2 * x, function(x) x / 2)
  expect_equal(density(d, list(c(1, 4)))[[1]], c(dnorm(0.5) / 2, dpois(2, 3)))
  expect_equal(density(exp(dist_normal()), 2), dlnorm(2))

  # Vectors with rep(), missing distributions and other families
  x <- rep(c(2 * dist_poisson(3), dist_normal()), 2)
  expect_equal(density(x, list(c(4, 0, 3, 1)))[[1]], c(dpois(2, 3), dnorm(0), 0, dnorm(1)))
  expect_equal(density(c(2 * dist_poisson(3), NA), 4), c(dpois(2, 3), NA))

  # Likelihoods of transformed counts are those of the counts
  expect_equal(likelihood(y, list(c(2, 4, 6))), prod(dpois(1:3, 3)))
  expect_equal(likelihood(y, list(c(2, 4, 6)), log = TRUE), sum(dpois(1:3, 3, log = TRUE)))
  expect_equal(likelihood(y, list(c(2, 3))), 0)
})

test_that("hdr() of transformed distributions", {
  # The 95% HDR of exp(X), X ~ Gamma(2, 1), with density log(y) / y^2 for y > 1
  f <- function(y) log(y) / y^2
  ends <- function(a) {
    c(stats::uniroot(function(y) f(y) - a, c(1, exp(0.5)), tol = 1e-12)$root,
      stats::uniroot(function(y) f(y) - a, c(exp(0.5), 1e8), tol = 1e-12)$root)
  }
  a <- stats::uniroot(function(a) diff(pgamma(log(ends(a)), 2)) - 0.95, c(1e-6, f(exp(0.5)) - 1e-6), tol = 1e-14)$root
  x <- hdr(exp(dist_gamma(2, 1)), n = 1e4)
  expect_equal(c(vctrs::field(x, "lower")[[1]], vctrs::field(x, "upper")[[1]]), ends(a), tolerance = 0.01)

  # A transformed discrete distribution has the HDR of the base, transformed
  x <- hdr(2 * dist_poisson(3))
  y <- hdr(dist_poisson(3))
  expect_equal(vctrs::field(x, "lower")[[1]], vctrs::field(y, "lower")[[1]] * 2)
  expect_equal(vctrs::field(x, "upper")[[1]], vctrs::field(y, "upper")[[1]] * 2)

  # Vectors of transformed and other distributions
  x <- hdr(c(2 * dist_poisson(3), exp(dist_gamma(2, 1)), dist_normal()))
  expect_length(x, 3)
  expect_equal(vctrs::field(x, "upper")[[3]], qnorm(0.975))
})

test_that("support() of transformed discrete distributions", {
  sup <- function(x) list(
    x = vctrs::field(support(x), "x")[[1]],
    lim = vctrs::field(support(x), "lim")[[1]],
    closed = vctrs::field(support(x), "closed")[[1]]
  )
  # Integers are mapped to integers
  expect_equal(sup(-dist_poisson(3)), list(x = integer(), lim = c(-Inf, 0), closed = c(FALSE, TRUE)))
  expect_equal(format(support(-dist_poisson(3))), "{...,-1,0}")
  expect_equal(sup(-dist_binomial(4, 0.5)), list(x = integer(), lim = c(-4, 0), closed = c(TRUE, TRUE)))
  expect_equal(sup(2 * dist_poisson(3))$x, integer())
  expect_equal(sup(dist_poisson(3)^2)$x, integer())
  expect_equal(sup(dist_geometric(0.3) - 5), list(x = integer(), lim = c(-5, Inf), closed = c(TRUE, FALSE)))

  # Integers aren't mapped to integers
  expect_equal(sup(0.1 * dist_poisson(3)), list(x = double(), lim = c(0, Inf), closed = c(TRUE, FALSE)))
  expect_equal(sup(dist_poisson(3) + 0.5)$x, double())
  expect_equal(sup(sqrt(dist_poisson(3)))$x, double())
  expect_equal(sup(exp(dist_poisson(3)))$x, double())
  expect_equal(format(support(0.1 * dist_poisson(3))), "[0,Inf)")

  # Logical support is kept only for 0 and 1
  expect_equal(sup(1 - dist_bernoulli(0.3))$x, logical())
  expect_equal(sup(2 * dist_bernoulli(0.3))$x, integer())
  expect_equal(sup(0.5 * dist_bernoulli(0.3))$x, double())

  # Truncating a transformed count off the integers
  x <- dist_truncated(0.1 * dist_poisson(3), lower = 0.25)
  expect_equal(cdf(x, 0.3), dpois(3, 3) / ppois(2, 3, lower.tail = FALSE))
  expect_equal(cdf(x, 0.45), (ppois(4, 3) - ppois(2, 3)) / ppois(2, 3, lower.tail = FALSE))
  expect_equal(quantile(x, 0.01), 0.3)
  expect_equal(density(x, 0.4), dpois(4, 3) / ppois(2, 3, lower.tail = FALSE))
})
