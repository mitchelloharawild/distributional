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

test_that("inverses are correct", {
  expect_correct_inverse <- function(dist, value) {
    trans <- vec_data(dist)[[1]]$transform
    inv <- vec_data(dist)[[1]]$inverse
    expect_equal(inv(trans(value)), value)
  }

  d <- dist_gamma(1,1)
  v <- runif(10)

  # single unary functions
  expect_correct_inverse(sqrt(d), v)
  expect_correct_inverse(exp(d), v)
  expect_correct_inverse(log(d), v)
  expect_correct_inverse(log(d, 2), v)
  expect_correct_inverse(log(d, base = 10), v)
  expect_correct_inverse(expm1(d), v)
  expect_correct_inverse(log1p(d), v)
  expect_correct_inverse(cos(d), v)
  expect_correct_inverse(sin(d), v)
  expect_correct_inverse(tan(d), v)
  expect_correct_inverse(acos(d), v)
  expect_correct_inverse(asin(d), v)
  expect_correct_inverse(atan(d), v)
  expect_correct_inverse(cosh(d), v)
  expect_correct_inverse(sinh(d), v)
  expect_correct_inverse(tanh(d), v)
  expect_correct_inverse(acosh(d), v+1)
  expect_correct_inverse(asinh(d), v)
  expect_correct_inverse(atanh(d), v)

  # binary functions
  constant <- runif(1)
  expect_correct_inverse(d + constant, v)
  expect_correct_inverse(d - constant, v)
  expect_correct_inverse(d * constant, v)
  expect_correct_inverse(d / constant, v)
  expect_correct_inverse(d ^ constant, v)
  expect_correct_inverse(constant + d, v)
  expect_correct_inverse(constant - d, v)
  expect_correct_inverse(constant * d, v)
  expect_correct_inverse(constant / d, v)
  expect_correct_inverse(constant ^ d, v)

  # custom functions
  myfun <- function(x) log(x + 1)
  inv_logit <- function(x) 1/(1 + exp(-x))
  expect_correct_inverse(myfun(d), v)
  expect_correct_inverse(inv_logit(d), v)

  # unary after binary
  expect_correct_inverse(log(d^2), v)
  expect_correct_inverse(log(d^2, 2), v)
  expect_correct_inverse(log(d^2, base = 10), v)
  expect_correct_inverse(log(-d), -v)
  expect_correct_inverse(exp(2^d), v)
  expect_correct_inverse(exp(d-1), v)
  expect_correct_inverse(cos(d/2), v)

  # binary after unary
  expect_correct_inverse(2^log(d), v)
  expect_correct_inverse(2^log(d, 2), v)
  expect_correct_inverse(2 + exp(d), v)

  # multiple nested functions of various complexity
  expect_correct_inverse(log(exp(d)), v)
  expect_correct_inverse(exp(log(d)), v)
  expect_correct_inverse(log(-log(d), base = 10), v)
  expect_correct_inverse(sin(log(5-log(d), base = 10)^2/100), v)
})

test_that('symbolic derivatives work', {
  get_deriv <- function(dist) {
    vec_data(dist)[[1]]$deriv
  }
  expect_correct_derivative <- function(dist, expr, value) {
    expr <- parse(text = deparse(substitute(expr)))
    expect_equal(get_deriv(dist)(value),
                 eval(expr, list(x = value)))
  }
  d <- dist_gamma(1,1)
  v <- runif(10)
  expect_correct_derivative(exp(d), 1/x, v) # the derivative of the inverse log(x) is 1/x
  expect_correct_derivative(log(d), exp(x), v)
  expect_correct_derivative(log(d, 2), log(2)*2^x, v)
  expect_correct_derivative(exp(2*d), 0.5/x, v)
  expect_correct_derivative(log(-log(d)), -exp(-exp(x)+x), v)
  expect_correct_derivative(sin(log(d) * 10),
                            0.1 * (exp(asin(x)/10)/sqrt(1 - x^2)), v)

  # with custom function and complex nesting
  inv_logit <- function(x) 1/(1 + exp(-x))
  expect_correct_derivative(acos(inv_logit(d)^2), tan(x)/(2*(sqrt(cos(x))-1)), v)

  ### -------- a super complicated final case --------------
  # nested custom functions involved in further nested transformation
  inv_logit <- function(x) 1/(1 + exp(-x))
  coshsquared_inv_logit <- function(x) cosh(inv_logit(x))^2
  dist2 <- exp(2-1/log(2+coshsquared_inv_logit(d), base = 10)^(0.4+0.1))

  # get random values within the support and test the inverse
  lim <- field(support(dist2), 'lim')[[1]]
  v <- runif(10, lim[1], lim[2])
  res <- exp(2-1/log(2+coshsquared_inv_logit(v), base = 10)^(0.4+0.1))
  expect_equal(vec_data(dist2)[[1]]$inverse(res), v)

  # monstrous analytical derivative based on wolfram alpha
  an_deriv <- function(x) {
    term1 <- (log(10) * 10^(1/((log(x) - 2)^2)))
    term2 <- x * sqrt(10^(1/((log(x) - 2)^2)) - 3)
    term3 <- sqrt(10^(1/((log(x) - 2)^2)) - 2)
    term4 <- (log(x) - 2)^3
    term1 / (term2 * term3 * term4 * (acosh(term3) - 1) * acosh(term3))
  }

  expect_equal(get_deriv(dist2)(v), an_deriv(v))
})
