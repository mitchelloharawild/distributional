expect_correct_inverse <- function(dist, value) {
  tvalue <- eval_transform(dist, value)[[1]]
  inv_tvalue <- eval_inverse(dist, tvalue)[[1]]
  expect_equal(inv_tvalue, value)
}

expect_correct_derivative <- function(dist, expr, value) {
  expr <- enexpr(expr)
  if (is.symbol(expr)) {
    expr <- eval(expr, parent.frame())
  }
  if (is.function(expr)) {
    ref <- expr(value)
  } else {
    ref <- eval(expr, list(x = value), parent.frame())
  }
  res <- eval_deriv(dist, value)[[1]]

  expect_equal(res, ref, tolerance = 1e-5)
}

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
  expect_correct_inverse(-d, v)

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
  expect_correct_inverse(dist2, v)

  # monstrous analytical derivative based on wolfram alpha
  an_deriv <- function(x) {
    term1 <- (log(10) * 10^(1/((log(x) - 2)^2)))
    term2 <- x * sqrt(10^(1/((log(x) - 2)^2)) - 3)
    term3 <- sqrt(10^(1/((log(x) - 2)^2)) - 2)
    term4 <- (log(x) - 2)^3
    term1 / (term2 * term3 * term4 * (acosh(term3) - 1) * acosh(term3))
  }

  expect_correct_derivative(dist2, an_deriv, v)
})


test_that('providing custom derivative functions works', {
  op <- options(dist.verbose = TRUE)
  on.exit(op, add = TRUE)
  # base distribution
  d <- dist_gamma(1,1)

  # built-in transformation (uses symbolic derivatives)
  dist1 <- log(1+exp(-d)^2)
  transform <- vec_data(dist1)[[1]]$transform
  inv <- vec_data(dist1)[[1]]$inverse
  deriv <- vec_data(dist1)[[1]]$d_inverse


  # custom transformation without provided derivative function but we find it numerically
  dist2 <- expect_message(dist_transformed(d, transform, inv),
    "Cannot compute the derivative of the inverse function symbolicly")

  # custom transformation with provided derivative function
  dist3 <- expect_silent(dist_transformed(d, transform, inv, deriv))

  # same custom transformation, but derivative is found symbolically
  dist4 <- expect_silent(
    dist_transformed(d, \(x) log(1+exp(-x)^2), \(x) -log(sqrt(exp(x)-1)))
  )


  res1 <- density(dist1, 0.5, verbose = TRUE)
  res2 <- density(dist2, 0.5, verbose = TRUE)
  res3 <- density(dist3, 0.5, verbose = TRUE)
  res4 <- density(dist4, 0.5, verbose = TRUE)
  expect_true(all.equal(res1, res2, res3, res4))
})


test_that("transforms with strange environments work", {
  # https://github.com/mitchelloharawild/distributional/pull/101#issuecomment-2035083570

  # functions with constants in the function environment
  f = (function() {
    b = 2
    function(x) x * b
  })()

  f_inv = (function() {
    b = 2
    function(x) x / b
  })()

  d_f_inv = (function() {
    b = 2
    function(x) 1 / b
  })()

  d = dist_transformed(dist_normal(), f, f_inv, d_f_inv)
  expect_equal(density(exp(d), 1), density(exp(2*dist_normal()), 1))

  d <- f(dist_wrap('norm'))
  expect_equal(density(exp(d), 1), density(exp(2*dist_normal()), 1))

  # apply another function with same constant name but different value
  f10 = (function() {
    b = 10
    function(x) x * b
  })()

  d10 <- f10(exp(d))
  expect_equal(density(d10, 1), density(10*exp(2*dist_normal()), 1))

  # functions with custom functions in the environment
  fil <- (function() {
    inv_logit <- function(x) 1 / (1 + exp(-x))
    function(x) inv_logit(x)
  })()

  fil_inv <- (function() {
    logit <- function(x) log(x) - log(1 - x)
    function(x) logit(x)
  })()

  d_fil_inv <- (function() {
    oneover <- function(x) 1 / (x * (1 - x))
    function(x) oneover(x)
  })()

  d <- dist_transformed(dist_logistic(0, 1), fil, fil_inv, d_fil_inv)
  expect_equal(density(d, 0.5), density(dist_uniform(0, 1), 0.5))
  d2 <- -log(1/d - 1)
  expect_equal(density(d2, 0), density(dist_logistic(0, 1), 0))

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
