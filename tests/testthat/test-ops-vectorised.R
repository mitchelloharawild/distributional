# Arithmetic on a distribution vector is computed once per storage slot, so
# compare each result with the same operation on single distributions.
expect_elementwise <- function(res, ref, at = c(1, 2, 3)) {
  expect_length(res, length(ref))
  expect_equal(format(res), vapply(ref, format, character(1L)))
  # Transformed discrete distributions warn at non-integer points
  density <- function(...) suppressWarnings(stats::density(...))
  cdf <- function(...) suppressWarnings(distributional::cdf(...))
  for (i in seq_along(ref)) {
    expect_equal(
      density(res[i], at)[[1L]], density(ref[[i]], at)[[1L]],
      label = sprintf("density of element %d", i)
    )
    expect_equal(
      cdf(res[i], at)[[1L]], cdf(ref[[i]], at)[[1L]],
      label = sprintf("cdf of element %d", i)
    )
  }
}

mixed <- function() {
  c(dist_normal(1, 2), dist_gamma(2, 1), dist_poisson(3), dist_uniform(0, 4))
}

test_that("arithmetic with per-element numbers matches element-wise results", {
  x <- mixed()
  y <- c(1, 2, 0.5, 4)
  for (op in c("+", "-", "*", "/")) {
    f <- match.fun(op)
    expect_elementwise(f(x, y), lapply(seq_along(x), function(i) f(x[i], y[i])))
    # A normal distribution can't be divided by
    z <- if (op == "/") x[-1] else x
    expect_elementwise(f(y[seq_along(z)], z), lapply(seq_along(z), function(i) f(y[i], z[i])))
    expect_elementwise(f(x, 2), lapply(seq_along(x), function(i) f(x[i], 2)))
  }
  expect_elementwise(x^2, lapply(seq_along(x), function(i) x[i]^2))
  expect_elementwise(-x, lapply(seq_along(x), function(i) -x[i]))
})

test_that("arithmetic and Math on transformed distributions are per element", {
  x <- dist_gamma(1:3, 1) * c(1, 2, 3)
  ref <- lapply(1:3, function(i) dist_gamma(i, 1) * i)
  expect_elementwise(x, ref)
  expect_equal(density(x[3], 2), density(dist_gamma(3, 1), 2 / 3) / 3)

  expect_elementwise(x + c(3, 2, 1), lapply(1:3, function(i) ref[[i]] + (4 - i)))
  expect_elementwise(10 - x, lapply(1:3, function(i) 10 - ref[[i]]), at = c(4, 7, 9))
  expect_elementwise(log(x), lapply(1:3, function(i) log(ref[[i]])))
  expect_elementwise(exp(x / 10), lapply(1:3, function(i) exp(ref[[i]] / 10)))
  expect_elementwise(sqrt(exp(x)), lapply(1:3, function(i) sqrt(exp(ref[[i]]))))
  expect_elementwise(
    log(dist_gamma(1:3, 1), base = 2),
    lapply(1:3, function(i) log(dist_gamma(i, 1), base = 2))
  )
})

test_that("equal constants share one transformation", {
  tf <- function(x) vctrs::field(x@x[[1L]], "transform")
  same <- function(f) all(vapply(f, identical, logical(1L), f[[1L]]))

  x <- dist_gamma(1:100, 1) * 2
  expect_length(x@x, 1L)
  expect_true(same(tf(x)))
  expect_true(same(tf(exp(x) + 1)))
  expect_false(same(tf(dist_gamma(1:3, 1) * c(1, 2, 3))))
  expect_equal(density(x, 3), dgamma(3 / 2, 1:100, 1) / 2)
})

test_that("Math of mixed families matches element-wise results", {
  x <- c(dist_normal(1, 0.5), dist_gamma(2, 1), dist_lognormal(0, 1), dist_uniform(1, 3))
  expect_elementwise(exp(x), lapply(seq_along(x), function(i) exp(x[i])))
  y <- x[2:4]
  expect_elementwise(log(y), lapply(seq_along(y), function(i) log(y[i])))
  expect_equal(format(exp(x)[1]), "lN(1, 0.25)")
  expect_equal(format(log(x)[3]), "N(0, 1)")
})

test_that("arithmetic between distributions is paired by position", {
  x <- dist_normal(1:3) + dist_normal(0, 1:3)
  expect_equal(x, dist_normal(1:3, sqrt(1 + (1:3)^2)))
  expect_equal(dist_normal(1:3) - dist_normal(0:2, 2), dist_normal(rep(1, 3), sqrt(5)))

  a <- c(dist_normal(1), dist_gamma(2, 1), dist_poisson(2))
  b <- c(dist_gamma(3, 1), dist_normal(2), dist_poisson(3))
  res <- a + b
  ref <- lapply(1:3, function(i) a[i] + b[i])
  expect_equal(format(res), vapply(ref, format, character(1L)))
  expect_equal(mean(res), vapply(ref, mean, numeric(1L)))
  expect_equal(variance(res), vapply(ref, variance, numeric(1L)))
  expect_equal(density(res[3], 5), dpois(5, 5))

  # Transformations of the same distributions are combined element by element
  g <- dist_gamma(1:3, 1)
  expect_equal(mean(exp(g) * exp(g)), mean(exp(2 * g)), tolerance = 1e-6)
  expect_error(exp(g) * exp(dist_gamma(2:4, 1)), "not supported")
})

test_that("arithmetic keeps rep()-compressed operands", {
  x <- rep(dist_gamma(1:2, 1), 3)
  expect_elementwise(x * 1:6, lapply(1:6, function(i) x[i] * i))
  expect_elementwise(x + 1, lapply(1:6, function(i) x[i] + 1))
  y <- rep(dist_normal(1:2), 3)
  expect_equal(y + rep(dist_normal(0, 2), 6), rep(dist_normal(1:2, sqrt(5)), 3))
  expect_equal(-y, rep(dist_normal(-(1:2)), 3))
})

test_that("arithmetic with missing and empty distributions", {
  x <- c(dist_normal(1), NA, dist_gamma(2, 1))
  res <- x + 1
  expect_length(res, 3L)
  expect_equal(format(res)[-2], c("N(2, 1)", format(dist_gamma(2, 1) + 1)))
  expect_equal(mean(res), c(2, NA, 3))
  expect_equal(mean(exp(x))[2], NA_real_)
  expect_equal(mean(x * c(1, 2, NA))[c(1, 3)], c(1, NA))

  m <- dist_missing(2)
  expect_equal(format(m + 1), c("NA", "NA"))
  expect_equal(format(-m), c("NA", "NA"))
  expect_equal(format(exp(m)), c("NA", "NA"))

  expect_length(dist_normal()[0] + 1, 0L)
  expect_length(-dist_normal()[0], 0L)
  expect_length(exp(dist_normal()[0]), 0L)
})

test_that("sum() of a distribution vector", {
  expect_equal(sum(dist_normal(1:3)), dist_normal(6, sqrt(3)))
})

test_that("chooseOpsMethod() is registered for R >= 4.3.0 only", {
  skip_if(getRversion() < "4.3.0")
  expect_false(is.null(getS3method("chooseOpsMethod", "dist_default", optional = TRUE)))
  expect_equal(format(dist_missing() + dist_normal()), "NA")
  expect_equal(format(dist_normal() + dist_missing()), "NA")
})
