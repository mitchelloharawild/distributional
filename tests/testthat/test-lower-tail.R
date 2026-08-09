test_that("cdf(tail = upper) keeps upper tail precision", {
  expect_equal(
    cdf(dist_normal(), 10, tail = "upper"),
    stats::pnorm(10, lower.tail = FALSE)
  )
  expect_gt(cdf(dist_normal(), 10, tail = "upper"), 0)
  expect_equal(
    cdf(dist_normal(1, 2), c(-3, 0, 25), tail = "upper")[[1]],
    stats::pnorm(c(-3, 0, 25), 1, 2, lower.tail = FALSE)
  )
  expect_equal(
    cdf(dist_student_t(3, 1, 2), 1e4, tail = "upper"),
    stats::pt((1e4 - 1) / 2, 3, lower.tail = FALSE)
  )
  expect_equal(
    cdf(dist_gamma(2, 1), 60, tail = "upper"),
    stats::pgamma(60, 2, 1, lower.tail = FALSE)
  )
  expect_equal(
    cdf(dist_exponential(2), 30, tail = "upper"),
    stats::pexp(30, 2, lower.tail = FALSE)
  )
  expect_equal(
    cdf(dist_poisson(3), 30, tail = "upper"),
    stats::ppois(30, 3, lower.tail = FALSE)
  )
  expect_equal(
    cdf(dist_binomial(100, 0.1), 80, tail = "upper"),
    stats::pbinom(80, 100, 0.1, lower.tail = FALSE)
  )
  expect_equal(
    cdf(dist_beta(2, 3), 1 - 1e-10, tail = "upper"),
    stats::pbeta(1 - 1e-10, 2, 3, lower.tail = FALSE)
  )
  expect_equal(
    cdf(dist_weibull(2, 1), 7, tail = "upper"),
    stats::pweibull(7, 2, 1, lower.tail = FALSE)
  )
  expect_equal(
    cdf(dist_lognormal(0, 1), 1e5, tail = "upper"),
    stats::plnorm(1e5, 0, 1, lower.tail = FALSE)
  )
  expect_equal(
    cdf(dist_f(3, 5), 1e6, tail = "upper"),
    stats::pf(1e6, 3, 5, lower.tail = FALSE)
  )
  expect_equal(
    cdf(dist_studentized_range(3, 10, 1), 20, tail = "upper"),
    stats::ptukey(20, 3, 10, 1, lower.tail = FALSE)
  )
})

test_that("cdf(tail = upper) for closed form families", {
  # GEV, Gumbel case: S(q) = 1 - exp(-exp(-q))
  expect_equal(
    cdf(dist_gev(0, 1, 0), 40, tail = "upper"),
    -expm1(-exp(-40))
  )
  expect_gt(cdf(dist_gev(0, 1, 0.2), 1e8, tail = "upper"), 0)
  # GPD: S(q) = (1 + xi q)^(-1/xi)
  expect_equal(
    cdf(dist_gpd(0, 1, 0.5), 1e20, tail = "upper"),
    (1 + 0.5 * 1e20)^(-2)
  )
  expect_equal(
    cdf(dist_gpd(0, 1, 0), 50, tail = "upper"),
    exp(-50)
  )
  # Laplace: S(q) = exp(-(q - mu) / sigma) / 2 for q > mu
  expect_equal(
    cdf(dist_laplace(0, 1), 50, tail = "upper"),
    exp(-50) / 2
  )
  expect_equal(
    cdf(dist_laplace(1, 2), c(-3, 1, 4), tail = "upper")[[1]],
    1 - cdf(dist_laplace(1, 2), c(-3, 1, 4))[[1]]
  )
  # Degenerate
  expect_equal(cdf(dist_degenerate(2), c(1, 2, 3), tail = "upper")[[1]], c(1, 0, 0))
  # Ordered categorical
  x <- dist_categorical(
    list(c(0.2, 0.3, 0.5)),
    list(factor(c("a", "b", "c"), ordered = TRUE))
  )
  expect_equal(
    cdf(x, c("a", "b", "c"), tail = "upper")[[1]],
    c(0.8, 0.5, 0)
  )
  # Sample
  x <- dist_sample(list(1:10))
  expect_equal(cdf(x, 3, tail = "upper"), 0.7)
})

test_that("cdf(tail = upper) passes through for extension families", {
  skip_if_not_installed("actuar")
  expect_equal(
    cdf(dist_pareto(3, 2), 1e6, tail = "upper"),
    actuar::ppareto(1e6, 3, 2, lower.tail = FALSE)
  )
  expect_equal(
    cdf(dist_inverse_gaussian(1, 1), 200, tail = "upper"),
    actuar::pinvgauss(200, 1, 1, lower.tail = FALSE)
  )
  expect_equal(
    cdf(dist_gumbel(0, 1), 40, tail = "upper"),
    actuar::pgumbel(40, 0, 1, lower.tail = FALSE)
  )
})

test_that("cdf(tail = upper) for dist_wrap()", {
  expect_equal(
    cdf(dist_wrap("norm", mean = 1), 11, tail = "upper"),
    stats::pnorm(11, 1, lower.tail = FALSE)
  )
  expect_equal(
    quantile(dist_wrap("norm", mean = 1), 1e-20, tail = "upper"),
    stats::qnorm(1e-20, 1, lower.tail = FALSE)
  )

  # Wrapped functions without a lower.tail argument use the complement
  env <- new.env()
  env$pmyexp <- function(q, rate = 1) 1 - exp(-rate * q)
  env$qmyexp <- function(p, rate = 1) -log(1 - p) / rate
  env$rmyexp <- function(n, rate = 1) stats::rexp(n, rate)
  x <- dist_wrap("myexp", rate = 2, package = env)
  expect_equal(cdf(x, 1, tail = "upper"), exp(-2))
  expect_equal(quantile(x, 0.25, tail = "upper"), -log(0.25) / 2)
})

test_that("cdf(tail = upper) for modifiers uses upper tails", {
  # Mixture: weighted upper tails
  x <- dist_mixture(dist_normal(), dist_normal(1), weights = c(0.3, 0.7))
  expect_equal(
    cdf(x, 12, tail = "upper"),
    0.3 * stats::pnorm(12, lower.tail = FALSE) +
      0.7 * stats::pnorm(11, lower.tail = FALSE)
  )
  expect_equal(
    cdf(x, c(-1, 0.5, 2), tail = "upper")[[1]],
    1 - cdf(x, c(-1, 0.5, 2))[[1]]
  )

  # Quantile mixture
  x <- dist_mixture(dist_normal(), dist_normal(1), weights = c(0.5, 0.5),
                    type = "quantile")
  expect_equal(
    cdf(x, c(-1, 0.5, 3), tail = "upper")[[1]],
    stats::pnorm(c(-1, 0.5, 3), 0.5, lower.tail = FALSE),
    tolerance = 1e-6
  )

  # Truncated: ratio of upper tails
  x <- dist_truncated(dist_normal(), 5, Inf)
  expect_equal(
    cdf(x, 10, tail = "upper"),
    stats::pnorm(10, lower.tail = FALSE) / stats::pnorm(5, lower.tail = FALSE)
  )
  x <- dist_truncated(dist_normal(), -1, 2)
  q <- c(-2, -1, 0, 1.5, 2, 3)
  expect_equal(cdf(x, q, tail = "upper")[[1]], 1 - cdf(x, q)[[1]])
  x <- dist_truncated(dist_poisson(4), 2, 7)
  q <- 0:9
  expect_equal(cdf(x, q, tail = "upper")[[1]], 1 - cdf(x, q)[[1]])

  # Inflated
  x <- dist_inflated(dist_normal(), 0.3, 1)
  expect_equal(
    cdf(x, 20, tail = "upper"),
    0.7 * stats::pnorm(20, lower.tail = FALSE)
  )
  q <- c(-1, 1, 2)
  expect_equal(cdf(x, q, tail = "upper")[[1]], 1 - cdf(x, q)[[1]])
  x <- dist_inflated(dist_poisson(3), 0.2)
  q <- 0:5
  expect_equal(cdf(x, q, tail = "upper")[[1]], 1 - cdf(x, q)[[1]])

  # Increasing transformation
  x <- exp(dist_normal())
  expect_equal(
    cdf(x, exp(10), tail = "upper"),
    stats::pnorm(10, lower.tail = FALSE)
  )
  # Decreasing transformation: the tails swap
  x <- -dist_normal()
  expect_equal(cdf(x, -10), stats::pnorm(10, lower.tail = FALSE))
  expect_equal(cdf(x, -10, tail = "upper"), stats::pnorm(-10, lower.tail = FALSE))
  expect_equal(
    cdf(x, 10, tail = "upper"),
    stats::pnorm(10, lower.tail = FALSE)
  )
  x <- 1 / dist_gamma(3, 1)
  q <- c(0.2, 0.5, 2)
  expect_equal(cdf(x, q, tail = "upper")[[1]], 1 - cdf(x, q)[[1]])
})

test_that("cdf(tail = upper) for multivariate distributions is the complement", {
  skip_if_not_installed("mvtnorm")
  x <- dist_multivariate_normal(list(c(0, 0)), list(diag(2)))
  expect_equal(
    cdf(x, cbind(0, 0), tail = "upper"),
    1 - cdf(x, cbind(0, 0))
  )
})

new_lower_tail_test_dist <- function(cls, methods) {
  ns <- asNamespace("distributional")
  for (generic in names(methods)) {
    registerS3method(generic, cls, methods[[generic]], envir = ns)
  }
  new_dist(placeholder = NA_real_, class = cls)
}

test_that("dist_default falls back to 1 - cdf() and quantile(1 - p)", {
  x <- new_lower_tail_test_dist("dist_test_lower_tail_generate", list(
    generate = function(x, times, ...) stats::rexp(times)
  ))
  set.seed(1)
  expect_equal(
    cdf(x, c(0.5, 1), tail = "upper")[[1]],
    stats::pexp(c(0.5, 1), lower.tail = FALSE),
    tolerance = 1e-2
  )

  x <- new_lower_tail_test_dist("dist_test_lower_tail_cdf", list(
    cdf = function(x, q, ...) stats::pexp(q)
  ))
  expect_equal(
    suppressWarnings(quantile(x, c(0.1, 0.5), tail = "upper"))[[1]],
    stats::qexp(c(0.1, 0.5), lower.tail = FALSE),
    tolerance = 1e-3
  )
})

test_that("tail = upper works with log = TRUE", {
  expect_equal(
    cdf(dist_normal(), 40, tail = "upper", log = TRUE),
    stats::pnorm(40, lower.tail = FALSE, log.p = TRUE)
  )
  expect_equal(
    cdf(dist_lognormal(0, 1), exp(40), tail = "upper", log = TRUE),
    stats::plnorm(exp(40), lower.tail = FALSE, log.p = TRUE)
  )
  # Families without a log_cdf() method use log(cdf(tail = "upper"))
  expect_equal(
    cdf(dist_gamma(2, 1), 40, tail = "upper", log = TRUE),
    stats::pgamma(40, 2, 1, lower.tail = FALSE, log.p = TRUE)
  )
  # Cumulative hazard of a Weibull distribution: (q / scale)^shape
  expect_equal(
    -cdf(dist_weibull(2, 3), 6, tail = "upper", log = TRUE),
    (6 / 3)^2
  )
  expect_equal(
    quantile(dist_normal(), log(1e-300), tail = "upper", log = TRUE),
    stats::qnorm(log(1e-300), lower.tail = FALSE, log.p = TRUE)
  )
  expect_equal(
    quantile(dist_gamma(2, 1), log(0.1), tail = "upper", log = TRUE),
    stats::qgamma(0.1, 2, 1, lower.tail = FALSE)
  )
})

test_that("quantile(tail = upper) is precise and round-trips with cdf()", {
  expect_equal(
    quantile(dist_normal(), 0.1, tail = "upper"),
    stats::qnorm(0.1, lower.tail = FALSE)
  )
  expect_equal(
    quantile(dist_normal(), 1e-20, tail = "upper"),
    stats::qnorm(1e-20, lower.tail = FALSE)
  )
  expect_equal(
    quantile(dist_gev(0, 1, 0), 1e-20, tail = "upper"),
    -log(-log1p(-1e-20))
  )
  expect_equal(
    quantile(dist_gpd(0, 1, 0), 1e-20, tail = "upper"),
    -log(1e-20)
  )
  expect_equal(
    quantile(dist_laplace(0, 1), 1e-20, tail = "upper"),
    -log(2e-20)
  )

  p <- c(1e-12, 0.01, 0.3, 0.7)
  dists <- c(
    dist_normal(1, 2), dist_student_t(4, 1, 2), dist_gamma(2, 1),
    dist_beta(2, 3), dist_weibull(2, 1), dist_logistic(0, 1),
    dist_gev(0, 1, 0.2), dist_gpd(0, 1, 0.3), dist_laplace(1, 2),
    exp(dist_normal()), -dist_gamma(2, 1),
    dist_truncated(dist_normal(), 1, 5)
  )
  for (i in seq_along(dists)) {
    x <- dists[i]
    q <- quantile(x, p, tail = "upper")[[1]]
    expect_equal(cdf(x, q, tail = "upper")[[1]], p, tolerance = 1e-6,
                 info = format(x))
  }

  # Moderate probabilities for numerically inverted modifiers
  p <- c(0.05, 0.3, 0.7)
  dists <- c(
    dist_mixture(dist_normal(), dist_normal(3), weights = c(0.4, 0.6)),
    dist_inflated(dist_normal(), 0.3, 1)
  )
  for (i in seq_along(dists)) {
    x <- dists[i]
    expect_equal(
      quantile(x, p, tail = "upper")[[1]],
      quantile(x, 1 - p)[[1]],
      tolerance = 1e-4, info = format(x)
    )
  }

  # Discrete distributions match quantile(1 - p)
  p <- c(0.05, 0.3, 0.5, 0.9)
  dists <- c(
    dist_poisson(3), dist_binomial(10, 0.4),
    dist_truncated(dist_poisson(4), 2, 7),
    dist_inflated(dist_poisson(3), 0.2)
  )
  for (i in seq_along(dists)) {
    x <- dists[i]
    expect_equal(
      quantile(x, p, tail = "upper")[[1]],
      quantile(x, 1 - p)[[1]],
      info = format(x)
    )
  }

  # Tabulated and sample based distributions match quantile(1 - p)
  expect_equal(
    quantile(dist_sample(list(1:100)), 0.1, tail = "upper"),
    quantile(dist_sample(list(1:100)), 0.9)
  )
  x <- dist_percentile(list(c(1, 2, 3, 4)), list(c(0, 25, 75, 100)))
  expect_equal(quantile(x, 0.1, tail = "upper"), quantile(x, 0.9))
  expect_equal(quantile(dist_degenerate(2), 0.1, tail = "upper"), 2)
})
