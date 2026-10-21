# Extracted from test-lower-tail.R:339

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "distributional", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
new_lower_tail_test_dist <- function(cls, methods) {
  ns <- asNamespace("distributional")
  for (generic in names(methods)) {
    registerS3method(generic, cls, methods[[generic]], envir = ns)
  }
  new_dist(placeholder = NA_real_, class = cls)
}

# test -------------------------------------------------------------------------
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
expect_equal(
    quantile(dist_sample(list(1:100)), 0.1, tail = "upper"),
    quantile(dist_sample(list(1:100)), 0.9)
  )
x <- dist_percentile(list(c(1, 2, 3, 4)), list(c(0, 25, 75, 100)))
expect_equal(quantile(x, 0.1, tail = "upper"), quantile(x, 0.9))
expect_equal(quantile(dist_degenerate(2), 0.1, tail = "upper"), 2)
