# Extracted from test-lower-tail.R:137

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "distributional", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
x <- dist_mixture(dist_normal(), dist_normal(1), weights = c(0.3, 0.7))
expect_equal(
    cdf(x, 12, tail = "upper"),
    0.3 * stats::pnorm(12, lower.tail = FALSE) +
      0.7 * stats::pnorm(11, lower.tail = FALSE)
  )
