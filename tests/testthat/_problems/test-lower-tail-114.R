# Extracted from test-lower-tail.R:114

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "distributional", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
expect_equal(
    cdf(dist_wrap("norm", mean = 1), 11, tail = "upper"),
    stats::pnorm(11, 1, lower.tail = FALSE)
  )
