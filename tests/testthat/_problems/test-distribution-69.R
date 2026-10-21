# Extracted from test-distribution.R:69

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "distributional", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
dist <- dist_inflated(
    dist_negative_binomial(c(10, 20), c(0.6, 0.4)),
    prob = 0.5
  )
