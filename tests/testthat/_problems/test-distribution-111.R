# Extracted from test-distribution.R:111

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "distributional", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
dist <- dist_inflated(dist_negative_binomial(10, 0.6), prob = 0.5)
