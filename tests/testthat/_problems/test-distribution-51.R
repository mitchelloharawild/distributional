# Extracted from test-distribution.R:51

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "distributional", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
base <- dist_negative_binomial(10, 0.6)
shifted <- dist_transformed(base, function(x) x + 1, function(x) x - 1)
dist <- dist_inflated(shifted, prob = 0.5, x = 0)
