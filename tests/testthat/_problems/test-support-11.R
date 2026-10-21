# Extracted from test-support.R:11

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "distributional", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
s <- support(c(dist_normal(),
               dist_gamma(1, 1),
               dist_gamma(2, 1),
               dist_lognormal(),
               dist_beta(1, 1),
               dist_beta(1, 2),
               dist_beta(2, 1),
               dist_beta(2, 2),
               exp(dist_wrap('norm')),
               2*atan(dist_normal())))
