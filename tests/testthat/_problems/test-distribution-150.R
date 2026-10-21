# Extracted from test-distribution.R:150

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "distributional", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
mix <- dist_mixture(dist_normal(0, 1), dist_normal(5, 2), weights = c(0.3, 0.7))
dist <- c(mix, dist_poisson(3))
fam <- family(dist, recursive = TRUE)
expect_length(fam, 2L)
expect_identical(fam[[1]], list(family = "mixture", dist = list("normal", "normal")))
