# Extracted from test-distribution.R:124

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "distributional", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
mix <- dist_mixture(dist_normal(0, 1), dist_normal(5, 2), weights = c(0.3, 0.7))
fam <- family(mix, recursive = TRUE)
expect_identical(
    fam,
    list(list(family = "mixture", dist = list("normal", "normal")))
  )
