test_that("Horseshoe distribution - format and structure", {
  dist <- dist_horseshoe(lambda = c(0.5, 1, 2), tau = 1)

  # length
  expect_length(dist, 3L)

  # format
  expect_equal(format(dist), c("HS(0.5, 1)", "HS(1.0, 1)", "HS(2.0, 1)"))
})

test_that("Horseshoe distribution - density", {
  lambda <- 1
  tau <- 1
  dist <- dist_horseshoe(lambda = lambda, tau = tau)

  x_vals <- c(-2, -1, 0, 1, 2)

  expect_equal(
    density(dist, x_vals)[[1]],
    LaplacesDemon::dhs(x_vals, lambda, tau)
  )
  expect_equal(
    density(dist, x_vals, log = TRUE)[[1]],
    LaplacesDemon::dhs(x_vals, lambda, tau, log = TRUE)
  )
})

test_that("Horseshoe distribution - support", {
  dist <- dist_horseshoe(lambda = 1, tau = 1)
  s <- support(dist)

  expect_s3_class(s, "support_region")
  expect_equal(format(s), "R")
})

test_that("Horseshoe distribution - generate", {
  dist <- dist_horseshoe(lambda = 1, tau = 1)

  set.seed(42)
  samples <- generate(dist, 100)[[1]]

  expect_length(samples, 100L)
  expect_type(samples, "double")
  # Horseshoe is symmetric around 0 -- large sample mean should be near 0
  expect_lt(abs(mean(samples)), 1)
})

test_that("Horseshoe distribution - input validation", {
  expect_error(
    dist_horseshoe(lambda = -1, tau = 1),
    "lambda"
  )
  expect_error(
    dist_horseshoe(lambda = 0, tau = 1),
    "lambda"
  )
  expect_error(
    dist_horseshoe(lambda = 1, tau = -1),
    "tau"
  )
  expect_error(
    dist_horseshoe(lambda = 1, tau = 0),
    "tau"
  )
})

test_that("Horseshoe distribution - heavier tails with larger lambda", {
  # With larger lambda the density at x=0 is lower (more spread),
  # but the tails are heavier
  dist_narrow <- dist_horseshoe(lambda = 0.1, tau = 1)
  dist_wide   <- dist_horseshoe(lambda = 5,   tau = 1)

  # Larger lambda => lower peak at 0
  expect_gt(density(dist_narrow, 0), density(dist_wide, 0))
})

test_that("Horseshoe distribution - larger tau scales distribution", {
  dist_small <- dist_horseshoe(lambda = 1, tau = 0.5)
  dist_large <- dist_horseshoe(lambda = 1, tau = 2)

  # Larger tau => more spread => lower peak at 0
  expect_gt(density(dist_small, 0), density(dist_large, 0))
})

test_that("Horseshoe distribution is vectorised over several distributions", {
  dist <- dist_horseshoe(c(0.5, 1, 2), c(1, 2, 3))
  at <- c(-1, 0, 1)

  expect_equal(density(dist, list(at))[[1]], LaplacesDemon::dhs(at, c(0.5, 1, 2), c(1, 2, 3)))
  expect_equal(vec_size(support(dist)), 3L)

  # generate() returns one draw vector per distribution
  set.seed(1)
  draws <- generate(dist, list(c(2L, 0L, 5L)))
  expect_equal(lengths(draws), c(2L, 0L, 5L))

  # The numerical cdf() is Monte Carlo, so compare with a tolerance
  set.seed(1)
  p <- cdf(dist, 0)
  expect_equal(p, rep(0.5, 3), tolerance = 0.02)
})
