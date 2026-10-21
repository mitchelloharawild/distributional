test_that("Check zero inflation", {
  dist <- dist_inflated(dist_poisson(6), 0.33)

  expect_equal(format(dist), "0+Pois(6)")

  # quantiles
  expect_equal(quantile(dist, 0.1), 0)
  expect_equal(quantile(dist, 0.5), 4)

  # pdf
  expect_equal(density(dist, 0), 0.33 + 0.67*dpois(0, 6))
  expect_equal(density(dist, 3), 0.67*dpois(3, 6))

  # cdf
  expect_equal(cdf(dist, 0), 0.33 + 0.67*ppois(0, 6))
  expect_equal(cdf(dist, 3), 0.33 + 0.67*ppois(3, 6))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.52)), 0.52, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0.67*6)
  expect_equal(variance(dist), 0.67*6 + (0.33/0.67)*(0.67*6)^2)
})

test_that("Check non-zero inflation", {
  dist <- dist_inflated(dist_poisson(6), 0.33, 2)

  expect_equal(format(dist), "2+Pois(6)")

  # quantiles
  expect_equal(quantile(dist, 0), 0)
  expect_equal(quantile(dist, 0.1), 2)
  expect_equal(quantile(dist, 0.33), 2)
  expect_equal(quantile(dist, 0.5), 4)

  # pdf
  expect_equal(density(dist, 0), 0.67*dpois(0, 6))
  expect_equal(density(dist, 2), 0.33 + 0.67*dpois(2, 6))
  expect_equal(density(dist, 3), 0.67*dpois(3, 6))

  # cdf
  expect_equal(cdf(dist, 0), 0.67*ppois(0, 6))
  expect_equal(cdf(dist, 2), 0.33 + 0.67*ppois(2, 6))
  expect_equal(cdf(dist, 3), 0.33 + 0.67*ppois(3, 6))

  # stats
  expect_equal(mean(dist), 0.33*2 + 0.67*6)
})

test_that("continuous distribution inflated at 0 has mass below inflated point", {
  # Normal(0,1) inflated at 0: base dist has mass below 0, so quantile(p=0) = -Inf
  dist <- dist_inflated(dist_normal(0, 1), prob = 0.3, x = 0)

  # p=0 should be -Inf (base dist has support below 0)
  expect_equal(quantile(dist, 0), -Inf)

  # p=1 should be +Inf
  expect_equal(quantile(dist, 1), Inf)

  # F_inflated(0) = 0.3 + 0.7 * 0.5 = 0.65, so p just below 0.65 maps below 0
  # and p just above maps to 0 or above
  # F_base(0) = 0.5, so the jump in F_inflated covers (0.7*0.5, 0.3 + 0.7*0.5] = (0.35, 0.65]
  # p < 0.35 maps to base dist below 0; 0.35 < p <= 0.65 maps to inflated point; p > 0.65 maps above
  expect_lt(quantile(dist, 0.34), 0)
  expect_equal(quantile(dist, 0.35), 0)  # right at jump start: inflated point
  expect_gt(quantile(dist, 0.66), 0)

  # Scalar / vector consistency
  probs <- c(0, 0.1, 0.35, 0.50, 0.65, 0.80, 1)
  result <- quantile(dist, probs)[[1]]
  scalar <- vapply(probs, function(p) quantile(dist, p), numeric(1))
  expect_equal(result, scalar)
})

test_that("quantile.dist_inflated is vectorised over p (zero-inflated)", {
  # Reproduces the bug in issue #151
  dist <- dist_inflated(dist_poisson(4), prob = 0.30, x = 0)
  probs <- c(0.01, 0.10, 0.25, 0.50, 0.75, 0.90, 0.99)

  # Must not error and return a length-1 list containing all quantile values
  result <- quantile(dist, probs)
  expect_type(result, "list")
  expect_length(result, 1L)
  expect_length(result[[1]], length(probs))

  # Each element must equal the scalar result for the same probability
  scalar_results <- vapply(probs, function(p) quantile(dist, p), numeric(1))
  expect_equal(result[[1]], scalar_results)

  # Boundary: p = 0 and p = 1
  boundary <- quantile(dist, c(0, 1))
  expect_length(boundary[[1]], 2)

  # Values below the inflation mass return the inflated point (0)
  expect_equal(quantile(dist, 0.10), 0)   # 0.10 < prob = 0.30, so inflated point
  expect_true(quantile(dist, 0.99) > 0)   # well above inflation mass
})

test_that("quantile.dist_inflated is vectorised over p (non-zero-inflated)", {
  dist <- dist_inflated(dist_poisson(6), prob = 0.33, x = 2)
  probs <- c(0.05, 0.20, 0.40, 0.60, 0.80, 0.95)

  result <- quantile(dist, probs)
  expect_type(result, "list")
  expect_length(result, 1L)
  expect_length(result[[1]], length(probs))

  scalar_results <- vapply(probs, function(p) quantile(dist, p), numeric(1))
  expect_equal(result[[1]], scalar_results)

  # CDF round-trip: F(Q(p)) >= p for discrete distributions
  cdfs <- vapply(result[[1]], function(q) cdf(dist, q), numeric(1))
  expect_true(all(cdfs >= probs - 1e-9))
})

test_that("quantile.dist_inflated scalar and length-1 vector agree", {
  dist <- dist_inflated(dist_poisson(4), prob = 0.30, x = 0)
  expect_equal(quantile(dist, 0.5), quantile(dist, c(0.5))[[1]])
})

test_that("Inflated distributions are vectorised over mixed families", {
  dist <- c(
    dist_inflated(dist_poisson(6), 0.33),
    dist_inflated(dist_normal(0, 1), 0.3, x = 1),
    dist_inflated(dist_poisson(4), 0.2, x = 2)
  )
  each <- function(f) unlist(lapply(seq_along(dist), function(i) f(dist[i])))

  expect_equal(format(dist), c("0+Pois(6)", "1+N(0, 1)", "2+Pois(4)"))
  expect_equal(density(dist, 2), each(function(d) density(d, 2)))
  expect_equal(density(dist, 1), c(0.67 * dpois(1, 6), 0.3 + 0.7 * dnorm(1), 0.8 * dpois(1, 4)))
  expect_equal(cdf(dist, 1), each(function(d) cdf(d, 1)))
  expect_equal(cdf(dist, 1, tail = "upper"), each(function(d) cdf(d, 1, tail = "upper")))
  expect_equal(
    cdf(dist, list(c(0, 1, 2)))[[1]],
    c(cdf(dist[1], 0), cdf(dist[2], 1), cdf(dist[3], 2))
  )
  for (p in c(0, 0.1, 0.5, 0.9, 1)) {
    expect_equal(quantile(dist, p), each(function(d) quantile(d, p)))
    expect_equal(quantile(dist, p, tail = "upper"), each(function(d) quantile(d, p, tail = "upper")))
  }
  expect_equal(mean(dist), c(0.67 * 6, 0.3 + 0.7 * 0, 0.2 * 2 + 0.8 * 4))
  expect_equal(variance(dist)[1], 0.67 * 6 + (0.33 / 0.67) * (0.67 * 6)^2)
  expect_true(all(is.na(variance(dist)[2:3])))
  expect_equal(field(support(dist), "lim")[[2]], c(-Inf, Inf))
})

test_that("Inflated distributions are vectorised over the probability", {
  dist <- dist_inflated(dist_poisson(4), prob = c(0.1, 0.3, 0.6), x = 0)
  expect_equal(density(dist, 0), c(0.1, 0.3, 0.6) + (1 - c(0.1, 0.3, 0.6)) * dpois(0, 4))
  expect_equal(quantile(dist, 0.2), c(quantile(dist[1], 0.2), 0, 0))
  expect_equal(
    quantile(dist, c(0.2, 0.7))[[2]],
    c(quantile(dist[2], 0.2), quantile(dist[2], 0.7))
  )
  expect_equal(mean(dist), (1 - c(0.1, 0.3, 0.6)) * 4)
  expect_error(dist_inflated(dist_poisson(4), prob = c(0.1, 1.2)), "between 0 and 1")
})

test_that("Inflated distributions can inflate non-numeric values", {
  dist <- dist_inflated(dist_categorical(list(c(0.5, 0.5)), list(c("a", "b"))), 0.2, x = "a")
  expect_equal(density(dist, "a"), 0.2 + 0.8 * 0.5)
  expect_equal(density(dist, "b"), 0.8 * 0.5)
  expect_true(is.na(mean(dist)))
  set.seed(1)
  draws <- generate(dist, 20)[[1]]
  expect_type(draws, "character")
  expect_length(draws, 20)
})

test_that("Inflated distributions generate the right number of draws", {
  dist <- c(dist_inflated(dist_poisson(6), 0.5), dist_inflated(dist_normal(0, 1), 1, x = 3))
  set.seed(1)
  draws <- generate(dist, list(c(100, 5)))
  expect_length(draws[[1]], 100)
  expect_equal(draws[[2]], rep(3, 5))
  expect_gt(mean(draws[[1]] == 0), 0.4)
})
