test_that("Multinomial distribution", {
  p <- c(0.3, 0.5, 0.2)
  dist <- dist_multinomial(size = 4, prob = list(p))
  dimnames(dist) <- c("a", "b", "c")

  expect_equal(format(dist), "Multinomial(4)[3]")

  # quantiles

  # pdf
  expect_equal(density(dist, cbind(1, 2, 1)), dmultinom(c(1, 2, 1), 4, p))

  # cdf
  # For multinomial, CDF is the probability that each category count is <= given values
  # This requires summing probabilities over all valid outcomes within the bounds
  
  # CDF should be 1 when bounds allow all possible outcomes
  expect_equal(cdf(dist, cbind(4, 4, 4)), 1)
  
  # CDF at a specific point should be >= PDF at that point
  test_point <- cbind(1, 2, 1)
  expect_gte(cdf(dist, test_point), density(dist, test_point))
  
  # CDF should be monotonically increasing
  expect_gte(cdf(dist, cbind(2, 2, 2)), cdf(dist, cbind(1, 1, 1)))
  
  # CDF at tight bounds should match the single point probability if only one outcome is possible
  # When bounds are (0, 0, 4), only the outcome (0, 0, 4) is valid
  expect_equal(cdf(dist, cbind(0, 0, 4)), dmultinom(c(0, 0, 4), 4, p))
  
  # CDF at bounds (1, 1, 4) should include multiple valid outcomes:
  # (0,0,4), (0,1,3), (1,0,3), (1,1,2)
  valid_outcomes <- rbind(c(0,0,4), c(0,1,3), c(1,0,3), c(1,1,2))
  expected_cdf <- sum(apply(valid_outcomes, 1, function(x) dmultinom(x, 4, p)))
  expect_equal(cdf(dist, cbind(1, 1, 4)), expected_cdf)
  
  # CDF at impossible values (where sum of bounds < size) should be 0
  expect_equal(cdf(dist, cbind(0, 0, 0)), 0)

  # F(Finv(a)) ~= a

  # stats
  expect_equal(
    mean(dist),
    matrix(c(1.2, 2, 0.8), nrow = 1, dimnames = list(NULL, c("a", "b", "c")))
  )

  expect_equal(
    covariance(dist),
    list(
      matrix(
        c(0.84, -0.6, -0.24, -0.6, 1, -0.4, -0.24, -0.4, 0.64),
        nrow = 3, dimnames = list(NULL, c("a", "b", "c"))
      )
    )
  )
})

test_that("Multinomial distribution with multiple distributions", {
  size <- c(3, 4, 5)
  p <- list(c(.3, .7), c(.5, .5), c(.2, .8))
  dist <- dist_multinomial(size = size, prob = p)

  expect_equal(format(dist), c("Multinomial(3)[2]", "Multinomial(4)[2]", "Multinomial(5)[2]"))

  # density pairs rows of `at` with the distributions by position
  at <- rbind(c(1, 2), c(2, 2), c(0, 5))
  expect_equal(
    density(dist, list(at))[[1]],
    c(dmultinom(c(1, 2), 3, p[[1]]), dmultinom(c(2, 2), 4, p[[2]]), dmultinom(c(0, 5), 5, p[[3]]))
  )
  # a point outside the support has no mass
  expect_equal(density(dist, cbind(1, 2)), c(dmultinom(c(1, 2), 3, p[[1]]), 0, 0))

  expect_equal(mean(dist), do.call(rbind, Map(`*`, size, p)))
  expect_equal(variance(dist), do.call(rbind, Map(function(s, p) s * p * (1 - p), size, p)))
  expect_equal(
    covariance(dist),
    lapply(1:3, function(i) covariance(dist[i])[[1]])
  )
  expect_equal(
    cdf(dist, cbind(1, 3)),
    vapply(1:3, function(i) cdf(dist[i], cbind(1, 3)), numeric(1))
  )
  expect_equal(
    quantile(dist, 0.5),
    do.call(rbind, Map(function(s, p) qbinom(0.5, s, p), size, p))
  )

  draws <- generate(dist, list(c(2, 3, 4)))
  expect_equal(vapply(draws, nrow, integer(1)), c(2L, 3L, 4L))
  expect_equal(rowSums(draws[[3]]), rep(5, 4))
})

test_that("Multinomial distribution with different numbers of categories", {
  dist <- dist_multinomial(size = c(3, 4), prob = list(c(.3, .7), c(.2, .3, .5)))

  expect_equal(format(dist), c("Multinomial(3)[2]", "Multinomial(4)[3]"))
  expect_equal(
    mean(dist),
    rbind(c(.9, 2.1, NA), c(.8, 1.2, 2))
  )
  expect_equal(variance(dist)[2, ], c(.64, .84, 1))
  expect_equal(lengths(generate(dist, list(c(2, 1)))), c(4L, 3L))
  expect_equal(vapply(covariance(dist), nrow, integer(1)), c(2L, 3L))
  expect_equal(ncol(generate(dist, list(c(2, 1)))[[2]]), 3L)
})
