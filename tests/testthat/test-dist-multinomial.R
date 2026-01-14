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
