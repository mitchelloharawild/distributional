test_that("Multinomial distribution", {
  p <- c(0.3, 0.5, 0.2)
  dist <- dist_multinomial(size = 4, prob = list(p))
  dimnames(dist) <- c("a", "b", "c")

  expect_equal(format(dist), "Multinomial(4)[3]")

  # quantiles

  # pdf
  expect_equal(density(dist, cbind(1, 2, 1)), dmultinom(c(1, 2, 1), 4, p))

  # cdf

  # F(Finv(a)) ~= a

  # stats
  expect_equal(
    mean(dist),
    matrix(c(1.2, 2, 0.8), nrow = 1, dimnames = list(NULL, c("a", "b", "c")))
  )

  expect_equal(
    covariance(dist),
    list(matrix(c(0.84, -0.6, -0.24, -0.6, 1, -0.4, -0.24, -0.4, 0.64), nrow = 3)))
})
