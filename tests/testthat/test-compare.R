test_that("== and != compare the family and parameters", {
  x <- dist_normal(1:3)
  expect_identical(x == dist_normal(2), c(FALSE, TRUE, FALSE))
  expect_identical(dist_normal(2) == x, c(FALSE, TRUE, FALSE))
  expect_identical(x != dist_normal(2), c(TRUE, FALSE, TRUE))
  expect_identical(x == x, rep(TRUE, 3))

  # Same parameters in a different family
  expect_false(dist_normal(1, 1) == dist_gamma(1, 1))
  expect_true(dist_normal(1, 1) != dist_gamma(1, 1))

  # Mixed families are paired by position
  y <- c(dist_normal(1), dist_gamma(2, 1), dist_poisson(3))
  expect_identical(
    y == c(dist_normal(1), dist_normal(1), dist_poisson(3)),
    c(TRUE, FALSE, TRUE)
  )
  expect_identical(y == dist_gamma(2, 1), c(FALSE, TRUE, FALSE))

  # Transformed distributions compare the base distribution and transformation
  expect_true(exp(dist_gamma(1, 1)) == exp(dist_gamma(1, 1)))
  expect_false(exp(dist_gamma(1, 1)) == exp(dist_gamma(2, 1)))
  expect_false(exp(dist_gamma(1, 1)) == log(dist_gamma(1, 1)))

  # Nested and multivariate distributions
  expect_identical(
    dist_truncated(dist_normal(), 0, 1) == dist_truncated(dist_normal(), 0, 1:2),
    c(TRUE, FALSE)
  )
  m <- dist_mixture(dist_normal(), dist_gamma(1, 1), weights = c(0.5, 0.5))
  expect_true(m == m)
  expect_false(m == dist_mixture(dist_normal(1), dist_gamma(1, 1), weights = c(0.5, 0.5)))
  mv <- dist_multivariate_normal(list(1:2), list(diag(2)))
  expect_true(mv == mv)
  expect_false(mv == dist_multivariate_normal(list(1:2), list(diag(2) * 2)))
  expect_identical(dist_sample(list(1:3)) == dist_sample(list(1:3, 1:4)), c(TRUE, FALSE))
})

test_that("== casts numbers to degenerate distributions", {
  expect_true(dist_degenerate(1) == 1)
  expect_true(1 == dist_degenerate(1))
  expect_false(dist_degenerate(1) == 2)
  expect_false(dist_normal(1) == 1)
  expect_true(dist_normal(1) != 1)
  expect_identical(dist_degenerate(1:3) == c(1, 5, 3), c(TRUE, FALSE, TRUE))
  expect_identical(dist_normal(1:3) == 1, rep(FALSE, 3))
})

test_that("== with missing distributions is NA", {
  x <- c(dist_normal(1), NA, dist_poisson(3))
  expect_identical(x == dist_normal(1), c(TRUE, NA, FALSE))
  expect_identical(x != dist_normal(1), c(FALSE, NA, TRUE))
  expect_identical(x == NA, rep(NA, 3))
  expect_identical(dist_missing(2) == dist_normal(), c(NA, NA))
  expect_identical(dist_normal() == dist_missing(2), c(NA, NA))
})

test_that("== handles zero-length and rep-compressed inputs", {
  expect_identical(dist_normal(1:2)[0] == 1, logical())
  expect_identical(dist_normal(1:2)[0] == dist_normal(), logical())
  x <- rep(dist_normal(1:2), 3)
  expect_identical(x == dist_normal(2), rep(c(FALSE, TRUE), 3))
  expect_identical(x == rep(dist_normal(1:2), each = 3), c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE))
})

test_that("== errors for incompatible operands", {
  expect_error(dist_normal() == "a", "convert")
  expect_error(dist_normal(1:3) == 1:2, "recycle")
})

test_that("arithmetic still dispatches through Ops", {
  x <- dist_normal(1:3)
  expect_identical(format(x + 1), format(dist_normal(2:4)))
  expect_identical(format(1 - x), format(dist_normal(0:-2)))
  expect_identical(format(-x), format(dist_normal(-(1:3))))
})
