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

# The probability of a Bernoulli result
prob <- function(x) mean(x)

test_that("ordering comparisons of continuous distributions use the cdf", {
  x <- dist_normal(1, 2)
  q <- 0.5
  p <- pnorm(q, 1, 2)
  expect_equal(prob(x < q), p)
  expect_equal(prob(x <= q), p)
  expect_equal(prob(x > q), 1 - p)
  expect_equal(prob(x >= q), 1 - p)
  expect_equal(prob(q > x), p)
  expect_equal(prob(q >= x), p)
  expect_equal(prob(q < x), 1 - p)
  expect_equal(prob(q <= x), 1 - p)

  g <- dist_gamma(2, 3)
  expect_equal(prob(g < 1), pgamma(1, 2, 3))
  expect_equal(prob(1 < g), pgamma(1, 2, 3, lower.tail = FALSE))
})

test_that("ordering comparisons include the mass at the threshold for discrete distributions", {
  x <- dist_poisson(3)
  expect_equal(prob(x < 2), ppois(1, 3))
  expect_equal(prob(x <= 2), ppois(2, 3))
  expect_equal(prob(x > 2), ppois(2, 3, lower.tail = FALSE))
  expect_equal(prob(x >= 2), ppois(1, 3, lower.tail = FALSE))
  expect_equal(prob(2 > x), ppois(1, 3))
  expect_equal(prob(2 >= x), ppois(2, 3))
  expect_equal(prob(2 < x), ppois(2, 3, lower.tail = FALSE))
  expect_equal(prob(2 <= x), ppois(1, 3, lower.tail = FALSE))

  # Between the support points
  expect_equal(prob(x < 2.5), ppois(2, 3))
  expect_equal(prob(x >= 2.5), ppois(2, 3, lower.tail = FALSE))

  b <- dist_binomial(10, 0.3)
  expect_equal(prob(b >= 3), pbinom(2, 10, 0.3, lower.tail = FALSE))
  expect_equal(prob(b < 3), pbinom(2, 10, 0.3))

  expect_equal(prob(dist_degenerate(1) >= 1), 1)
  expect_equal(prob(dist_degenerate(1) > 1), 0)
  expect_equal(prob(dist_degenerate(1) <= 1), 1)
  expect_equal(prob(dist_degenerate(1) < 1), 0)

  s <- dist_sample(list(c(1, 2, 2, 3)))
  expect_equal(prob(s >= 2), 0.75)
  expect_equal(prob(s > 2), 0.25)
  expect_equal(prob(s < 2), 0.25)
  expect_equal(prob(s <= 2), 0.75)

  # Transformed discrete distributions use the mass of the base distribution
  y <- 2 * dist_poisson(3)
  expect_equal(prob(y >= 4), ppois(1, 3, lower.tail = FALSE))
  expect_equal(prob(y < 4), ppois(1, 3))
})

test_that("ordering comparisons pair thresholds and mixed families by position", {
  x <- c(dist_normal(0, 1), dist_poisson(2), dist_gamma(2, 1))
  q <- c(1, 2, 3)
  expect_equal(
    prob(x >= q),
    c(pnorm(1, lower.tail = FALSE), ppois(1, 2, lower.tail = FALSE), pgamma(3, 2, 1, lower.tail = FALSE))
  )
  expect_equal(
    prob(q > x),
    c(pnorm(1), ppois(1, 2), pgamma(3, 2, 1))
  )
  expect_equal(prob(dist_normal(1:3) < 2), pnorm(2, 1:3))
  expect_equal(prob(dist_poisson(3) <= 0:2), ppois(0:2, 3))
  expect_error(dist_normal(1:3) < 1:2, "recycle")
})

test_that("ordering comparisons keep upper tail precision", {
  expect_equal(prob(dist_normal() > 10), pnorm(10, lower.tail = FALSE))
  expect_gt(prob(dist_normal() > 10), 0)
  expect_equal(prob(-10 > dist_normal()), pnorm(-10))
  expect_equal(prob(10 < dist_normal()), pnorm(10, lower.tail = FALSE))
})

test_that("comparing two distributions uses their difference", {
  expect_equal(
    prob(dist_normal(1, 1) > dist_normal(0, 2)),
    pnorm(0, 1, sqrt(5), lower.tail = FALSE)
  )
  expect_equal(
    prob(dist_normal(1:2) < dist_normal(0)),
    pnorm(0, 1:2, sqrt(2))
  )
  # Without a closed form the difference is convolved numerically
  expect_equal(
    prob(dist_gamma(2, 1) > dist_gamma(1, 1)),
    0.75, tolerance = 1e-3
  )
})

test_that("ordering comparisons with missing values are missing", {
  x <- c(dist_normal(), NA, dist_poisson(2))
  res <- x > c(0, 1, 2)
  expect_identical(is.na(res), c(FALSE, TRUE, FALSE))
  expect_equal(prob(res), c(0.5, NA, ppois(2, 2, lower.tail = FALSE)))
  expect_true(is.na(dist_normal() > NA))
  expect_identical(is.na(dist_missing(2) < 0), c(TRUE, TRUE))
  expect_identical(is.na(dist_normal() < dist_missing(2)), c(TRUE, TRUE))
})

test_that("ordering comparisons handle zero-length and rep-compressed inputs", {
  expect_length(dist_normal(1:2)[0] > 1, 0)
  expect_s3_class(dist_normal(1:2)[0] > 1, "distribution")
  expect_length(dist_normal() > numeric(), 0)
  x <- rep(dist_normal(1:2), 3)
  expect_equal(prob(x > 1), rep(pnorm(1, 1:2, lower.tail = FALSE), 3))
  expect_equal(prob(x <= 1:6), pnorm(1:6, rep(1:2, 3)))
})

test_that("ordering comparisons return Bernoulli distributions", {
  res <- dist_normal(0:1) > 0
  expect_true(is_distribution(res))
  expect_identical(family(res), c("bernoulli", "bernoulli"))
  expect_identical(format(res), c("Bernoulli(0.50)", "Bernoulli(0.84)"))
  expect_identical(format(dist_poisson(3) >= 2), format(dist_bernoulli(ppois(1, 3, lower.tail = FALSE))))
  expect_named(parameters(dist_normal() > 0), "p")
})

test_that("ordering comparisons error for unsupported operands", {
  expect_error(dist_normal() > "a", "Can't compare")
  expect_error(dist_multivariate_normal(list(1:2), list(diag(2))) > 0, "multivariate")
  expect_error(hilo(dist_normal()) < dist_normal(), "Can't compare")
})
