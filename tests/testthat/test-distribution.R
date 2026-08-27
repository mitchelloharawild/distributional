test_that("is_distribution", {
  expect_false(is_distribution(iris))
  expect_true(is_distribution(dist_normal()))
  expect_false(is_distribution(NULL))
  expect_false(is_distribution(0))

  df <- data.frame(a = 1:10, b = dist_poisson(1:10), c = dist_normal(1:10))
  expect_true(all(sapply(df, is_distribution) == c(FALSE, TRUE, TRUE)))
})

test_that("variance() works correctly on vectors/matrices of different dimension", {
  x = 1:8

  expect_equal(variance(x), 6)
  expect_equal(variance(matrix(x, nrow = 2)), rep(0.5, 4))
})

test_that("variance() throws an error on non-numeric objects", {
  expect_error(variance("foo"))
})

test_that("parameters() returns distribution-valued parameters unmodified by default", {
  dist <- dist_inflated(dist_negative_binomial(10, 0.6), prob = 0.5)
  p <- parameters(dist)

  expect_identical(names(p), c("dist", "x", "p"))
  expect_true(is_distribution(p$dist))
  expect_equal(p$x, 0)
  expect_equal(p$p, 0.5)
})

test_that("parameters(recursive = TRUE) expands distribution-valued parameters", {
  dist <- dist_inflated(dist_negative_binomial(10, 0.6), prob = 0.5)
  p <- parameters(dist, recursive = TRUE)

  expect_identical(names(p), c("dist.n", "dist.p", "x", "p"))
  expect_false(any(vapply(p, is_distribution, logical(1L))))
  expect_equal(p$dist.n, 10)
  expect_equal(p$dist.p, 0.6)
  expect_equal(p$x, 0)
  expect_equal(p$p, 0.5)

  # Non-recursive columns are unaffected
  dist2 <- c(dist_normal(1:2), dist_poisson(3))
  expect_identical(parameters(dist2, recursive = TRUE), parameters(dist2))
})

test_that("parameters(recursive = TRUE) fully expands multiple levels of nesting", {
  base <- dist_negative_binomial(10, 0.6)
  shifted <- dist_transformed(base, function(x) x + 1, function(x) x - 1)
  dist <- dist_inflated(shifted, prob = 0.5, x = 0)

  p <- parameters(dist, recursive = TRUE)

  expect_false(any(vapply(p, is_distribution, logical(1L))))
  expect_true(all(!vapply(p, is.data.frame, logical(1L))))
  expect_identical(
    names(p),
    c("dist.dist.n", "dist.dist.p", "dist.transform", "dist.inverse", "x", "p")
  )
  expect_equal(p$dist.dist.n, 10)
  expect_equal(p$dist.dist.p, 0.6)
})

test_that("parameters(recursive = TRUE) works with vectorised distributions", {
  dist <- dist_inflated(
    dist_negative_binomial(c(10, 20), c(0.6, 0.4)),
    prob = 0.5
  )
  p <- parameters(dist, recursive = TRUE)

  expect_identical(names(p), c("dist.n", "dist.p", "x", "p"))
  expect_equal(p$dist.n, c(10, 20))
  expect_equal(p$dist.p, c(0.6, 0.4))
  expect_equal(nrow(p), 2L)
})

test_that("parameters(recursive = TRUE) handles multiple distribution-valued parameters", {
  # A distribution with two distribution-valued parameters either side of a
  # plain one, to check that expanding the first doesn't misplace the
  # second (or the plain parameter between them).
  dist <- new_dist(
    a = dist_negative_binomial(10, 0.6),
    mid = 99,
    b = dist_poisson(3),
    class = "dist_test_two_nested"
  )
  p <- parameters(dist, recursive = TRUE)

  expect_identical(names(p), c("a.n", "a.p", "mid", "b.l"))
  expect_equal(p$a.n, 10)
  expect_equal(p$a.p, 0.6)
  expect_equal(p$mid, 99)
  expect_equal(p$b.l, 3)
})
