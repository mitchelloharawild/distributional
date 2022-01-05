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
