test_that("is_distribution", {
  expect_false(is_distribution(iris))
  expect_true(is_distribution(dist_normal()))
  expect_false(is_distribution(NULL))
  expect_false(is_distribution(0))
  
  df <- data.frame(a = 1:10, b = dist_poisson(1:10), c = dist_normal(1:10))
  expect_true(all(sapply(df, is_distribution) == c(FALSE, TRUE, TRUE)))
})
