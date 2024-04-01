test_that("symbolic differentiation works", {
  d <- exp(-log(dist_gamma(2,1)))
  expect_silent(density(d, 1))
  expect_true(quantile(d,0) < quantile(d,1))
})
