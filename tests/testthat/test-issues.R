test_that("is.na() on <dist>[[1]] (#29)", {
  x <- c(dist_normal(0,1), NA)
  expect_equal(
    is.na(x),
    c(FALSE, TRUE)
  )

  expect_equal(
    is.na(x[[1]]),
    FALSE
  )

  expect_equal(
    is.na(x[[2]]),
    TRUE
  )
})
