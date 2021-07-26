test_that("Recycling rules and output for applying multiple inputs over multiple univariate distributions", {
  # <dist> is a distribution vector of length 10
  dist <- dist_normal(1:10, 1:10)

  # p = 0.5:          apply p across all elements of <dist> (recycling p onto <dist>)
  #                   Returns a vector of length 10
  expect_equal(
    quantile(dist, 0.5),
    qnorm(0.5, 1:10, 1:10)
  )

  # p = c(0.5, 0.9):  Cannot recycle p (length 2) onto <dist> (length 10)
  #                   Returns an error.
  expect_error(
    quantile(dist, c(0.5, 0.9)),
    "Cannot recycle input"
  )

  # p = ppoints(10):  apply each p to each element of <dist> (no recycling)
  #                   Returns a vector of length 10
  expect_equal(
    quantile(dist, ppoints(10)),
    qnorm(ppoints(10), 1:10, 1:10)
  )

  # p = list(0.5):    equivalent to p = 0.5, but returns a list output
  #                   Returns a tibble with 1 vector column of length 10
  expect_equal(
    quantile(dist, list(a = 0.5)),
    new_data_frame(list(a = quantile(dist, 0.5)))
  )

  # p = list(c(0.5, 0.9)):
  #                   Cannot recycle p[[1]] (length 2) onto <dist> (length 10)
  #                   Returns an error.
  expect_error(
    quantile(dist, list(a = c(0.5, 0.9))),
    "Cannot recycle input"
  )

  # p = list(p1, 0.5): equivalent to df(quantile(<dist>, p1), quantile(<dist>, 0.5)).
  #                   Returns a tibble with 2 vector columns of length 10
  #                   Names of p are used in output.
  expect_equal(
    quantile(dist, list(a=ppoints(10), b=0.5)),
    new_data_frame(list(a = quantile(dist, ppoints(10)), b = quantile(dist, 0.5)))
  )
})

test_that("Recycling rules and output for applying multiple inputs over multiple multivariate distributions", {
  # <dist> is a bivariate distribution vector of length 2
  dist <- dist_multivariate_normal(mu = list(c(1,2), c(3,5)),
                                   sigma = list(matrix(c(4,2,2,3), ncol=2),
                                                matrix(c(5,1,1,4), ncol=2)))
  dimnames(dist) <- c("a", "b")

  expect_equal(
    quantile(dist, 0.5),
    matrix(c(1,3,2,5), nrow = 2, dimnames = list(NULL, c("a", "b")))
  )

  expect_equal(
    quantile(dist, c(0.5, 0.9)),
    matrix(c(1, 5.86563641722901, 2, 7.5631031310892), nrow = 2, dimnames = list(NULL, c("a", "b")))
  )

  expect_error(
    quantile(dist, c(0.5, 0.9, 0.95)),
    "Cannot recycle input"
  )

  expect_equal(
    quantile(dist, list(single = 0.5, varied = c(0.8, 0.3))),
    new_data_frame(
      list(single = quantile(dist, 0.5), varied = quantile(dist, c(0.8, 0.3)))
    )
  )

  expect_equal(
    density(dist, cbind(2, 3)),
    c(0.046649277604197, 0.0215708514518913)
  )

  expect_equal(
    mean(dist),
    matrix(c(1,3,2,5), nrow = 2, dimnames = list(NULL, c("a", "b")))
  )

  expect_equal(
    variance(dist),
    list(
      matrix(c(4,2,2,3), nrow = 2),
      matrix(c(5,1,1,4), nrow = 2)
    )
  )
})
