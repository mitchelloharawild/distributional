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
  #                   Returns a list containing values for each p.
  expect_equal(
    quantile(dist, c(0.5, 0.9)),
    mapply({function(mean, sd) qnorm(c(0.5, 0.9), mean, sd)},
           mean = 1:10, sd = 1:10, SIMPLIFY = FALSE)
  )

  # p = ppoints(10):  apply each p to each element of <dist> (no recycling)
  #                   Returns a list for each distribution with the 10 quantiles.
  expect_equal(
    quantile(dist, ppoints(10)),
    mapply({function(mean, sd) qnorm(ppoints(10), mean, sd)},
           mean = 1:10, sd = 1:10, SIMPLIFY = FALSE)
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
    new_data_frame(list(a = qnorm(ppoints(10), 1:10, 1:10), b = quantile(dist, 0.5)))
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
    list(matrix(c(1, 3.5631031310892, 2, 4.21971242404268), ncol = 2, dimnames = list(NULL, c("a", "b"))),
         matrix(c(3, 5.86563641722901, 5, 7.5631031310892), ncol = 2, dimnames = list(NULL, c("a", "b"))))
  )

  expect_equal(
    quantile(dist, c(0.5, 0.9, 0.95)),
    list(
      matrix(c(1, 3.5631031310892, 4.28970725390294, 2, 4.21971242404268, 4.84897005289389),
             ncol = 2, dimnames = list(NULL, c("a","b"))),
      matrix(c(3, 5.86563641722901, 6.67800452290057, 5, 7.5631031310892, 8.28970725390294),
             ncol = 2, dimnames = list(NULL, c("a", "b")))
    )
  )

  expect_equal(
    quantile(dist, list(single = 0.5, varied = c(0.8, 0.3))),
    new_data_frame(
      list(single = quantile(dist, 0.5), varied = rbind(quantile(dist[1], 0.8), quantile(dist[2], 0.3)))
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
    covariance(dist),
    list(
      matrix(c(4,2,2,3), nrow = 2),
      matrix(c(5,1,1,4), nrow = 2)
    )
  )
})
