test_that("hilo of transformed distributions", {
  expect_identical(
    hilo(exp(dist_normal())),
    exp(hilo((dist_normal())))
  )
})

test_that("chains of transformations", {
  expect_identical(
    hilo(dist_normal()),
    hilo(log(exp(dist_normal())))
  )

  expect_output(
    print(exp(dist_normal())-1),
    "t\\(N\\(0, 1\\)\\)"
  )
})

test_that("handling of transformation arguments", {
  expect_identical(
    hilo(logb(dist_normal(5, 1), base = 10)),
    logb(hilo(dist_normal(5, 1)), base = 10)
  )

  expect_identical(
    hilo(10^logb(dist_normal(5, 1), base = 10)),
    10^logb(hilo(dist_normal(5, 1)), base = 10)
  )
})

test_that("LogNormal distributions", {
  dist <- dist_transformed(dist_normal(0, 0.5), exp, log)

  # format
  expect_equal(format(dist), sprintf("t(%s)", format(dist_normal(0, 0.5))))

  # quantiles
  expect_equal(
    quantile(dist, 0.1),
    qlnorm(0.1, 0, 0.5)
  )
  expect_equal(
    quantile(dist, 0.5),
    qlnorm(0.5, 0, 0.5)
  )

  # pdf
  expect_equal(
    density(dist, 1),
    dlnorm(1, 0, 0.5)
  )
  expect_equal(
    density(dist, 20),
    dlnorm(20, 0, 0.5)
  )

  # cdf
  expect_equal(
    cdf(dist, 4),
    plnorm(4, 0, 0.5)
  )
  expect_equal(
    cdf(dist, 90),
    plnorm(90, 0, 0.5)
  )

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.372)), 0.372, tolerance = 1e-3)

  # stats (approximate due to bias adjustment method)
  expect_equal(mean(dist), exp(0.25/2), tolerance = 0.01)
  expect_equal(variance(dist), (exp(0.25) - 1)*exp(0.25), tolerance = 0.1)
})
