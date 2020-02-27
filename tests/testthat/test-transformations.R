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
})
