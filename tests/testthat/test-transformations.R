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

