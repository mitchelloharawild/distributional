test_that("hilo of transformed distributions", {
  expect_identical(
    hilo(exp(dist_normal())),
    exp(hilo((dist_normal())))
  )
})
