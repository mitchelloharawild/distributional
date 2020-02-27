test_that("hilo", {
  # defaults
  hl <- new_hilo()
  expect_length(hl, 0)

  # display
  expect_output(print(hl), "<hilo\\[0\\]>")
  expect_output(print(new_hilo(0,1,95)), "\\[0, 1\\]95")

  dist <- dist_normal()

  # hilo.distribution
  hl <- hilo(dist, 95)
  expect_length(hl, 1)
  expect_equal(hl, new_hilo(qnorm(0.025), qnorm(0.975), 95))

  # vec_math.hilo
  expect_equal(is.na(hl), FALSE)
  expect_equal(is.nan(hl), FALSE)

  # vec_arith.hilo
  expect_equal(
    hl*3+1,
    new_hilo(qnorm(0.025)*3+1, qnorm(0.975)*3+1, 95)
  )
  expect_equal(-hl, hl)
  expect_equal(-3*hl, hl/(1/3))
})
