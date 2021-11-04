dist <- c(dist_normal(0, 1), dist_beta(5, 1))

test_that("geom_hilo_ribbon()", {
  dist <- dist_normal(1:3, 1:3)
  p <- ggplot2::ggplot(
    data.frame(x = rep(1:3, 2), interval = c(hilo(dist, 80), hilo(dist, 95)))
  ) +
    geom_hilo_ribbon(ggplot2::aes(x = x, hilo = interval))
  expect_silent(print(p))
  expect_length(
    ggplot2::layer_data(p)$hilo,
    3*2
  )
  expect_equal(
    ggplot2::layer_data(p)$hilo,
    c(hilo(dist, 80), hilo(dist, 95))[c(1,4,2,5,3,6)]
  )
})
test_that("geom_hilo_linerange()", {
  dist <- dist_normal(1:3, 1:3)
  p <- ggplot2::ggplot(
    data.frame(x = rep(1:3, 2), interval = c(hilo(dist, 80), hilo(dist, 95)))
  ) +
    geom_hilo_linerange(ggplot2::aes(x = x, hilo = interval))
  expect_silent(print(p))
  expect_length(
    ggplot2::layer_data(p)$hilo,
    3*2
  )
  expect_equal(
    ggplot2::layer_data(p)$hilo,
    c(hilo(dist, 80), hilo(dist, 95))
  )
})

