dist <- c(dist_normal(0, 1), dist_beta(5, 1))

test_that("autoplot.distribution pdf", {
  p <- autoplot(dist, type = "pdf")
  expect_silent(print(p))
  expect_length(
    ggplot2::layer_data(p)$y,
    100*length(dist)
  )
  expect_equal(
    vapply(ggplot2::layer_data(p)$x[1:100], density, numeric(1L), x = dist[1]),
    ggplot2::layer_data(p)$y[1:100]
  )
  expect_equal(
    vapply(ggplot2::layer_data(p)$x[101:200], density, numeric(1L), x = dist[2]),
    ggplot2::layer_data(p)$y[101:200]
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_identical(
    p_built$plot$labels[c("x", "y", "colour")],
    list(x = "x", y = "pdf", colour = "distribution")
  )
})

test_that("autoplot.distribution cdf", {
  p <- autoplot(dist, type = "cdf")
  expect_silent(print(p))
  expect_length(
    ggplot2::layer_data(p)$y,
    100*length(dist)
  )
  expect_equal(
    vapply(ggplot2::layer_data(p)$x[1:100], cdf, numeric(1L), x = dist[1]),
    ggplot2::layer_data(p)$y[1:100]
  )
  expect_equal(
    vapply(ggplot2::layer_data(p)$x[101:200], cdf, numeric(1L), x = dist[2]),
    ggplot2::layer_data(p)$y[101:200]
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_identical(
    p_built$plot$labels[c("x", "y", "colour")],
    list(x = "x", y = "cdf", colour = "distribution")
  )
})

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

