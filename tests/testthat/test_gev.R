test_that("GEV", {
  dist <- dist_gev(location = c(0, .5, 0), scale = c(1, 2, 3), shape = c(0, 0.1, 1.1))
  euler <- 0.57721566490153286 # Euler's constant
  # Mean
  expect_equal(mean(dist), c(
    euler,
    0.5 + 2 * (gamma(0.9) - 1) / 0.1, # location + scale*(gamma(1 - shape) - 1)/shape
    Inf # Since shape >= 1
  ), tolerance = 0.0001)
  # Median
  expect_equal(median(dist), c(
    -log(log(2)),
    0.5 + 2 * (log(2)^(-0.1) - 1) / 0.1, # location + scale*(log(2)^(-shape) - 1)/shape
    3 * (log(2)^(-1.1) - 1) / 1.1 # location + scale*(log(2)^(-shape) - 1)/shape
  ), tolerance = 0.0001)
  expect_equal(median(dist), quantile(dist, 0.5))
  # Variance
  expect_equal(distributional::variance(dist), c(
    pi^2 / 6,
    2^2 * (gamma(1 - 2 * 0.1) - gamma(1 - 0.1)^2) / 0.1^2, # scale^2 * (g2 - g1^2)/shape^2
    Inf # since shape >= 0.5
  ), tolerance = 0.0001)
  # Density
  at <- (0:20) / 10
  expect_equal(density(dist, at), list(
    evd::dgev(at, loc = 0, scale = 1, shape = 0),
    evd::dgev(at, loc = 0.5, scale = 2, shape = 0.1),
    evd::dgev(at, loc = 0, scale = 3, shape = 1.1)
  ))
  # CDF
  expect_equal(distributional::cdf(dist, at), list(
    evd::pgev(at, loc = 0, scale = 1, shape = 0),
    evd::pgev(at, loc = 0.5, scale = 2, shape = 0.1),
    evd::pgev(at, loc = 0, scale = 3, shape = 1.1)
  ))
  # Quantiles
  p <- (1:19) / 20
  expect_equal(quantile(dist, p = p), list(
    evd::qgev(p = p, loc = 0, scale = 1, shape = 0),
    evd::qgev(p = p, loc = 0.5, scale = 2, shape = 0.1),
    evd::qgev(p = p, loc = 0, scale = 3, shape = 1.1)
  ))
  # Generate
  set.seed(123)
  rand_dist <- distributional::generate(dist, times = 1e6)
  expect_equal(lapply(rand_dist[1:2], mean) |> unlist(),
    mean(dist)[1:2],
    tolerance = 0.01
  )
  expect_equal(lapply(rand_dist[1:2], var) |> unlist(),
    distributional::variance(dist)[1:2],
    tolerance = 0.01
  )
  expect_equal(lapply(rand_dist, median) |> unlist(),
    median(dist),
    tolerance = 0.01
  )
})
