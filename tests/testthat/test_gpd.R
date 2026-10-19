test_that("GPD", {
  dist <- dist_gpd(location = c(0, .5, 0), scale = c(1, 2, 3), shape = c(0, 0.1, 1.1))
  # Mean
  expect_equal(mean(dist), c(
    1,
    0.5 + 2 / 0.9, # location + scale/(1 - shape)
    Inf # Since shape >= 1
  ), tolerance = 0.0001)
  # Median
  expect_equal(median(dist), c(
    -log(0.5),
    0.5 + 2 * (2^0.1 - 1) / 0.1, # location + scale * (2^shape - 1) / shape
    3 * (2^1.1 - 1) / 1.1 # location + scale * (2^shape - 1) / shape
  ), tolerance = 0.0001)
  expect_equal(median(dist), quantile(dist, 0.5))
  # Variance
  expect_equal(distributional::variance(dist), c(
    1,
    2^2 / 0.9^2 / (1 - 2 * 0.1), # scale^2 / (1 - shape)^2 / (1 - 2 * shape)
    Inf # since shape >= 0.5
  ), tolerance = 0.0001)
  # Density
  at <- (0:20) / 10 + 1e-8 # Avoiding being on the boundary where evd gives wrong result
  expect_equal(density(dist, at), list(
    evd::dgpd(at, loc = 0, scale = 1, shape = 0),
    evd::dgpd(at, loc = 0.5, scale = 2, shape = 0.1),
    evd::dgpd(at, loc = 0, scale = 3, shape = 1.1)
  ))
  # CDF
  expect_equal(distributional::cdf(dist, at), list(
    evd::pgpd(at, loc = 0, scale = 1, shape = 0),
    evd::pgpd(at, loc = 0.5, scale = 2, shape = 0.1),
    evd::pgpd(at, loc = 0, scale = 3, shape = 1.1)
  ))
  # Quantiles
  p <- (1:19) / 20
  expect_equal(quantile(dist, p = p), list(
    evd::qgpd(p = p, loc = 0, scale = 1, shape = 0),
    evd::qgpd(p = p, loc = 0.5, scale = 2, shape = 0.1),
    evd::qgpd(p = p, loc = 0, scale = 3, shape = 1.1)
  ))
  # Generate
  set.seed(123)
  rand_dist <- distributional::generate(dist, times = 1e6)
  expect_equal(unlist(lapply(rand_dist[1:2], mean)),
    mean(dist)[1:2],
    tolerance = 0.01
  )
  expect_equal(unlist(lapply(rand_dist[1:2], var)),
    distributional::variance(dist)[1:2],
    tolerance = 0.01
  )
  expect_equal(unlist(lapply(rand_dist, median)),
    median(dist),
    tolerance = 0.01
  )
})

test_that("GPD support and density agree at the upper bound", {
  dist <- dist_gpd(0.1, 0.3, c(-0.5, -1, -2, 0.3))
  expect_equal(
    format(support(dist)),
    c("[0.1,0.7)", "[0.1,0.4]", "[0.1,0.25]", "[0.1,Inf)")
  )
  upper <- quantile(dist, 1)
  expect_equal(upper[1:3], vapply(field(support(dist), "lim")[1:3], `[`, numeric(1L), 2L))
  # Density at the upper bound is 0, 1/scale and Inf for shape -0.5, -1, -2
  expect_equal(density(dist[1], upper[1]), 0)
  expect_equal(density(dist[2], upper[2]), 1 / 0.3)
  expect_equal(density(dist[3], upper[3]), Inf)
  # Shape = -1 is uniform on [location, location + scale]
  expect_equal(
    density(dist_gpd(0, 1, -1), c(-0.1, 0, 0.5, 1, 1.1)),
    list(c(0, 1, 1, 1, 0))
  )
  # Interior values match the closed form
  at <- c(0.15, 0.2, 0.24)
  z <- (at - 0.1) / 0.3
  expect_equal(density(dist[3], at), list((1 - 2 * z)^(1 / 2 - 1) / 0.3))
})
