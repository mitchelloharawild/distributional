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

test_that("GEV is vectorised over mixed shape signs", {
  loc <- c(0, 1, -1, 2)
  scale <- c(1, 2, 3, 0.5)
  shape <- c(-0.2, 0, 0.3, 1.5)
  ev <- function(f, a) as.vector(mapply(f, a, loc = loc, scale = scale, shape = shape))
  ev_d <- function(a) ev(evd::dgev, a)
  ev_p <- function(a) ev(evd::pgev, a)
  ev_q <- function(a) ev(evd::qgev, a)
  dist <- dist_gev(loc, scale, shape)
  at <- c(0.5, 1, 2, 3)
  p <- c(0.2, 0.5, 0.7, 0.9)

  expect_equal(density(dist, list(at))[[1]], ev_d(at))
  expect_equal(log_density(dist, list(at))[[1]], log(ev_d(at)))
  expect_equal(cdf(dist, list(at))[[1]], ev_p(at))
  expect_equal(cdf(dist, list(at), tail = "upper")[[1]], 1 - ev_p(at))
  expect_equal(quantile(dist, list(p))[[1]], ev_q(p))
  expect_equal(quantile(dist, 0.3), ev_q(0.3))
  expect_equal(median(dist), ev_q(0.5))

  # Moments agree with the single distribution results
  expect_no_warning(m <- mean(dist))
  expect_no_warning(v <- variance(dist))
  expect_equal(m, vapply(vec_chop(dist), mean, 1))
  expect_equal(v, vapply(vec_chop(dist), variance, 1))
  expect_equal(m[4], Inf)
  expect_equal(v[4], Inf)

  # Support depends on the sign of the shape
  s <- support(dist)
  expect_equal(vec_size(s), 4L)
  expect_equal(
    field(s, "lim"),
    list(c(-Inf, 5), c(-Inf, Inf), c(-1 - 3 / 0.3, Inf), c(2 - 0.5 / 1.5, Inf))
  )
  expect_equal(density(dist, list(rep(-100, 4)))[[1]], c(0, evd::dgev(-100, 1, 2, 0), 0, 0))

  # generate() draws the requested sizes per distribution
  set.seed(1)
  draws <- generate(dist, list(c(1L, 0L, 3L, 2L)))
  expect_equal(lengths(draws), c(1L, 0L, 3L, 2L))
  expect_equal(format(dist)[2], "GEV(1, 2, 0)")
})
