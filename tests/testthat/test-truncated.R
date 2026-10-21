test_that("Truncated Normal distributions", {
  dist <- dist_truncated(dist_normal(0, 1), -5, 5)

  # format
  expect_equal(format(dist), sprintf("%s[-5,5]", format(dist_normal(0,1))))

  # quantiles
  expect_equal(
    quantile(dist, 0.1),
    -1.28155025885944 #dput(extraDistr::qtnorm(0.1, 0, 1, -5, 5))
  )
  expect_equal(
    quantile(dist, 0.5),
    -1.39145821233588e-16 #dput(extraDistr::qtnorm(0.5, 0, 1, -5, 5))
  )

  # pdf
  expect_equal(
    density(dist, 0),
    0.398942509116427, #dput(extraDistr::dtnorm(0, 0, 1, -5, 5))
  )
  expect_equal(
    density(dist, 3),
    0.00443185095273209, #dput(extraDistr::dtnorm(3, 0, 1, -5, 5))
  )

  # cdf
  expect_equal(cdf(dist, 0), 0.5)
  expect_equal(
    cdf(dist, 3),
    0.998650387846205, #dput(extraDistr::ptnorm(3, 0, 1, -5, 5))
  )

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.3)), 0.3, tolerance = 1e-6)

  # stats
  # expect_equal(mean(dist), ???)
  # expect_equal(variance(dist), ???)
})

test_that("Truncated Binomial distributions", {
  dist <- dist_truncated(dist_binomial(100, 0.83), 76, 86)
  # extraDistr truncates to (a, b], so a = 75 gives the closed support [76, 86]

  # format
  expect_equal(format(dist), sprintf("%s[76,86]", format(dist_binomial(100,0.83))))

  # quantiles
  expect_equal(
    quantile(dist, 0.1),
    78 #dput(extraDistr::qtbinom(0.1, 100, 0.83, 75, 86))
  )
  expect_equal(
    quantile(dist, 0.5),
    82 #dput(extraDistr::qtbinom(0.5, 100, 0.83, 75, 86))
  )

  # pdf
  expect_equal(density(dist, 75), 0)
  expect_equal(density(dist, 87), 0)
  expect_equal(
    density(dist, 80),
    0.0918887156304038, #dput(extraDistr::dtbinom(80, 100, 0.83, 75, 86))
  )
  expect_equal(
    density(dist, 85),
    0.1204919038506, #dput(extraDistr::dtbinom(85, 100, 0.83, 75, 86))
  )

  # cdf
  expect_equal(cdf(dist, 0), 0)
  expect_equal(cdf(dist, 75), 0)
  expect_equal(cdf(dist, 76), density(dist, 76))
  expect_equal(cdf(dist, 86), 1)
  expect_equal(
    cdf(dist, 80),
    0.277016503848048, #dput(extraDistr::ptbinom(80, 100, 0.83, 75, 86))
  )

  # F(Finv(a)) >= a > F(Finv(a) - 1)
  expect_gte(cdf(dist, quantile(dist, 0.372)), 0.372)
  expect_lt(cdf(dist, quantile(dist, 0.372) - 1), 0.372)

  # stats
  # expect_equal(mean(dist), ???)
  # expect_equal(variance(dist), ???)
})

test_that("Truncated discrete distributions keep the mass at the lower bound", {
  # Zero-truncated Poisson
  dist <- dist_truncated(dist_poisson(3), lower = 1)
  expect_equal(
    density(dist, 1:5)[[1]],
    dpois(1:5, 3) / (1 - dpois(0, 3))
  )
  expect_equal(density(dist, 0), 0)
  expect_equal(sum(density(dist, 0:100)[[1]]), 1)
  expect_equal(cdf(dist, 0), 0)
  expect_equal(cdf(dist, 1), density(dist, 1))
  expect_equal(
    cdf(dist, 1:5)[[1]],
    (ppois(1:5, 3) - dpois(0, 3)) / (1 - dpois(0, 3))
  )
  expect_equal(quantile(dist, 0), 1)
  expect_equal(quantile(dist, 0.01), 1)
  expect_equal(quantile(dist, 0.1), 1)
  expect_equal(quantile(dist, 0.2), 2)

  # Non-integer lower bound
  dist <- dist_truncated(dist_poisson(3), lower = 0.5)
  expect_equal(sum(density(dist, 0:100)[[1]]), 1)
  expect_equal(quantile(dist, 0), 1)

  # Two-sided truncation
  dist <- dist_truncated(dist_poisson(3), lower = 2, upper = 4)
  expect_equal(sum(density(dist, 0:10)[[1]]), 1)
  expect_equal(
    density(dist, 2:4)[[1]],
    dpois(2:4, 3) / sum(dpois(2:4, 3))
  )
  expect_equal(cdf(dist, 2), density(dist, 2))
  expect_equal(cdf(dist, 4), 1)
  expect_equal(quantile(dist, 0), 2)
  expect_equal(quantile(dist, 1), 4)

  dist <- dist_truncated(dist_binomial(10, 0.5), lower = 3, upper = 7)
  expect_equal(sum(density(dist, 0:10)[[1]]), 1)
  expect_equal(cdf(dist, 3), density(dist, 3))
  expect_equal(quantile(dist, 0), 3)
})

test_that("Truncated continuous distributions are unaffected by the lower bound mass", {
  dist <- dist_truncated(dist_normal(0, 1), lower = 0)
  expect_equal(cdf(dist, 0), 0)
  expect_equal(density(dist, 1), 2 * dnorm(1))
  expect_equal(quantile(dist, 0.5), qnorm(0.75))

  dist <- dist_truncated(dist_normal(0, 1), upper = 0)
  expect_equal(cdf(dist, 0), 1)
  expect_equal(density(dist, -1), 2 * dnorm(-1))
})

test_that("Truncated zero-inflated discrete distributions keep the lower bound mass", {
  dist <- dist_truncated(dist_inflated(dist_poisson(3), 0.2), lower = 1)
  expect_equal(sum(density(dist, 0:100)[[1]]), 1)
  expect_equal(density(dist, 1:3), list(dpois(1:3, 3) / (1 - dpois(0, 3))))
  expect_equal(quantile(dist, 0), 1)
})

test_that("Truncated distributions are vectorised over mixed families", {
  # Truncated normals and poissons (a lattice) in one vector
  dist <- c(
    dist_truncated(dist_normal(0, 1), -1, 2),
    dist_truncated(dist_poisson(3), 1, 5),
    dist_truncated(dist_normal(2, 1), 0)
  )
  each <- function(f) unlist(lapply(seq_along(dist), function(i) f(dist[i])))

  expect_equal(format(dist), c("N(0, 1)[-1,2]", "Pois(3)[1,5]", "N(2, 1)[0,Inf)"))
  expect_equal(density(dist, 2), each(function(d) density(d, 2)))
  expect_equal(cdf(dist, 1), each(function(d) cdf(d, 1)))
  expect_equal(cdf(dist, 1, tail = "upper"), each(function(d) cdf(d, 1, tail = "upper")))
  expect_equal(quantile(dist, 0.3), each(function(d) quantile(d, 0.3)))
  expect_equal(quantile(dist, 0.3, tail = "upper"), each(function(d) quantile(d, 0.3, tail = "upper")))
  expect_equal(mean(dist), each(mean), tolerance = 1e-6)

  # Lists pair a value with each distribution
  expect_equal(
    cdf(dist, list(c(0, 3, 1)))[[1]],
    c(cdf(dist[1], 0), cdf(dist[2], 3), cdf(dist[3], 1))
  )
  expect_equal(
    density(dist, list(c(0, 3, 1)))[[1]],
    c(density(dist[1], 0), density(dist[2], 3), density(dist[3], 1))
  )

  # The lower bound mass is only kept for the lattice distribution
  expect_equal(density(dist[2], 1)[[1]], dpois(1, 3) / (ppois(5, 3) - dpois(0, 3)))
  expect_equal(quantile(dist, 0), c(-1, 1, 0))

  # Bounds that differ for the same distribution
  dist <- dist_truncated(dist_poisson(3), lower = c(0, 1, 2), upper = c(4, 5, Inf))
  expect_equal(
    cdf(dist, 2),
    c(ppois(2, 3) / ppois(4, 3),
      (ppois(2, 3) - dpois(0, 3)) / (ppois(5, 3) - dpois(0, 3)),
      dpois(2, 3) / (1 - ppois(1, 3)))
  )
  expect_equal(support(dist)[[1]], support(dist[1])[[1]])
  expect_equal(quantile(dist, 0), c(0, 1, 2))
  expect_equal(quantile(dist, 1), c(4, 5, Inf))

  # Sampling respects the bounds of each distribution
  set.seed(1)
  draws <- generate(c(dist_truncated(dist_normal(0, 1), -1, 2), dist_truncated(dist_poisson(3), 1, 5)), list(c(50, 20)))
  expect_length(draws[[1]], 50)
  expect_length(draws[[2]], 20)
  expect_true(all(draws[[1]] >= -1 & draws[[1]] <= 2))
  expect_true(all(draws[[2]] >= 1 & draws[[2]] <= 5))
})
