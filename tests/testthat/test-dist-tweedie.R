test_that("Tweedie distribution", {
  mu <- 2
  dispersion <- 0.8
  power <- 1.5
  dist <- dist_tweedie(mu, dispersion, power)

  # Check formatting
  expect_equal(format(dist), "Tweedie(2, 0.8, 1.5)")

  # Require package installed
  skip_if_not_installed("tweedieDistr")

  # quantiles
  expect_equal(quantile(dist, 0.1), tweedieDistr::qtweedie(0.1, mu, dispersion, power))
  expect_equal(quantile(dist, 0.5), tweedieDistr::qtweedie(0.5, mu, dispersion, power))

  # pdf
  expect_equal(density(dist, 0), tweedieDistr::dtweedie(0, mu, dispersion, power))
  expect_equal(density(dist, 3), tweedieDistr::dtweedie(3, mu, dispersion, power))
  expect_equal(density(dist, 3, log = TRUE), tweedieDistr::dtweedie(3, mu, dispersion, power, log = TRUE))

  # cdf
  expect_equal(cdf(dist, 0), tweedieDistr::ptweedie(0, mu, dispersion, power))
  expect_equal(cdf(dist, 3), tweedieDistr::ptweedie(3, mu, dispersion, power))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4)), 0.4, tolerance = 1e-3)

  # Generate random samples
  set.seed(123)
  samples <- generate(dist, 10)
  set.seed(123)
  expect_equal(samples[[1L]], tweedieDistr::rtweedie(10, mu, dispersion, power))

  # stats
  expect_equal(mean(dist), mu)
  expect_equal(variance(dist), dispersion * mu^power)
  expect_equal(skewness(dist), power * sqrt(dispersion) * mu^(power / 2 - 1))
  expect_equal(kurtosis(dist), power * (2 * power - 1) * dispersion * mu^(power - 2))
})

test_that("Tweedie distribution parameter validation", {
  expect_error(dist_tweedie(mean = -1), "mean parameter")
  expect_error(dist_tweedie(dispersion = 0), "dispersion parameter")
  expect_error(dist_tweedie(power = 1), "power parameter")
  expect_error(dist_tweedie(power = 2), "power parameter")
})

test_that("Tweedie distribution is vectorised over several distributions", {
  dist <- dist_tweedie(1:3, c(1, 2, 3), c(1.3, 1.5, 1.7))
  n <- 3
  at <- c(1, 2, 3)

  expect_equal(density(dist, list(at))[[1]], tweedieDistr::dtweedie(at, 1:3, c(1, 2, 3), c(1.3, 1.5, 1.7)))
  expect_equal(cdf(dist, list(at))[[1]], tweedieDistr::ptweedie(at, 1:3, c(1, 2, 3), c(1.3, 1.5, 1.7)))
  expect_equal(quantile(dist, 0.3), tweedieDistr::qtweedie(0.3, 1:3, c(1, 2, 3), c(1.3, 1.5, 1.7)))
  expect_equal(log_density(dist, list(at))[[1]], tweedieDistr::dtweedie(at, 1:3, c(1, 2, 3), c(1.3, 1.5, 1.7), log = TRUE))

  # support has one region per distribution
  expect_equal(vec_size(support(dist)), n)
  expect_equal(mean(dist), as.numeric(1:3))
  expect_equal(skewness(dist), vapply(vec_chop(dist), skewness, 1))

  # generate() returns one draw vector per distribution
  set.seed(1)
  draws <- generate(dist, list(c(2L, 0L, 5L)))
  expect_equal(lengths(draws), c(2L, 0L, 5L))
  expect_true(all(unlist(draws) >= 0))
})
