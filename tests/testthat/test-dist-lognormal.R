test_that("Log-normal distribution", {
  # defaults
  dist <- dist_lognormal()
  expect_equal(mean(dist), exp(1/2))
  expect_equal(variance(dist), (exp(1)-1)*exp(1))

  # display
  expect_s3_class(dist, "distribution")
  expect_output(print(dist), "lN\\(0, 1\\)")
  expect_output(print(dist_lognormal(numeric())), "<distribution\\[0\\]>")

  # error checking
  expect_error(
    dist_lognormal(0, -1),
    "non-negative"
  )
  expect_silent(
    dist_lognormal(mu = 0, sigma = 1)
  )

  # density
  expect_equal(
    density(dist, 0),
    dlnorm(0, mean = 0, sd = 1)
  )
  # cdf
  expect_equal(
    cdf(dist, 5),
    plnorm(5, mean = 0, sd = 1)
  )
  # quantile
  expect_equal(
    quantile(dist, 0.1),
    qlnorm(0.1, mean = 0, sd = 1)
  )
  # generate
  expect_equal(
    {
      set.seed(0)
      generate(dist, 10)
    },
    {
      set.seed(0)
      mapply(function(m, s) rlnorm(10, m, s), m = 0, s = 1, SIMPLIFY = FALSE)
    }
  )
})

test_that("Log-normal distribution is vectorised over several distributions", {
  dist <- dist_lognormal(1:3, c(1, 2, 3))
  n <- 3
  at <- c(0.5, 2, 4)

  expect_equal(density(dist, list(at))[[1]], stats::dlnorm(at, 1:3, c(1, 2, 3)))
  expect_equal(cdf(dist, list(at))[[1]], stats::plnorm(at, 1:3, c(1, 2, 3)))
  expect_equal(quantile(dist, 0.3), stats::qlnorm(0.3, 1:3, c(1, 2, 3)))
  expect_equal(log_density(dist, list(at))[[1]], stats::dlnorm(at, 1:3, c(1, 2, 3), log = TRUE))

  # support has one region per distribution
  expect_equal(vec_size(support(dist)), n)
  expect_equal(mean(dist), exp(1:3 + c(1, 2, 3)^2 / 2))
  expect_equal(skewness(dist), vapply(vec_chop(dist), skewness, 1))

  # generate() returns one draw vector per distribution
  set.seed(1)
  draws <- generate(dist, list(c(2L, 0L, 5L)))
  expect_equal(lengths(draws), c(2L, 0L, 5L))
  expect_true(all(unlist(draws) >= 0))
})
