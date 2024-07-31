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
