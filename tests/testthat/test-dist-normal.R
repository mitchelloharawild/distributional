test_that("Normal distribution", {
  # defaults
  dist <- dist_normal()
  expect_equal(mean(dist), 0)
  expect_equal(variance(dist), 1)

  # display
  expect_s3_class(dist, "distribution")
  expect_output(print(dist), "N\\(0, 1\\)")
  expect_output(print(dist_normal(numeric())), "<distribution\\[0\\]>")

  # error checking
  expect_error(
    dist_normal(0, -1),
    "non-negative"
  )
  expect_silent(
    dist_normal(mu = 0L, sigma = 1L)
  )

  mu <- rnorm(10)
  sigma <- abs(rnorm(10))
  dist <- dist_normal(mu, sigma)

  # summary statistics
  expect_equal(mean(dist), mu)
  expect_equal(median(dist), mu)
  expect_equal(median(dist[[1]]), mu[[1]])
  expect_equal(variance(dist), sigma^2)

  # math
  expect_equal(mean(dist*3+1), mu*3+1)
  expect_equal(variance(dist*3+1), (sigma*3)^2)
  expect_equal(mean(dist + dist), mean(dist) + mean(dist))
  expect_equal(variance(dist + dist), variance(dist) + variance(dist))
  expect_equal(dist_normal() * 2, dist_normal(0, 2))

  # density
  expect_equal(
    density(dist, 0),
    dnorm(0, mean = mu, sd = sigma)
  )
  # cdf
  expect_equal(
    cdf(dist, 5),
    pnorm(5, mean = mu, sd = sigma)
  )
  # quantile
  expect_equal(
    quantile(dist, 0.1),
    qnorm(0.1, mean = mu, sd = sigma)
  )
  # generate
  expect_equal(
    {
      set.seed(0)
      generate(dist, 10)
    },
    {
      set.seed(0)
      mapply(function(m, s) rnorm(10, m, s), m = mu, s = sigma, SIMPLIFY = FALSE)
    }
  )
})
