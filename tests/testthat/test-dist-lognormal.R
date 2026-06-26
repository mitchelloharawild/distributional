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

  mu1    <- rnorm(10)
  sigma1 <- abs(rnorm(10))
  dist1  <- dist_lognormal(mu1, sigma1)

  mu2    <- rnorm(10)
  sigma2 <- abs(rnorm(10))
  dist2  <- dist_lognormal(mu2, sigma2)

  # summary statistics
  expect_equal(median(dist1), exp( mu1) )
  expect_equal(mean(dist1), exp( mu1 + (sigma1^2)/2  )   )
  expect_equal(variance(dist1), (exp(sigma1^2) - 1) * exp (2 * mu1 + sigma1 ^2) )

  # math ops
  expect_equal(dist1 * dist2,  dist_lognormal(mu1+mu2, sqrt(sigma1^2 +sigma2^2)   ) )
  expect_equal(dist1 / dist2,  dist_lognormal(mu1-mu2, sqrt(sigma1^2 +sigma2^2)   ) )

  expect_equal(3 * dist1, dist_lognormal(mu1 + log(3), sigma1))
  expect_equal(3 / dist1, dist_lognormal(log(3) - mu1, sigma1))

  expect_equal(dist1 * 3, dist_lognormal(mu1 + log(3), sigma1))
  expect_equal(dist1 / 3, dist_lognormal(mu1 - log(3), sigma1))

  expect_equal(+dist1,  dist1   )

  expect_equal(mean(dist1 * 3), exp((mu1 + log(3)) + sigma1^2/2))
  expect_equal(variance(dist1 * 3), (exp(sigma1^2)-1)*exp(2*(mu1 + log(3)) + sigma1^2))
  expect_equal(mean(dist1 / 2), exp((mu1 - log(2)) + sigma1^2/2))
  expect_equal(variance(dist1 / 2), (exp(sigma1^2)-1)*exp(2*(mu1 - log(2)) + sigma1^2))
  expect_equal(mean(3 / dist1), exp((log(3) - mu1) + sigma1^2/2))
  expect_equal(variance(3/ dist1 ), (exp(sigma1^2)-1)*exp(2*(- mu1 + log(3)) + sigma1^2))

  expect_equal(mean(dist1 * dist2), exp((mu1 + mu2) + (sqrt(sigma1^2 + sigma2^2))^2/2))
  expect_equal(variance(dist1 * dist2), (exp((sqrt(sigma1^2 + sigma2^2))^2)-1)*exp(2*(mu1 + mu2) + (sqrt(sigma1^2 + sigma2^2))^2))

  expect_equal(mean(dist1 / dist2), exp((mu1 - mu2) + (sqrt(sigma1^2 + sigma2^2))^2/2))
  expect_equal(variance(dist1 / dist2), (exp((sqrt(sigma1^2 + sigma2^2))^2)-1)*exp(2*(mu1 - mu2) + (sqrt(sigma1^2 + sigma2^2))^2))

  expect_equal(dist_lognormal(1, 1) * dist_lognormal(2, 2), dist_lognormal(3, sqrt(5)))
  expect_equal(dist_lognormal(3, 3) / dist_lognormal(1, 4), dist_lognormal(2, 5))

})
