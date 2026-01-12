test_that("Tweedie distribution", {
  # variance = phi * mu^power = 1 * 0^1.5 = 0
  dist <- dist_tweedie(mu = 0, phi = 1, power = 1.5)
  expect_equal(mean(dist), 0)
  expect_equal(variance(dist), 0)  # variance is 0 when mu = 0

  # display
  expect_s3_class(dist, "distribution")
  expect_output(print(dist), "Tweedie\\(0, 1, 1.5\\)")
  expect_output(print(dist_tweedie(numeric(), numeric(), numeric())), "<distribution\\[0\\]>")

  # error checking
  expect_error(
    dist_tweedie(0, -1, 1.5),
    "non-negative"
  )
  expect_error(
    dist_tweedie(0, 1, 0.5),
    "must be"
  )
  expect_error(
    dist_tweedie(-1, 1, 1.5),
    "non-negative"
  )
  expect_silent(
    dist_tweedie(mu = 0, phi = 1, power = 1.5)
  )
  expect_silent(
    dist_tweedie(mu = 0L, phi = 1L, power = 1.5)
  )

  # Test Poisson-like (power = 1)
  mu1 <- c(1, 5, 10)
  phi1 <- c(1, 1, 1)
  power1 <- c(1, 1, 1)
  dist1 <- dist_tweedie(mu1, phi1, power1)

  # summary statistics for Poisson-like
  expect_equal(mean(dist1), mu1)
  expect_equal(variance(dist1), phi1 * mu1^power1)
  expect_equal(variance(dist1), c(1, 5, 10))

  # density for Poisson-like
  expect_equal(
    as.numeric(density(dist1, 2)),
    c(0.1839397, 0.08422434, 0.002269996),
    tolerance = 1e-6
  )

  # cdf for Poisson-like
  expect_equal(
    as.numeric(cdf(dist1, 5)),
    c(0.9994058, 0.6159607, 0.06708596),
    tolerance = 1e-6
  )

  # quantile for Poisson-like
  expect_equal(
    as.numeric(quantile(dist1, 0.5)),
    c(1, 5, 10),
    tolerance = 1e-6
  )

  # Test Compound Poisson-Gamma (power = 1.5)
  mu2 <- c(2, 4, 8)
  phi2 <- c(0.5, 1, 2)
  power2 <- c(1.5, 1.5, 1.5)
  dist2 <- dist_tweedie(mu2, phi2, power2)

  # summary statistics for Compound Poisson-Gamma
  expect_equal(mean(dist2), mu2)
  expect_equal(variance(dist2), phi2 * mu2^power2)
  expect_equal(
    variance(dist2),
    c(1.414214, 8, 45.25483),
    tolerance = 1e-5
  )

  # density for Compound Poisson-Gamma
  expect_equal(
    as.numeric(density(dist2, 2)),
    c(0.3240153, 0.1564012, 0.06979308),
    tolerance = 1e-6
  )

  # cdf for Compound Poisson-Gamma
  expect_equal(
    as.numeric(cdf(dist2, 5)),
    c(0.9809178, 0.6929818, 0.4003952),
    tolerance = 1e-6
  )

  # quantile for Compound Poisson-Gamma
  expect_equal(
    as.numeric(quantile(dist2, 0.5)),
    c(1.820328, 3.487817, 6.532882),
    tolerance = 1e-5
  )

  # Test Gamma-like (power = 2)
  mu3 <- c(3, 6, 9)
  phi3 <- c(1, 1.5, 2)
  power3 <- c(2, 2, 2)
  dist3 <- dist_tweedie(mu3, phi3, power3)

  # summary statistics for Gamma-like
  expect_equal(mean(dist3), mu3)
  expect_equal(variance(dist3), phi3 * mu3^power3)
  expect_equal(
    variance(dist3),
    c(9, 54, 162),
    tolerance = 1e-6
  )

  # density for Gamma-like
  expect_equal(
    as.numeric(density(dist3, 2)),
    c(0.171139, 0.1084747, 0.08414317),
    tolerance = 1e-6
  )

  # cdf for Gamma-like
  expect_equal(
    as.numeric(cdf(dist3, 5)),
    c(0.8111244, 0.607629, 0.5439435),
    tolerance = 1e-6
  )

  # quantile for Gamma-like
  expect_equal(
    as.numeric(quantile(dist3, 0.5)),
    c(2.079442, 3.389004, 4.094428),
    tolerance = 1e-5
  )

  # Test Inverse Gaussian (power = 3)
  mu4 <- c(2, 5, 10)
  phi4 <- c(1, 2, 3)
  power4 <- c(3, 3, 3)
  dist4 <- dist_tweedie(mu4, phi4, power4)

  # summary statistics for Inverse Gaussian
  expect_equal(mean(dist4), mu4)
  expect_equal(variance(dist4), phi4 * mu4^power4)
  expect_equal(
    variance(dist4),
    c(8, 250, 3000),
    tolerance = 1e-6
  )

  # generate - test with seed for reproducibility
  expect_equal(
    {
      set.seed(42)
      generate(dist_tweedie(mu = 5, phi = 1, power = 1.5), 5)
    },
    {
      set.seed(42)
      list(tweedie::rtweedie(5, mu = 5, phi = 1, power = 1.5))
    }
  )

  # generate - vectorized
  expect_equal(
    {
      set.seed(42)
      generate(dist_tweedie(mu = c(2, 5), phi = c(1, 1), power = c(1.5, 1.5)), 5)
    },
    {
      set.seed(42)
      list(
        tweedie::rtweedie(5, mu = 2, phi = 1, power = 1.5),
        tweedie::rtweedie(5, mu = 5, phi = 1, power = 1.5)
      )
    }
  )

  # Test median
  expect_equal(median(dist2), quantile(dist2, 0.5))
  expect_equal(median(dist2[[1]]), quantile(dist2, 0.5)[[1]])

  # Test support regions
  dist_mixed <- dist_tweedie(
    mu = c(1, 2, 3, 4),
    phi = c(1, 1, 1, 1),
    power = c(1, 1.5, 2, 3)
  )
  supp <- support(dist_mixed)
  expect_s3_class(supp, "support_region")
  expect_equal(length(supp), 4)
  expect_equal(unname(format(supp)), c("N0", "[0,Inf)", "(0,Inf)", "(0,Inf)"))
})
