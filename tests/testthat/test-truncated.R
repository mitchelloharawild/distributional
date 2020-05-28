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

  # format
  expect_equal(format(dist), sprintf("%s[76,86]", format(dist_binomial(100,0.83))))

  # quantiles
  expect_equal(
    quantile(dist, 0.1),
    79 #dput(extraDistr::qtbinom(0.1, 100, 0.83, 76, 86))
  )
  expect_equal(
    quantile(dist, 0.5),
    82 #dput(extraDistr::qtbinom(0.5, 100, 0.83, 76, 86))
  )

  # pdf
  expect_equal(density(dist, 75), 0)
  expect_equal(density(dist, 87), 0)
  expect_equal(
    density(dist, 80),
    0.094154977726162, #dput(extraDistr::dtbinom(80, 100, 0.83, 76, 86))
  )
  expect_equal(
    density(dist, 85),
    0.123463609708811, #dput(extraDistr::dtbinom(85, 100, 0.83, 76, 86))
  )

  # cdf
  expect_equal(cdf(dist, 0), 0)
  expect_equal(cdf(dist, 76), 0)
  expect_equal(cdf(dist, 86), 1)
  expect_equal(
    cdf(dist, 80),
    0.259185477677455, #dput(extraDistr::ptbinom(80, 100, 0.83, 76, 86))
  )

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.372)), 0.372, tolerance = 1e-3)

  # stats
  # expect_equal(mean(dist), ???)
  # expect_equal(variance(dist), ???)
})
