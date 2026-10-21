test_that("F distribution", {
  dist <- dist_f(5, 2)

  expect_equal(format(dist), "F(5, 2)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qf(0.1, 5, 2))
  expect_equal(quantile(dist, 0.5), stats::qf(0.5, 5, 2))

  # pdf
  expect_equal(density(dist, 0), stats::df(0, 5, 2))
  expect_equal(density(dist, 3), stats::df(3, 5, 2))

  # cdf
  expect_equal(cdf(dist, 0), stats::pf(0, 5, 2))
  expect_equal(cdf(dist, 3), stats::pf(3, 5, 2))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), NA_real_)
  expect_equal(variance(dist), NA_real_)
  dist <- dist_f(5, 5)
  expect_equal(mean(dist), 5/(5-2))
  expect_equal(variance(dist), 2*5^2*(5+5-2)/(5*(5-2)^2*(5-4)))
})

test_that("F vectorised over several distributions", {
  dist <- dist_f(c(5, 5, 5, 5), c(2, 5, 7, 9))
  expect_equal(mean(dist), c(NA, 5/3, 7/5, 9/7))
  expect_equal(variance(dist), c(NA, 2*25*8/(5*9*1), 2*49*10/(5*25*3), 2*81*12/(5*49*5)))
  expect_equal(skewness(dist), c(NA, NA, skewness(dist_f(5, 7)), skewness(dist_f(5, 9))))
  expect_equal(kurtosis(dist)[1:3], rep(NA_real_, 3))
  expect_equal(density(dist, 1.5), stats::df(1.5, 5, c(2, 5, 7, 9)))
  expect_equal(cdf(dist, 1.5), stats::pf(1.5, 5, c(2, 5, 7, 9)))
  expect_equal(quantile(dist, 0.3), stats::qf(0.3, 5, c(2, 5, 7, 9)))
  expect_equal(vctrs::vec_size(support(dist)), 4L)
  expect_length(format(dist), 4L)
  set.seed(1)
  expect_equal(lengths(generate(dist, list(1:4))), 1:4)

  # non-central
  nc <- dist_f(c(3, 4), c(5, 2), ncp = c(1, 2))
  expect_equal(mean(nc), c(5 * (3 + 1) / (3 * 3), NA))
  expect_equal(density(nc, 1), stats::df(1, c(3, 4), c(5, 2), c(1, 2)))
  expect_equal(cdf(nc, 1), stats::pf(1, c(3, 4), c(5, 2), c(1, 2)))
  expect_equal(skewness(nc), rep(NA_real_, 2))
  expect_equal(lengths(generate(nc, list(c(2, 3)))), c(2, 3))
  # central and non-central together
  mix <- c(dist_f(3, 5), nc)
  expect_equal(density(mix, 1), c(stats::df(1, 3, 5), stats::df(1, c(3, 4), c(5, 2), c(1, 2))))
})
