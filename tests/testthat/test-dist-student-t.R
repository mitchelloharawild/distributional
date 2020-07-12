test_that("Student T distribution", {
  dist <- dist_student_t(5)

  expect_equal(format(dist), "t(5, 0, 1)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qt(0.1, 5))
  expect_equal(quantile(dist, 0.5), stats::qt(0.5, 5))

  # pdf
  expect_equal(density(dist, 0), stats::dt(0, 5))
  expect_equal(density(dist, 3), stats::dt(3, 5))

  # cdf
  expect_equal(cdf(dist, 0), stats::pt(0, 5))
  expect_equal(cdf(dist, 3), stats::pt(3, 5))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 0)
  expect_equal(variance(dist), 5 / (5-2))
})

test_that("Noncentral Student t distribution", {
  dist <- dist_student_t(8, ncp = 6)

  expect_equal(format(dist), "t(8, 0, 1, 6)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qt(0.1, 8, ncp = 6))
  expect_equal(quantile(dist, 0.5), stats::qt(0.5, 8, ncp = 6))

  # pdf
  expect_equal(density(dist, 0), stats::dt(0, 8, ncp = 6))
  expect_equal(density(dist, 3), stats::dt(3, 8, ncp = 6))

  # cdf
  expect_equal(cdf(dist, 0), stats::pt(0, 8, ncp = 6))
  expect_equal(cdf(dist, 3), stats::pt(3, 8, ncp = 6))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 2 * gamma(7/2))
  expect_equal(variance(dist), 148/3 - 4 * gamma(7/2)^2)
})

test_that("Location-scale Student t distribution", {
  dist <- dist_student_t(5, 2, 3)

  expect_equal(format(dist), "t(5, 2, 3)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qt(0.1, 5)*3 + 2)
  expect_equal(quantile(dist, 0.5), stats::qt(0.5, 5)*3 + 2)

  # pdf
  expect_equal(density(dist, 0), stats::dt(-2/3, 5)/3)
  expect_equal(density(dist, 3), stats::dt((3-2)/3, 5)/3)

  # cdf
  expect_equal(cdf(dist, 0), stats::pt(-2/3, 5))
  expect_equal(cdf(dist, 3), stats::pt((3-2)/3, 5))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 2)
  expect_equal(variance(dist), 5 / (5-2) * 3^2)
})

test_that("Noncentral location-scale Student t distribution", {
  dist <- dist_student_t(5, 2, 3, ncp = 6)

  expect_equal(format(dist), "t(5, 2, 3, 6)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qt(0.1, 5, 6)*3 + 2)
  expect_equal(quantile(dist, 0.5), stats::qt(0.5, 5, 6)*3 + 2)

  # pdf
  expect_equal(density(dist, 0), stats::dt(-2/3, 5, 6)/3)
  expect_equal(density(dist, 3), stats::dt((3-2)/3, 5, 6)/3)

  # cdf
  expect_equal(cdf(dist, 0), stats::pt(-2/3, 5, 6))
  expect_equal(cdf(dist, 3), stats::pt((3-2)/3, 5, 6))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)

  # stats
  expect_equal(mean(dist), 2 + 6 * sqrt(5/2) * gamma(2)/gamma(5/2) * 3)
  expect_equal(variance(dist), ((5*(1+6^2))/(5-2) - (6 * sqrt(5/2) * (gamma((5-1)/2)/gamma(5/2)))^2)*3^2)
})
