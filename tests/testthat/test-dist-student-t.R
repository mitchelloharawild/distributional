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

test_that("Student t distribution is vectorised over distributions", {
  dist <- dist_student_t(c(3, 5, 8), c(0, 1, 2), c(1, 2, 3))
  expect_equal(format(dist), c("t(3, 0, 1)", "t(5, 1, 2)", "t(8, 2, 3)"))
  expect_equal(mean(dist), c(0, 1, 2))
  expect_equal(variance(dist), c(3, 5/3 * 4, 8/6 * 9))
  expect_equal(density(dist, 1), dt((1 - c(0, 1, 2)) / c(1, 2, 3), c(3, 5, 8)) / c(1, 2, 3))
  expect_equal(cdf(dist, 1), pt((1 - c(0, 1, 2)) / c(1, 2, 3), c(3, 5, 8)))
  expect_equal(quantile(dist, 0.3), qt(0.3, c(3, 5, 8)) * c(1, 2, 3) + c(0, 1, 2))
  expect_equal(cdf(dist, list(c(0, 1, 2)))[[1]], pt((c(0, 1, 2) - c(0, 1, 2)) / c(1, 2, 3), c(3, 5, 8)))

  # Moments are undefined or infinite for small df
  small <- dist_student_t(c(0.5, 1.5, 3))
  expect_equal(mean(small), c(NA, 0, 0))
  expect_equal(variance(small), c(NA, Inf, 3))

  gen <- generate(dist, list(c(2, 3, 4)))
  expect_equal(lengths(gen), c(2L, 3L, 4L))

  expect_equal(vctrs::vec_size(support(dist)), 3L)
})

test_that("Student t distribution mixes central and non-central elements", {
  dist <- c(dist_student_t(5, 1, 2), dist_student_t(8, 2, 3, ncp = 6), dist_student_t(4))
  ncp <- c(NA, 6, NA)
  expect_equal(format(dist), c("t(5, 1, 2)", "t(8, 2, 3, 6)", "t(4, 0, 1)"))

  # Same results as each distribution on its own
  single <- function(f, ...) vapply(seq_along(dist), function(i) f(dist[i], ...), numeric(1L))
  expect_equal(mean(dist), single(mean))
  expect_equal(variance(dist), single(variance))
  expect_equal(density(dist, 1.5), single(density, 1.5))
  expect_equal(cdf(dist, 1.5), single(cdf, 1.5))
  expect_equal(cdf(dist, 1.5, tail = "upper"), single(cdf, 1.5, tail = "upper"))
  expect_equal(quantile(dist, 0.3), single(quantile, 0.3))
  expect_equal(density(dist, 1.5, log = TRUE), log(density(dist, 1.5)))

  # Same-slot vector of distributions
  same <- dist_student_t(c(5, 8, 4), c(1, 2, 0), c(2, 3, 1), ncp = c(NA, 6, NA))
  expect_equal(mean(same), mean(dist))
  expect_equal(density(same, 1.5), density(dist, 1.5))
  expect_equal(cdf(same, list(c(0, 1, 2)))[[1]], c(cdf(dist[1], 0), cdf(dist[2], 1), cdf(dist[3], 2)))
  expect_equal(format(same), format(dist))

  # Non-central draws are centred on the non-central mean
  set.seed(1)
  gen <- generate(dist_student_t(c(5, 30), 0, 1, ncp = c(NA, 2)), list(c(2, 20000)))
  expect_equal(lengths(gen), c(2L, 20000L))
  expect_equal(mean(gen[[2]]), mean(dist_student_t(30, 0, 1, ncp = 2)), tolerance = 0.05)
})
