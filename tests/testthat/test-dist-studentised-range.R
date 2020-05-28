test_that("Studentized Range distribution", {
  dist <- dist_studentized_range(6, 5, 1)

  expect_equal(format(dist), "StudentizedRange(6, 5, 1)")

  # quantiles
  expect_equal(quantile(dist, 0.1), stats::qtukey(0.1, 6, 5, 1))
  expect_equal(quantile(dist, 0.5), stats::qtukey(0.5, 6, 5, 1))

  # pdf

  # cdf
  expect_equal(cdf(dist, 0), stats::ptukey(0, 6, 5, 1))
  expect_equal(cdf(dist, 3), stats::ptukey(3, 6, 5, 1))

  # F(Finv(a)) ~= a
  expect_equal(cdf(dist, quantile(dist, 0.4246)), 0.4246, tolerance = 1e-3)
})
