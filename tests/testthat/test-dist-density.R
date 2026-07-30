test_that("Density distribution", {
  at <- seq(-6, 6, by = 0.005)
  dist <- dist_density(list(at), list(dnorm(at)))

  expect_equal(format(dist), "density[2401]")

  # pdf
  expect_equal(density(dist, 0), dnorm(0))
  expect_equal(density(dist, c(-1, 0.5))[[1]], dnorm(c(-1, 0.5)))

  # cdf
  expect_equal(cdf(dist, 1.96), pnorm(1.96), tolerance = 1e-5)
  expect_equal(cdf(dist, c(-1, 0))[[1]], pnorm(c(-1, 0)), tolerance = 1e-5)

  # quantiles
  expect_equal(quantile(dist, 0.975), qnorm(0.975), tolerance = 1e-5)

  # stats
  expect_equal(mean(dist), 0)
  expect_equal(variance(dist), 1, tolerance = 1e-5)
  expect_equal(median(dist), 0)
})

test_that("the density is exactly interpolated and standardised", {
  dist <- vec_data(dist_density(list(c(0, 1, 2)), list(c(0, 1, 0))))[[1]]

  # Standardised to integrate to one (the input integrates to 1 already)
  expect_equal(density(dist, c(0, 0.5, 1, 1.5, 2)), c(0, 0.5, 1, 0.5, 0))

  # The density is zero beyond the tabulated values, not NA
  expect_equal(density(dist, c(-1, 3)), c(0, 0))
  expect_equal(density(dist, NA_real_), NA_real_)

  # Unlike `dist_quantile()`, evaluating a single point works and is exact
  expect_equal(density(dist, 1), 1)
  expect_equal(density(dist, 1), density(dist, 1))

  # An unstandardised density is rescaled, with a warning when far from one
  expect_warning(
    scaled <- vec_data(dist_density(list(c(0, 1)), list(c(2, 2))))[[1]],
    "integrates to 2"
  )
  expect_equal(density(scaled, 0.5), 1)

  # Small deviations (such as a grid missing a little tail mass) are silent
  expect_silent(dist_density(list(c(0, 1)), list(c(0.995, 0.995))))
})

test_that("moments of piecewise linear densities are exact", {
  # Uniform on [0,1]
  unif <- dist_density(list(c(0, 1)), list(c(1, 1)))
  expect_identical(mean(unif), 0.5)
  expect_equal(variance(unif), 1/12)
  expect_equal(cdf(unif, 0.25), 0.25)
  expect_equal(quantile(unif, 0.25), 0.25)

  # Triangular on [0,2] with the peak at 0
  tri <- dist_density(list(c(0, 2)), list(c(1, 0)))
  expect_equal(mean(tri), 2/3)
  expect_equal(variance(tri), 2/9)

  # Symmetric triangular on [0,2], compared against its closed form
  sym <- dist_density(list(c(0, 1, 2)), list(c(0, 1, 0)))
  expect_equal(mean(sym), 1)
  expect_equal(variance(sym), 1/6)
})

test_that("the quantile function inverts the cdf", {
  at <- seq(-6, 6, by = 0.005)
  dist <- vec_data(dist_density(list(at), list(dnorm(at))))[[1]]

  p <- seq(0.0005, 0.9995, length.out = 257)
  expect_equal(cdf(dist, quantile(dist, p)), p)

  # Also for a density with a non-constant slope within each interval
  tri <- vec_data(dist_density(list(c(0, 1, 2)), list(c(0, 1, 0))))[[1]]
  expect_equal(cdf(tri, quantile(tri, p)), p)
  expect_equal(quantile(tri, 0.5), 1)
})

test_that("evaluation beyond the tabulated values", {
  dist <- vec_data(dist_density(list(c(0, 1)), list(c(1, 1))))[[1]]

  expect_equal(cdf(dist, c(-1, 0, 1, 2)), c(0, 0, 1, 1))
  expect_equal(cdf(dist, NA_real_), NA_real_)

  expect_equal(quantile(dist, c(0, 1)), c(0, 1))
  expect_equal(quantile(dist, c(-0.1, 1.1)), c(NaN, NaN))
  expect_equal(quantile(dist, NA_real_), NA_real_)

  expect_equal(density(dist, numeric()), numeric())
})

test_that("support of a density distribution", {
  # A density which is non-zero at its limits is closed there
  unif <- support(dist_density(list(c(0, 1)), list(c(1, 1))))
  expect_equal(field(unif, "lim")[[1]], c(0, 1))
  expect_equal(field(unif, "closed")[[1]], c(TRUE, TRUE))
  expect_equal(format(unif), "[0,1]")

  # A density which decays to zero is open at its limits
  at <- seq(-6, 6, by = 0.01)
  norm <- support(dist_density(list(at), list(dnorm(at))))
  expect_equal(field(norm, "lim")[[1]], c(-6, 6))
  expect_equal(field(norm, "closed")[[1]], c(FALSE, FALSE))
})

test_that("generating from a density distribution", {
  at <- seq(-6, 6, by = 0.005)
  dist <- dist_density(list(at), list(dnorm(at)))

  set.seed(0)
  r <- generate(dist, 10000)[[1]]
  expect_length(r, 10000)
  expect_equal(mean(r), 0, tolerance = 0.05)
  expect_equal(sd(r), 1, tolerance = 0.05)
  expect_true(all(r >= -6 & r <= 6))
})

test_that("density distributions are vectorised", {
  dist <- dist_density(list(c(0, 1), c(0, 2)), list(c(1, 1), c(1, 0)))

  expect_length(dist, 2L)
  expect_equal(format(dist), c("density[2]", "density[2]"))
  expect_equal(mean(dist), c(0.5, 2/3))
  # The second density interpolates from f(0) = 1 down to f(2) = 0
  expect_equal(density(dist, 0.5), c(1, 0.75))
})

test_that("invalid tabulated densities are rejected", {
  expect_error(
    dist_density(list(c(0, 1, 2)), list(c(1, 1))),
    "must be the same length"
  )
  expect_error(dist_density(list(1), list(1)), "At least two values")
  expect_error(dist_density(list(c(0, NA)), list(c(1, 1))), "must not be missing")
  expect_error(dist_density(list(c(0, 1)), list(c(-1, 1))), "must not be negative")
  expect_error(dist_density(list(c(0, 0)), list(c(1, 1))), "must not contain duplicates")
  expect_error(dist_density(list(c(0, 1)), list(c(0, 0))), "must not all be zero")
})

test_that("unsorted values are sorted", {
  sorted <- dist_density(list(c(0, 1, 2)), list(c(0, 1, 0)))
  unsorted <- dist_density(list(c(2, 0, 1)), list(c(0, 0, 1)))

  expect_equal(density(unsorted, 0.5), density(sorted, 0.5))
  expect_equal(mean(unsorted), mean(sorted))
})
