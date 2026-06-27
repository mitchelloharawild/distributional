test_that("dist_convolved: basic construction and format", {
  d <- dist_convolved(dist_gamma(2, 2), dist_gamma(2, 2))
  expect_s3_class(d, "distribution")
  expect_match(format(d), "Gamma\\(2, 2\\) \\+ Gamma\\(2, 2\\)")
})

test_that("dist_convolved: density approximates the analytical result", {
  # Gamma(2,2) + Gamma(2,2) = Gamma(4,2) analytically
  d_conv  <- dist_convolved(dist_gamma(2, 2), dist_gamma(2, 2))
  d_exact <- dist_gamma(4, 2)

  x <- c(0.5, 1, 2, 3, 4)
  approx_dens <- density(d_conv, x)
  exact_dens  <- density(d_exact, x)

  expect_equal(approx_dens, exact_dens, tolerance = 1e-3)
})

test_that("dist_convolved: CDF approximates the analytical result", {
  d_conv  <- dist_convolved(dist_gamma(2, 2), dist_gamma(2, 2))
  d_exact <- dist_gamma(4, 2)

  q <- c(0.5, 1, 2, 3, 4)
  expect_equal(cdf(d_conv, q), cdf(d_exact, q), tolerance = 1e-3)
})

test_that("dist_convolved: quantile approximates the analytical result", {
  d_conv  <- dist_convolved(dist_gamma(2, 2), dist_gamma(2, 2))
  d_exact <- dist_gamma(4, 2)

  p <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  expect_equal(quantile(d_conv, p), quantile(d_exact, p), tolerance = 1e-2)
})

test_that("dist_convolved: generate returns correct length and type", {
  d <- dist_convolved(dist_gamma(2, 2), dist_exponential(1))
  # generate() on a distribution vector returns a list of length = length(d)
  samp <- generate(d, 100)
  expect_length(samp, 1L)       # 1 distribution in the vector
  expect_length(samp[[1]], 100L) # 100 samples from it
  expect_true(is.numeric(samp[[1]]))
})

test_that("dist_convolved: mean is exact sum of component means", {
  d <- dist_convolved(dist_gamma(2, 2), dist_exponential(3))
  # E[Gamma(2,2)] = 2/2 = 1; E[Exp(3)] = 1/3
  expect_equal(mean(d), 1 + 1/3, tolerance = 1e-10)
})

test_that("dist_convolved: variance is exact sum of component variances", {
  d <- dist_convolved(dist_gamma(2, 2), dist_exponential(3))
  # Var[Gamma(2,2)] = 2/4 = 0.5; Var[Exp(3)] = 1/9
  expect_equal(variance(d), 0.5 + 1/9, tolerance = 1e-10)
})

test_that("dist_convolved: normal sum via + uses closed-form, not convolution", {
  # dist_normal has Ops.dist_normal which returns an exact dist_normal
  d <- dist_normal(0, 1) + dist_normal(0, 1)
  expect_s3_class(vec_data(d)[[1]], "dist_normal")
})

test_that("dist_convolved: + operator creates dist_convolved for other families", {
  d <- dist_gamma(2, 2) + dist_gamma(2, 2)
  expect_s3_class(vec_data(d)[[1]], "dist_convolved")
})

test_that("dist_convolved: subtraction via - works", {
  # X - Y = X + (-Y); test that it constructs without error
  d <- dist_normal(5, 1) - dist_normal(0, 1)
  # E[N(5,1) - N(0,1)] = 5; Var = 1 + 1 = 2
  # Using analytical result (normal - normal = normal)
  expect_s3_class(vec_data(d)[[1]], "dist_normal")
})

test_that("dist_convolved: subtraction of non-normal distributions", {
  # Gamma - Gamma is not analytically simple; should give a dist_convolved
  d <- dist_gamma(4, 1) - dist_gamma(2, 1)
  expect_s3_class(vec_data(d)[[1]], "dist_convolved")

  # Mean: E[Gamma(4,1)] - E[Gamma(2,1)] = 4 - 2 = 2
  expect_equal(mean(d), 2, tolerance = 1e-10)
})

test_that("dist_convolved: works with mixed distribution families", {
  skip_unless_r(">= 4.3.0")

  d <- dist_normal(0, 1) + dist_exponential(1)
  expect_s3_class(vec_data(d)[[1]], "dist_convolved")
  # Mean = 0 + 1 = 1
  expect_equal(mean(d), 1, tolerance = 1e-10)
})

test_that("dist_convolved: vectorises over pairs", {
  d <- dist_convolved(
    dist_gamma(c(2, 3), c(2, 1)),
    dist_gamma(c(2, 1), c(2, 1))
  )
  expect_length(d, 2L)
  expect_s3_class(d, "distribution")
})

test_that("dist_convolved: invalid arguments are rejected", {
  expect_error(dist_convolved(1, dist_normal()))
})

test_that("dist_convolved: family returns 'convolved'", {
  d <- dist_convolved(dist_gamma(2, 2), dist_gamma(2, 2))
  expect_equal(family(d), "convolved")
})

test_that("dist_convolved: transformed distribution + distribution works", {
  skip_unless_r(">= 4.3.0")

  # dist_lognormal is a dist_transformed internally; adding another dist
  # should produce a dist_convolved
  d <- dist_lognormal(0, 1) + dist_exponential(1)
  expect_s3_class(vec_data(d)[[1]], "dist_convolved")
  # Mean of lognormal(0,1) = exp(0.5); mean of Exp(1) = 1
  expect_equal(mean(d), exp(0.5) + 1, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# k-way convolution tests
# ---------------------------------------------------------------------------

test_that("dist_convolved: explicit 3-arg construction works", {
  d3 <- dist_convolved(dist_gamma(2, 2), dist_gamma(2, 2), dist_gamma(2, 2))
  raw <- vec_data(d3)[[1L]]
  expect_s3_class(raw, "dist_convolved")
  expect_length(raw[["dist"]], 3L)
})

test_that("dist_convolved: requires at least two distributions", {
  expect_error(dist_convolved(dist_gamma(2, 2)), "at least two")
})

test_that("dist_convolved: k-way chaining produces a flat dist_convolved", {
  d3 <- dist_gamma(2, 2) + dist_gamma(2, 2) + dist_gamma(2, 2)
  raw <- vec_data(d3)[[1L]]
  expect_s3_class(raw, "dist_convolved")
  # Flattening happens at construction: 3 components, not a nested 2+1
  expect_length(raw[["dist"]], 3L)
})

test_that("dist_convolved: k-way format lists all components", {
  d3 <- dist_gamma(2, 2) + dist_gamma(2, 2) + dist_gamma(2, 2)
  expect_match(format(d3), "Gamma\\(2, 2\\) \\+ Gamma\\(2, 2\\) \\+ Gamma\\(2, 2\\)")
})

test_that("dist_convolved: 3-way density matches Gamma(6,2) analytically", {
  # Gamma(2,2) + Gamma(2,2) + Gamma(2,2) = Gamma(6,2)
  d_conv  <- dist_gamma(2, 2) + dist_gamma(2, 2) + dist_gamma(2, 2)
  d_exact <- dist_gamma(6, 2)

  x <- c(1, 2, 3, 4, 5)
  expect_equal(density(d_conv, x), density(d_exact, x), tolerance = 1e-2)
})

test_that("dist_convolved: 3-way CDF matches Gamma(6,2) analytically", {
  d_conv  <- dist_gamma(2, 2) + dist_gamma(2, 2) + dist_gamma(2, 2)
  d_exact <- dist_gamma(6, 2)

  q <- c(1, 2, 3, 4, 5)
  expect_equal(cdf(d_conv, q), cdf(d_exact, q), tolerance = 1e-2)
})

test_that("dist_convolved: k-way mean is exact sum of all component means", {
  # E[Gamma(2,2)] = 1; three of them => mean = 3
  d3 <- dist_gamma(2, 2) + dist_gamma(2, 2) + dist_gamma(2, 2)
  expect_equal(mean(d3), 3, tolerance = 1e-10)

  skip_unless_r(">= 4.3.0")

  # 4-way with mixed families
  d4 <- dist_gamma(2, 2) + dist_exponential(2) + dist_normal(1, 1) + dist_gamma(3, 1)
  expected_mean <- 1 + 0.5 + 1 + 3  # 1 + 1/2 + 1 + 3/1
  expect_equal(mean(d4), expected_mean, tolerance = 1e-10)
})

test_that("dist_convolved: k-way variance is exact sum of component variances", {
  # Var[Gamma(2,2)] = 2/4 = 0.5; three of them => var = 1.5
  d3 <- dist_gamma(2, 2) + dist_gamma(2, 2) + dist_gamma(2, 2)
  expect_equal(variance(d3), 1.5, tolerance = 1e-10)
})

test_that("dist_convolved: chaining is associative in output", {
  # (d1 + d2) + d3 vs d1 + (d2 + d3) should give the same mean and density
  d_left  <- (dist_gamma(2, 2) + dist_gamma(2, 2)) + dist_gamma(2, 2)
  d_right <- dist_gamma(2, 2) + (dist_gamma(2, 2) + dist_gamma(2, 2))

  expect_equal(mean(d_left), mean(d_right), tolerance = 1e-10)
  expect_equal(density(d_left, 3), density(d_right, 3), tolerance = 1e-3)
})

# ---------------------------------------------------------------------------
# Analytical convolution identities: FFT approximation accuracy
# ---------------------------------------------------------------------------

test_that("dist_convolved: Normal + Normal FFT approximates Normal(mu1+mu2, sqrt(s1^2+s2^2))", {
  # Force the FFT path by calling dist_convolved() directly.
  # N(1, 1) + N(2, 1.5) = N(3, sqrt(1 + 2.25)) = N(3, sqrt(3.25))
  d_conv  <- dist_convolved(dist_normal(1, 1), dist_normal(2, 1.5))
  d_exact <- dist_normal(3, sqrt(3.25))

  x <- c(0, 1, 3, 5, 7)
  expect_equal(density(d_conv, x), density(d_exact, x), tolerance = 1e-3)
  expect_equal(cdf(d_conv, x),     cdf(d_exact, x),     tolerance = 1e-3)

  p <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  expect_equal(quantile(d_conv, p), quantile(d_exact, p), tolerance = 1e-2)
})

test_that("dist_convolved: Exponential + Exponential approximates Gamma(2, rate)", {
  # Exp(lambda) + Exp(lambda) = Gamma(shape = 2, rate = lambda)
  lambda  <- 3
  d_conv  <- dist_convolved(dist_exponential(lambda), dist_exponential(lambda))
  d_exact <- dist_gamma(2, lambda)

  # Avoid x values very close to 0 where density is small and relative error inflates
  x <- c(0.2, 0.4, 0.7, 1.0, 1.5)
  expect_equal(density(d_conv, x), density(d_exact, x), tolerance = 1e-2)
  expect_equal(cdf(d_conv, x),     cdf(d_exact, x),     tolerance = 1e-2)

  p <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  expect_equal(quantile(d_conv, p), quantile(d_exact, p), tolerance = 1e-2)
})

test_that("dist_convolved: Chi-squared + Chi-squared approximates Chi-squared(k1 + k2)", {
  # chi^2(k1) + chi^2(k2) = chi^2(k1 + k2)
  d_conv  <- dist_convolved(dist_chisq(4), dist_chisq(6))
  d_exact <- dist_chisq(10)

  x <- c(2, 5, 8, 12, 18)
  expect_equal(density(d_conv, x), density(d_exact, x), tolerance = 1e-3)
  expect_equal(cdf(d_conv, x),     cdf(d_exact, x),     tolerance = 1e-3)

  p <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  expect_equal(quantile(d_conv, p), quantile(d_exact, p), tolerance = 1e-2)
})
