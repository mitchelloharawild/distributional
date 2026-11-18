test_that("is_distribution", {
  expect_false(is_distribution(iris))
  expect_true(is_distribution(dist_normal()))
  expect_false(is_distribution(NULL))
  expect_false(is_distribution(0))

  df <- data.frame(a = 1:10, b = dist_poisson(1:10), c = dist_normal(1:10))
  expect_true(all(sapply(df, is_distribution) == c(FALSE, TRUE, TRUE)))
})

test_that("variance() works correctly on vectors/matrices of different dimension", {
  x = 1:8

  expect_equal(variance(x), 6)
  expect_equal(variance(matrix(x, nrow = 2)), rep(0.5, 4))
})

test_that("variance() throws an error on non-numeric objects", {
  expect_error(variance("foo"))
})

test_that("parameters() returns distribution-valued parameters unmodified by default", {
  dist <- dist_inflated(dist_negative_binomial(10, 0.6), prob = 0.5)
  p <- parameters(dist)

  expect_identical(names(p), c("dist", "x", "p"))
  expect_true(is_distribution(p$dist))
  expect_equal(p$x, 0)
  expect_equal(p$p, 0.5)
})

test_that("parameters(recursive = TRUE) expands distribution-valued parameters", {
  dist <- dist_inflated(dist_negative_binomial(10, 0.6), prob = 0.5)
  p <- parameters(dist, recursive = TRUE)

  expect_identical(names(p), c("dist.n", "dist.p", "x", "p"))
  expect_false(any(vapply(p, is_distribution, logical(1L))))
  expect_equal(p$dist.n, 10)
  expect_equal(p$dist.p, 0.6)
  expect_equal(p$x, 0)
  expect_equal(p$p, 0.5)

  # Non-recursive columns are unaffected
  dist2 <- c(dist_normal(1:2), dist_poisson(3))
  expect_identical(parameters(dist2, recursive = TRUE), parameters(dist2))
})

test_that("parameters(recursive = TRUE) fully expands multiple levels of nesting", {
  base <- dist_negative_binomial(10, 0.6)
  shifted <- dist_transformed(base, function(x) x + 1, function(x) x - 1)
  dist <- dist_inflated(shifted, prob = 0.5, x = 0)

  p <- parameters(dist, recursive = TRUE)

  expect_false(any(vapply(p, is_distribution, logical(1L))))
  expect_true(all(!vapply(p, is.data.frame, logical(1L))))
  expect_identical(
    names(p),
    c("dist.dist.n", "dist.dist.p", "dist.transform", "dist.inverse", "x", "p")
  )
  expect_equal(p$dist.dist.n, 10)
  expect_equal(p$dist.dist.p, 0.6)
})

test_that("parameters(recursive = TRUE) works with vectorised distributions", {
  dist <- dist_inflated(
    dist_negative_binomial(c(10, 20), c(0.6, 0.4)),
    prob = 0.5
  )
  p <- parameters(dist, recursive = TRUE)

  expect_identical(names(p), c("dist.n", "dist.p", "x", "p"))
  expect_equal(p$dist.n, c(10, 20))
  expect_equal(p$dist.p, c(0.6, 0.4))
  expect_equal(nrow(p), 2L)
})

test_that("parameters(recursive = TRUE) handles multiple distribution-valued parameters", {
  # A distribution with two distribution-valued parameters either side of a
  # plain one, to check that expanding the first doesn't misplace the
  # second (or the plain parameter between them).
  dist <- new_dist(
    a = dist_negative_binomial(10, 0.6),
    mid = 99,
    b = dist_poisson(3),
    class = "dist_test_two_nested"
  )
  p <- parameters(dist, recursive = TRUE)

  expect_identical(names(p), c("a.n", "a.p", "mid", "b.l"))
  expect_equal(p$a.n, 10)
  expect_equal(p$a.p, 0.6)
  expect_equal(p$mid, 99)
  expect_equal(p$b.l, 3)
})

test_that("family() returns the outer family name by default", {
  dist <- c(dist_normal(1:2), dist_poisson(3))
  expect_identical(family(dist), c("normal", "normal", "poisson"))

  mix <- dist_mixture(dist_normal(0, 1), dist_normal(5, 2), weights = c(0.3, 0.7))
  expect_identical(family(mix), "mixture")
})

test_that("family(recursive = TRUE) leaves simple distributions as scalars", {
  dist <- c(dist_normal(1:2), dist_poisson(3))
  expect_identical(family(dist, recursive = TRUE), list("normal", "normal", "poisson"))
})

test_that("family(recursive = TRUE) expands a single embedded distribution", {
  dist <- dist_inflated(dist_negative_binomial(10, 0.6), prob = 0.5)
  fam <- family(dist, recursive = TRUE)

  expect_identical(fam, list(list(family = "inflated", dist = "negbin")))
})

test_that("family(recursive = TRUE) expands a list of embedded distributions", {
  mix <- dist_mixture(dist_normal(0, 1), dist_normal(5, 2), weights = c(0.3, 0.7))
  fam <- family(mix, recursive = TRUE)

  expect_identical(
    fam,
    list(list(family = "mixture", dist = list("normal", "normal")))
  )
})

test_that("family(recursive = TRUE) fully expands multiple levels of nesting", {
  base <- dist_negative_binomial(10, 0.6)
  shifted <- dist_transformed(base, function(x) x + 1, function(x) x - 1)
  dist <- dist_inflated(shifted, prob = 0.5, x = 0)

  fam <- family(dist, recursive = TRUE)

  expect_identical(
    fam,
    list(list(
      family = "inflated",
      dist = list(family = "transformed", dist = "negbin")
    ))
  )
})

test_that("family(recursive = TRUE) works with vectorised and mixed distributions", {
  mix <- dist_mixture(dist_normal(0, 1), dist_normal(5, 2), weights = c(0.3, 0.7))
  dist <- c(mix, dist_poisson(3))

  fam <- family(dist, recursive = TRUE)

  expect_length(fam, 2L)
  expect_identical(fam[[1]], list(family = "mixture", dist = list("normal", "normal")))
  expect_identical(fam[[2]], "poisson")
})

test_that("family(recursive = TRUE) handles multiple distribution-valued fields", {
  dist <- new_dist(
    a = dist_negative_binomial(10, 0.6),
    mid = 99,
    b = dist_poisson(3),
    class = "dist_test_two_nested"
  )
  fam <- family(dist, recursive = TRUE)

  expect_identical(fam, list(list(family = "test_two_nested", a = "negbin", b = "poisson")))
})

test_that("pillar_shaft() min_width is not inflated by padded parameters", {
  dist <- c(dist_normal(200.123456, 12.3456), dist_gamma(1.2345, 3.21))
  shaft <- pillar::pillar_shaft(dist)

  expect_lte(attr(shaft, "min_width"), attr(shaft, "width"))
  expect_false(any(grepl("  ", shaft$dist_min)))

  out <- format(pillar::pillar(dist, "d"), width = 15)
  expect_true(any(grepl("<dist>", out, fixed = TRUE)))
  expect_true(any(grepl("N(200, 152)", out, fixed = TRUE)))
})

test_that("dist_paired() pairs its argument with the distributions", {
  dist <- c(dist_normal(0, 1), dist_poisson(3), dist_normal(1, 2))
  expect_equal(dist_paired(cdf, dist, q = c(0, 2, 3)), c(0.5, ppois(2, 3), pnorm(3, 1, 2)))
  # Mapped by cdf(), but paired by dist_paired()
  expect_length(cdf(dist, c(0, 2, 3)), 3)
  expect_equal(dist_paired(cdf, dist, q = 1), c(pnorm(1), ppois(1, 3), pnorm(1, 1, 2)))
  expect_equal(
    dist_paired(quantile, dist, p = 0.5, tail = "upper"),
    c(0, qpois(0.5, 3, lower.tail = FALSE), 1)
  )
})

test_that("c() casts non-distribution inputs", {
  x <- c(dist_normal(), NA, 1)
  expect_equal(mean(x), c(0, NA, 1))
  expect_equal(density(x, 1), c(dnorm(1), NA, 1))
})

test_that("likelihood() pairs samples with distributions by position", {
  x <- c(dist_normal(), dist_poisson(2))
  expect_equal(likelihood(x, list(1, 2)), c(dnorm(1), dpois(2, 2)))
  expect_equal(likelihood(x, list(c(1, 2))), c(dnorm(1) * dnorm(2), dpois(1, 2) * dpois(2, 2)))
  expect_equal(likelihood(x, list(1, 2), log = TRUE), c(dnorm(1, log = TRUE), dpois(2, 2, log = TRUE)))
  expect_error(likelihood(x, list(1, 2, 3)), "recycle")
})

test_that("integer probabilities and interval sizes are accepted", {
  expect_equal(quantile(dist_normal(), 1L), Inf)
  expect_equal(hilo(dist_normal(), 95L), hilo(dist_normal(), 95))
})

test_that("empty and missing distributions give results of the usual type", {
  x <- c(dist_normal(), dist_poisson(2))
  for (d in list(x[0], x[c(NA, NA)])) {
    n <- length(d)
    expect_identical(density(d, 1), rep(NA_real_, n))
    expect_identical(cdf(d, 1), rep(NA_real_, n))
    expect_identical(quantile(d, 0.5), rep(NA_real_, n))
    expect_identical(mean(d), rep(NA_real_, n))
    expect_identical(has_symmetry(d), rep(NA, n))
    expect_identical(format(d), rep(NA_character_, n))
    expect_identical(likelihood(d, list(1)), rep(NA_real_, n))
    expect_identical(generate(d, 1), vector("list", n))
    expect_identical(covariance(d), vector("list", n))
    expect_s3_class(hilo(d), "hilo")
    expect_s3_class(support(d), "support_region")
    expect_type(density(d, 1:2), "list")
  }
  expect_identical(density(x[0], list(a = 1)), data.frame(a = double()))
})

test_that("same-family slots are merged, even when not adjacent", {
  parts <- lapply(1:20, function(i) if (i %% 2) dist_normal(i) else dist_poisson(i))
  x <- do.call(c, parts)
  expect_length(x@x, 2L)
  expect_length(x, 20L)
  expect_equal(family(x), rep(c("normal", "poisson"), 10))
  expect_equal(mean(x), as.numeric(1:20))
  expect_equal(density(x, 2)[[1]], dist_density_ref <- stats::dnorm(2, 1, 1))
  expect_equal(format(x[1:4]), c("N(1, 1)", "Pois(2)", "N(3, 1)", "Pois(4)"))

  # Missing indices stay missing, and reordering is kept
  y <- c(dist_normal(1:2)[c(2, NA)], dist_poisson(3), dist_normal(4))
  expect_length(y@x, 2L)
  expect_equal(format(y), c("N(2, 1)", NA, "Pois(3)", "N(4, 1)"))
  expect_equal(is.na(y), c(FALSE, TRUE, FALSE, FALSE))

  # Variate names are kept apart
  a <- dist_multivariate_normal(mu = list(c(1, 2)), sigma = list(diag(2)))
  b <- a
  variates(b) <- c("p", "q")
  z <- c(a, dist_normal(0), b, a)
  expect_length(z@x, 3L)
  expect_equal(family(z), rep(c("mvnorm", "normal", "mvnorm", "mvnorm"), 1L))
  expect_equal(dist_elem(z, 3L), dist_elem(b, 1L))

  # Names of the inputs aren't carried into the merged records
  m <- function(a) dist_mixture(dist_normal(a), dist_normal(a + 1), weights = c(0.5, 0.5))
  nm <- c(first = m(0), second = m(10))
  expect_equal(mean(nm), c(0.5, 10.5))
})

test_that("all.equal() compares distributions by family and parameters", {
  x <- c(dist_normal(1:2), dist_poisson(3))
  expect_true(all.equal(x, c(dist_normal(1:2), dist_poisson(3))))
  expect_match(all.equal(x, c(dist_normal(1:2), dist_poisson(4))), "Element 3: Component .l.")
  expect_match(all.equal(x, c(dist_normal(1:2), dist_gamma(3, 1))), "Element 3: family classes differ")
  expect_match(all.equal(dist_normal(1:3), c(dist_normal(1), dist_normal(5), dist_normal(3))), "Element 2:")
  expect_match(all.equal(x, x[1:2]), "Lengths")
  expect_true(all.equal(dist_normal(1:2)[2:1], dist_normal(2:1)))
})

test_that("as.character() formats distributions", {
  x <- c(dist_normal(1, 2), dist_poisson(3))
  expect_equal(as.character(x), format(x))
  expect_type(as.character(x), "character")
})

test_that("Summary functions other than sum() are not supported", {
  x <- dist_normal(1:3)
  expect_error(min(x), "not supported")
  expect_error(max(x), "not supported")
  expect_error(range(x), "not supported")
  expect_error(prod(x), "not supported")
  expect_equal(format(sum(x)), "N(6, 3)")
})
