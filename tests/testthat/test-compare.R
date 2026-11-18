test_that("== and != compare the family and parameters", {
  x <- dist_normal(1:3)
  expect_identical(x == dist_normal(2), c(FALSE, TRUE, FALSE))
  expect_identical(dist_normal(2) == x, c(FALSE, TRUE, FALSE))
  expect_identical(x != dist_normal(2), c(TRUE, FALSE, TRUE))
  expect_identical(x == x, rep(TRUE, 3))

  # Same parameters in a different family
  expect_false(dist_normal(1, 1) == dist_gamma(1, 1))
  expect_true(dist_normal(1, 1) != dist_gamma(1, 1))

  # Mixed families are paired by position
  y <- c(dist_normal(1), dist_gamma(2, 1), dist_poisson(3))
  expect_identical(
    y == c(dist_normal(1), dist_normal(1), dist_poisson(3)),
    c(TRUE, FALSE, TRUE)
  )
  expect_identical(y == dist_gamma(2, 1), c(FALSE, TRUE, FALSE))

  # Transformed distributions compare the base distribution and transformation
  expect_true(exp(dist_gamma(1, 1)) == exp(dist_gamma(1, 1)))
  expect_false(exp(dist_gamma(1, 1)) == exp(dist_gamma(2, 1)))
  expect_false(exp(dist_gamma(1, 1)) == log(dist_gamma(1, 1)))

  # Nested and multivariate distributions
  expect_identical(
    dist_truncated(dist_normal(), 0, 1) == dist_truncated(dist_normal(), 0, 1:2),
    c(TRUE, FALSE)
  )
  m <- dist_mixture(dist_normal(), dist_gamma(1, 1), weights = c(0.5, 0.5))
  expect_true(m == m)
  expect_false(m == dist_mixture(dist_normal(1), dist_gamma(1, 1), weights = c(0.5, 0.5)))
  mv <- dist_multivariate_normal(list(1:2), list(diag(2)))
  expect_true(mv == mv)
  expect_false(mv == dist_multivariate_normal(list(1:2), list(diag(2) * 2)))
  expect_identical(dist_sample(list(1:3)) == dist_sample(list(1:3, 1:4)), c(TRUE, FALSE))
})

test_that("== casts numbers to degenerate distributions", {
  expect_true(dist_degenerate(1) == 1)
  expect_true(1 == dist_degenerate(1))
  expect_false(dist_degenerate(1) == 2)
  expect_false(dist_normal(1) == 1)
  expect_true(dist_normal(1) != 1)
  expect_identical(dist_degenerate(1:3) == c(1, 5, 3), c(TRUE, FALSE, TRUE))
  expect_identical(dist_normal(1:3) == 1, rep(FALSE, 3))
})

test_that("== with missing distributions is NA", {
  x <- c(dist_normal(1), NA, dist_poisson(3))
  expect_identical(x == dist_normal(1), c(TRUE, NA, FALSE))
  expect_identical(x != dist_normal(1), c(FALSE, NA, TRUE))
  expect_identical(x == NA, rep(NA, 3))
  expect_identical(dist_missing(2) == dist_normal(), c(NA, NA))
  expect_identical(dist_normal() == dist_missing(2), c(NA, NA))
})

test_that("== handles zero-length and rep-compressed inputs", {
  expect_identical(dist_normal(1:2)[0] == 1, logical())
  expect_identical(dist_normal(1:2)[0] == dist_normal(), logical())
  x <- rep(dist_normal(1:2), 3)
  expect_identical(x == dist_normal(2), rep(c(FALSE, TRUE), 3))
  expect_identical(x == rep(dist_normal(1:2), each = 3), c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE))
})

test_that("== errors for incompatible operands", {
  expect_error(dist_normal() == "a", "convert")
  expect_error(dist_normal(1:3) == 1:2, "recycle")
})

test_that("arithmetic still dispatches through Ops", {
  x <- dist_normal(1:3)
  expect_identical(format(x + 1), format(dist_normal(2:4)))
  expect_identical(format(1 - x), format(dist_normal(0:-2)))
  expect_identical(format(-x), format(dist_normal(-(1:3))))
})

# The probability of a Bernoulli result
prob <- function(x) mean(x)

test_that("ordering comparisons of continuous distributions use the cdf", {
  x <- dist_normal(1, 2)
  q <- 0.5
  p <- pnorm(q, 1, 2)
  expect_equal(prob(x < q), p)
  expect_equal(prob(x <= q), p)
  expect_equal(prob(x > q), 1 - p)
  expect_equal(prob(x >= q), 1 - p)
  expect_equal(prob(q > x), p)
  expect_equal(prob(q >= x), p)
  expect_equal(prob(q < x), 1 - p)
  expect_equal(prob(q <= x), 1 - p)

  g <- dist_gamma(2, 3)
  expect_equal(prob(g < 1), pgamma(1, 2, 3))
  expect_equal(prob(1 < g), pgamma(1, 2, 3, lower.tail = FALSE))
})

test_that("ordering comparisons include the mass at the threshold for discrete distributions", {
  x <- dist_poisson(3)
  expect_equal(prob(x < 2), ppois(1, 3))
  expect_equal(prob(x <= 2), ppois(2, 3))
  expect_equal(prob(x > 2), ppois(2, 3, lower.tail = FALSE))
  expect_equal(prob(x >= 2), ppois(1, 3, lower.tail = FALSE))
  expect_equal(prob(2 > x), ppois(1, 3))
  expect_equal(prob(2 >= x), ppois(2, 3))
  expect_equal(prob(2 < x), ppois(2, 3, lower.tail = FALSE))
  expect_equal(prob(2 <= x), ppois(1, 3, lower.tail = FALSE))

  # Between the support points
  expect_equal(prob(x < 2.5), ppois(2, 3))
  expect_equal(prob(x >= 2.5), ppois(2, 3, lower.tail = FALSE))

  b <- dist_binomial(10, 0.3)
  expect_equal(prob(b >= 3), pbinom(2, 10, 0.3, lower.tail = FALSE))
  expect_equal(prob(b < 3), pbinom(2, 10, 0.3))

  expect_equal(prob(dist_degenerate(1) >= 1), 1)
  expect_equal(prob(dist_degenerate(1) > 1), 0)
  expect_equal(prob(dist_degenerate(1) <= 1), 1)
  expect_equal(prob(dist_degenerate(1) < 1), 0)

  s <- dist_sample(list(c(1, 2, 2, 3)))
  expect_equal(prob(s >= 2), 0.75)
  expect_equal(prob(s > 2), 0.25)
  expect_equal(prob(s < 2), 0.25)
  expect_equal(prob(s <= 2), 0.75)

  # Transformed discrete distributions use the mass of the base distribution
  y <- 2 * dist_poisson(3)
  expect_equal(prob(y >= 4), ppois(1, 3, lower.tail = FALSE))
  expect_equal(prob(y < 4), ppois(1, 3))
})

test_that("ordering comparisons include the point masses of mixed distributions", {
  # Inflation of a continuous distribution
  x <- dist_inflated(dist_normal(), 0.2, 0)
  expect_equal(prob(x >= 0), 0.2 + 0.8 * 0.5)
  expect_equal(prob(x > 0), 0.8 * 0.5)
  expect_equal(prob(x < 0), 0.8 * 0.5)
  expect_equal(prob(x <= 0), cdf(x, 0))
  expect_equal(prob(x >= 1), pnorm(1, lower.tail = FALSE) * 0.8)

  # Inflation at a non-zero point, of continuous and discrete distributions
  x <- dist_inflated(dist_normal(), 0.3, 1.5)
  expect_equal(prob(x >= 1.5), 0.3 + 0.7 * pnorm(1.5, lower.tail = FALSE))
  expect_equal(prob(x < 1.5), 0.7 * pnorm(1.5))
  x <- dist_inflated(dist_poisson(3), 0.2, 2)
  expect_equal(prob(x >= 2), 0.2 + 0.8 * ppois(1, 3, lower.tail = FALSE))
  expect_equal(prob(x < 2), 0.8 * ppois(1, 3))
  expect_equal(prob(x >= 3), 0.8 * ppois(2, 3, lower.tail = FALSE))

  # Mixtures of continuous and discrete distributions
  m <- dist_mixture(dist_normal(), dist_poisson(3), weights = c(0.5, 0.5))
  expect_equal(
    prob(m >= 2),
    0.5 * pnorm(2, lower.tail = FALSE) + 0.5 * ppois(1, 3, lower.tail = FALSE)
  )
  expect_equal(
    prob(m > 2),
    0.5 * pnorm(2, lower.tail = FALSE) + 0.5 * ppois(2, 3, lower.tail = FALSE)
  )
  expect_equal(prob(m < 2), 0.5 * pnorm(2) + 0.5 * ppois(1, 3))
  expect_equal(
    prob(m >= c(0, 1.5, 3)),
    0.5 * pnorm(c(0, 1.5, 3), lower.tail = FALSE) +
      0.5 * c(1, ppois(1, 3, lower.tail = FALSE), ppois(2, 3, lower.tail = FALSE))
  )
})

test_that("ordering comparisons treat thresholds within rounding error of an integer as integers", {
  x <- dist_poisson(3)
  expect_equal(prob(x >= 3 - 1e-12), ppois(2, 3, lower.tail = FALSE))
  expect_equal(prob(x >= 3 + 1e-12), ppois(2, 3, lower.tail = FALSE))
  expect_equal(prob(x < 3 - 1e-12), ppois(2, 3))
  # As in ppois(), which treats values within 1e-7 of an integer as integers
  expect_equal(prob(x >= 3 - 1e-6), ppois(2, 3, lower.tail = FALSE))
  expect_equal(prob(x >= 3 - 1e-3), ppois(2, 3, lower.tail = FALSE))
  expect_equal(prob(x > 3 - 1e-3), ppois(2, 3, lower.tail = FALSE))
  expect_equal(prob(x > 3 - 1e-12), ppois(3, 3, lower.tail = FALSE))
})

test_that("ordering comparisons of transformed discrete distributions use the mass at the preimage", {
  # 0.3 / 0.1 is 2.9999999999999996
  y <- 0.1 * dist_poisson(3)
  q <- c(0, 0.1, 0.2, 0.3, 0.7, 0.25)
  k <- c(0, 1, 2, 3, 7, 2.5)
  expect_equal(prob(y >= q), ppois(ceiling(k) - 1, 3, lower.tail = FALSE))
  expect_equal(prob(y > q), ppois(floor(k), 3, lower.tail = FALSE))
  expect_equal(prob(y < q), ppois(ceiling(k) - 1, 3))
  expect_equal(prob(y <= q), ppois(floor(k), 3))
  expect_equal(prob(y >= 0.3), ppois(2, 3, lower.tail = FALSE))

  # 3 isn't in the image of the support, so it has no mass
  y <- 2 * dist_poisson(3)
  expect_equal(prob(y >= 3), ppois(1, 3, lower.tail = FALSE))
  expect_equal(prob(y > 3), ppois(1, 3, lower.tail = FALSE))
  expect_equal(prob(y < 3), ppois(1, 3))
  expect_equal(point_mass(y, 3), 0)
  expect_equal(point_mass(y, c(3, 4, 6)), c(0, dpois(2:3, 3)))

  # Other transformations
  expect_equal(point_mass(exp(dist_poisson(3)), exp(2)), dpois(2, 3))
  expect_equal(point_mass(dist_poisson(3) + 0.1, 2.1), dpois(2, 3))
  expect_equal(point_mass(dist_poisson(3) + 1e9, 1e9 + 2.5), 0)
  expect_equal(point_mass(sqrt(dist_poisson(3)), sqrt(2)), dpois(2, 3))
  # Continuous base distributions have no mass
  expect_equal(point_mass(exp(dist_normal()), 1), 0)
  # Transformations of mixed distributions keep their point masses
  expect_equal(prob(2 * dist_inflated(dist_normal(), 0.2, 0) >= 0), 0.6)
})

test_that("point_mass() gives the probability mass of each distribution", {
  expect_equal(point_mass(dist_poisson(3), 0:4), dpois(0:4, 3))
  expect_equal(point_mass(dist_poisson(3), c(2.5, -1, Inf)), c(0, 0, 0))
  expect_equal(point_mass(dist_normal(), 0), 0)
  expect_equal(point_mass(dist_bernoulli(0.3), c(0, 1)), c(0.7, 0.3))
  expect_equal(point_mass(dist_binomial(4, 0.5), 2), dbinom(2, 4, 0.5))
  expect_equal(point_mass(dist_wrap("pois", lambda = 3), 2), dpois(2, 3))
  expect_equal(point_mass(dist_degenerate(c(1, 2)), 1), c(1, 0))
  expect_equal(point_mass(dist_sample(list(c(1, 2, 2, 3))), c(2, 2.5)), c(0.5, 0))
  expect_equal(
    point_mass(dist_mixture(dist_degenerate(1), dist_poisson(1), weights = c(0.4, 0.6)), 1),
    0.4 + 0.6 * dpois(1, 1)
  )
  # Missing distributions and thresholds
  expect_equal(point_mass(c(dist_poisson(3), NA, dist_missing()), 2), c(dpois(2, 3), NA, NA))
  expect_equal(point_mass(dist_poisson(3), NA_real_), NA_real_)
  expect_equal(point_mass(dist_normal(), NA_real_), NA_real_)

  # Mixed families, with thresholds paired by position
  x <- c(
    dist_normal(), dist_poisson(3), dist_inflated(dist_normal(), 0.2, 1),
    dist_degenerate(4), 0.1 * dist_poisson(3), dist_poisson(2)
  )
  expect_equal(
    point_mass(x, c(0, 2, 1, 4, 0.3, 1 - 1e-12)),
    c(0, dpois(2, 3), 0.2, 1, dpois(3, 3), dpois(1, 2))
  )
  expect_equal(
    prob(x >= c(0, 2, 1, 4, 0.3, 1 - 1e-12)),
    c(0.5, ppois(1, 3, lower.tail = FALSE), 0.2 + 0.8 * pnorm(1, lower.tail = FALSE),
      1, ppois(2, 3, lower.tail = FALSE), ppois(0, 2, lower.tail = FALSE))
  )

  # rep()-compressed vectors
  x <- rep(c(dist_poisson(3), dist_inflated(dist_normal(), 0.5, 0)), 3)
  expect_equal(point_mass(x, 0), rep(c(dpois(0, 3), 0.5), 3))
  expect_equal(
    prob(x >= c(0, 0, 1, 1, 2, 2)),
    c(1, 0.75, ppois(0, 3, lower.tail = FALSE), 0.5 * pnorm(1, lower.tail = FALSE),
      ppois(1, 3, lower.tail = FALSE), 0.5 * pnorm(2, lower.tail = FALSE))
  )
  expect_length(point_mass(x[0], 1), 0)
})

test_that("point_mass() of continuous families is 0", {
  x <- c(
    dist_normal(), dist_gamma(2, 1), dist_uniform(0, 1), dist_student_t(3),
    dist_beta(2, 2), dist_exponential(1), dist_lognormal(), dist_weibull(2, 1)
  )
  expect_equal(point_mass(x, 1), rep(0, 8))
  expect_equal(point_mass(x, NA_real_), rep(NA_real_, 8))
  expect_equal(point_mass(dist_normal(1:3), c(1, NA, 3)), c(0, NA, 0))
  expect_equal(prob(dist_normal(0:2) >= 1), pnorm(1, 0:2, lower.tail = FALSE))
  expect_equal(prob(dist_normal(0:2) < 1), pnorm(1, 0:2))
})

test_that("point_mass() builds the support of one distribution per family", {
  n_built <- 0L
  sup <- support.dist_normal
  local_mocked_bindings(support.dist_normal = function(x, ...) {
    n_built <<- n_built + vctrs::vec_size(x)
    sup(x, ...)
  })
  expect_equal(point_mass(dist_normal(1:100), 1), rep(0, 100))
  expect_equal(n_built, 1L)
  expect_equal(point_mass(dist_poisson(1:3), c(1, 1.5, NA)), c(dpois(1, 1), 0, NA))
})

test_that("point_mass() checks the data type of each convolved or wrapped distribution", {
  # One storage slot holding both a lattice and a continuous distribution
  x <- c(dist_poisson(3) + dist_gamma(2, 1), dist_poisson(1) + dist_poisson(2))
  expect_equal(point_mass(x, 2), c(0, dpois(2, 3)))
  expect_equal(prob(x >= 2)[2], ppois(1, 3, lower.tail = FALSE))

  dcnt <- function(x, a) dpois(x, a)
  pcnt <- function(q, a, ...) ppois(q, a, ...)
  qcnt <- function(p, a, ...) qpois(p, a, ...)
  rcnt <- function(n, a) rpois(n, a)
  dcts <- function(x, a) dexp(x, a)
  pcts <- function(q, a, ...) pexp(q, a, ...)
  qcts <- function(p, a, ...) qexp(p, a, ...)
  rcts <- function(n, a) rexp(n, a)
  w <- c(dist_wrap("cts", a = 1), dist_wrap("cnt", a = 3))
  expect_equal(point_mass(w, 2), c(0, dpois(2, 3)))
  expect_equal(prob(w >= 2), c(pexp(2, 1, lower.tail = FALSE), ppois(1, 3, lower.tail = FALSE)))
})

test_that("point_mass() handles families with mass at a point of continuous support", {
  skip_if_not_installed("tweedieDistr")
  x <- dist_tweedie(1, 1, 1.5)
  expect_equal(point_mass(x, c(0, 1)), c(cdf(x, 0), 0))
  expect_equal(prob(x >= 0), 1)
  expect_equal(prob(x > 0), 1 - cdf(x, 0))
})

test_that("point_mass() of quantile mixtures of discrete components", {
  # Q(p) = sum(w * qpois(p, lambda)) is constant between the cdf values of the
  # components, so the mass at `a` is the total length of these intervals
  # where Q is `a`
  truth <- function(w, lambda, a) {
    b <- sort(unique(c(0, 1, unlist(lapply(lambda, function(l) ppois(0:80, l))))))
    mid <- (head(b, -1) + tail(b, -1)) / 2
    Q <- Reduce(`+`, Map(function(wk, l) wk * qpois(mid, l), w, lambda))
    vapply(a, function(ai) sum(diff(b)[abs(Q - ai) < 1e-9]), numeric(1))
  }
  q <- dist_mixture(dist_poisson(3), dist_poisson(5), weights = c(0.5, 0.5), type = "quantile")
  a <- c(0, 0.5, 2, 2.5, 3, 4, 7, 2.25, -1, 100)
  expect_equal(point_mass(q, a), truth(c(0.5, 0.5), c(3, 5), a))
  expect_equal(point_mass(q, 0), dpois(0, 5))
  q <- dist_mixture(dist_poisson(3), dist_poisson(5), weights = c(0.3, 0.7), type = "quantile")
  a <- c(0, 0.7, 1.4, 2.7, 3.7, 4.4, 1)
  expect_equal(point_mass(q, a), truth(c(0.3, 0.7), c(3, 5), a))

  # A continuous component makes the mixture continuous
  q <- dist_mixture(dist_normal(), dist_poisson(5), weights = c(0.5, 0.5), type = "quantile")
  expect_equal(point_mass(q, c(4, 2.5)), c(0, 0))

  # Vectors of mixtures, with missing thresholds
  q <- c(
    dist_mixture(dist_poisson(3), dist_poisson(5), weights = c(0.5, 0.5), type = "quantile"),
    dist_mixture(dist_normal(), dist_poisson(5), weights = c(0.5, 0.5), type = "quantile")
  )
  expect_equal(point_mass(q, c(2.5, 2.5)), c(truth(c(0.5, 0.5), c(3, 5), 2.5), 0))
  expect_equal(point_mass(q, NA_real_), c(NA_real_, NA_real_))
})

test_that("point_mass() of truncated distributions keeps the mass at the bounds", {
  x <- dist_truncated(dist_poisson(3), lower = 2, upper = 5)
  z <- ppois(5, 3) - ppois(1, 3)
  expect_equal(point_mass(x, 1:6), c(0, dpois(2:5, 3) / z, 0))
  expect_equal(point_mass(x, 2 - 1e-12), dpois(2, 3) / z)
  expect_equal(point_mass(x, 5 + 1e-12), dpois(5, 3) / z)
  expect_equal(sum(point_mass(x, 2:5)), 1)
  expect_equal(prob(x >= 2), 1)
  expect_equal(prob(x > 2), 1 - dpois(2, 3) / z)
  expect_equal(prob(x < 5), 1 - dpois(5, 3) / z)
  expect_equal(prob(x <= 5), 1)
  expect_equal(prob(x >= 4), (dpois(4, 3) + dpois(5, 3)) / z)

  # Non-integer bounds of discrete distributions
  x <- dist_truncated(dist_poisson(3), lower = 1.5, upper = 4.5)
  z <- ppois(4, 3) - ppois(1, 3)
  expect_equal(point_mass(x, 1:5), c(0, dpois(2:4, 3) / z, 0))
  expect_equal(prob(x >= 2), 1)

  # Continuous distributions have no mass at the bounds
  expect_equal(point_mass(dist_truncated(dist_normal(), 0, 1), c(0, 0.5, 1)), c(0, 0, 0))
})

test_that("ordering comparisons pair thresholds and mixed families by position", {
  x <- c(dist_normal(0, 1), dist_poisson(2), dist_gamma(2, 1))
  q <- c(1, 2, 3)
  expect_equal(
    prob(x >= q),
    c(pnorm(1, lower.tail = FALSE), ppois(1, 2, lower.tail = FALSE), pgamma(3, 2, 1, lower.tail = FALSE))
  )
  expect_equal(
    prob(q > x),
    c(pnorm(1), ppois(1, 2), pgamma(3, 2, 1))
  )
  expect_equal(prob(dist_normal(1:3) < 2), pnorm(2, 1:3))
  expect_equal(prob(dist_poisson(3) <= 0:2), ppois(0:2, 3))
  expect_error(dist_normal(1:3) < 1:2, "recycle")
})

test_that("ordering comparisons keep upper tail precision", {
  expect_equal(prob(dist_normal() > 10), pnorm(10, lower.tail = FALSE))
  expect_gt(prob(dist_normal() > 10), 0)
  expect_equal(prob(-10 > dist_normal()), pnorm(-10))
  expect_equal(prob(10 < dist_normal()), pnorm(10, lower.tail = FALSE))
})

test_that("comparing two distributions uses their difference", {
  expect_equal(
    prob(dist_normal(1, 1) > dist_normal(0, 2)),
    pnorm(0, 1, sqrt(5), lower.tail = FALSE)
  )
  expect_equal(
    prob(dist_normal(1:2) < dist_normal(0)),
    pnorm(0, 1:2, sqrt(2))
  )
  # Without a closed form the difference is convolved numerically
  expect_equal(
    prob(dist_gamma(2, 1) > dist_gamma(1, 1)),
    0.75, tolerance = 1e-3
  )
})

test_that("ordering comparisons with missing values are missing", {
  x <- c(dist_normal(), NA, dist_poisson(2))
  res <- x > c(0, 1, 2)
  expect_identical(is.na(res), c(FALSE, TRUE, FALSE))
  expect_equal(prob(res), c(0.5, NA, ppois(2, 2, lower.tail = FALSE)))
  expect_true(is.na(dist_normal() > NA))
  expect_identical(is.na(dist_missing(2) < 0), c(TRUE, TRUE))
  expect_identical(is.na(dist_normal() < dist_missing(2)), c(TRUE, TRUE))
})

test_that("ordering comparisons handle zero-length and rep-compressed inputs", {
  expect_length(dist_normal(1:2)[0] > 1, 0)
  expect_s3_class(dist_normal(1:2)[0] > 1, "distribution")
  expect_length(dist_normal() > numeric(), 0)
  x <- rep(dist_normal(1:2), 3)
  expect_equal(prob(x > 1), rep(pnorm(1, 1:2, lower.tail = FALSE), 3))
  expect_equal(prob(x <= 1:6), pnorm(1:6, rep(1:2, 3)))
})

test_that("ordering comparisons return Bernoulli distributions", {
  res <- dist_normal(0:1) > 0
  expect_true(is_distribution(res))
  expect_identical(family(res), c("bernoulli", "bernoulli"))
  expect_identical(format(res), c("Bernoulli(0.50)", "Bernoulli(0.84)"))
  expect_identical(format(dist_poisson(3) >= 2), format(dist_bernoulli(ppois(1, 3, lower.tail = FALSE))))
  expect_named(parameters(dist_normal() > 0), "p")
})

test_that("ordering comparisons error for unsupported operands", {
  expect_error(dist_normal() > "a", "Can't compare")
  expect_error(dist_multivariate_normal(list(1:2), list(diag(2))) > 0, "multivariate")
  expect_error(hilo(dist_normal()) < dist_normal(), "Can't compare")
})

test_that("logical operators combine independent Bernoulli distributions", {
  a <- dist_bernoulli(c(0.2, 0.5))
  b <- dist_bernoulli(c(0.3, 0.4))
  expect_equal(prob(!a), c(0.8, 0.5))
  expect_equal(prob(a & b), c(0.2 * 0.3, 0.5 * 0.4))
  expect_equal(prob(a | b), c(0.2 + 0.3 - 0.2 * 0.3, 0.5 + 0.4 - 0.5 * 0.4))
  expect_equal(prob(a & dist_bernoulli(0.5)), c(0.1, 0.25))

  x <- dist_normal()
  expect_equal(prob((x > -1) & (x < 1)), pnorm(-1, lower.tail = FALSE) * pnorm(1))
  expect_equal(prob(!(x > 1.96)), pnorm(1.96))
  expect_identical(format(!dist_bernoulli(0.25)), "Bernoulli(0.75)")

  # Degenerate distributions at 0 or 1 are certain events
  expect_equal(prob(dist_degenerate(1) & a), c(0.2, 0.5))
  expect_equal(prob(dist_degenerate(0) | a), c(0.2, 0.5))
  expect_equal(prob(!dist_degenerate(0)), 1)
})

test_that("logical operators treat logicals as certain events", {
  a <- dist_bernoulli(c(0.2, 0.5))
  expect_equal(prob(a & TRUE), c(0.2, 0.5))
  expect_equal(prob(TRUE & a), c(0.2, 0.5))
  expect_equal(prob(a & FALSE), c(0, 0))
  expect_equal(prob(FALSE | a), c(0.2, 0.5))
  expect_equal(prob(a | TRUE), c(1, 1))
  expect_equal(prob(a | c(TRUE, FALSE)), c(1, 0.5))
  expect_equal(prob(c(FALSE, TRUE) & a), c(0, 0.5))
  expect_equal(prob(a & 1), c(0.2, 0.5))

  # Missing values give missing results
  expect_identical(is.na(a & NA), c(TRUE, TRUE))
  expect_identical(is.na(NA | a), c(TRUE, TRUE))
  expect_identical(is.na(a & c(TRUE, NA)), c(FALSE, TRUE))
  expect_identical(is.na(c(dist_bernoulli(0.3), NA) & dist_bernoulli(0.5)), c(FALSE, TRUE))
  expect_identical(is.na(dist_missing(2) | a), c(TRUE, TRUE))
  expect_identical(is.na(!c(a, NA)), c(FALSE, FALSE, TRUE))
})

test_that("logical operators handle zero-length and rep-compressed inputs", {
  a <- dist_bernoulli(c(0.2, 0.5))
  expect_length(a[0] & TRUE, 0)
  expect_length(!a[0], 0)
  expect_s3_class(!a[0], "distribution")
  expect_equal(prob(rep(a, 2) & dist_bernoulli(0.5)), c(0.1, 0.25, 0.1, 0.25))
  expect_equal(prob(!rep(a, each = 2)), c(0.8, 0.8, 0.5, 0.5))
  expect_error(rep(a, 2) & a[c(1, 2, 1)], "recycle")
})

test_that("logical operators error for operands that aren't events", {
  a <- dist_bernoulli(0.2)
  expect_error(dist_normal() & a, "other than Bernoulli")
  expect_error(a | dist_poisson(2), "Element 1 is Pois\\(2\\)")
  expect_error(!dist_normal(), "other than Bernoulli")
  expect_error(c(a, dist_normal()) & TRUE, "Element 2")
  expect_error(dist_degenerate(2) & a, "other than Bernoulli")
  expect_error(a & 2, "0 or 1")
  expect_error(a & "a", "<character>")
})

test_that("unary minus and plus still dispatch through Ops", {
  expect_identical(format(-dist_normal(1)), "N(-1, 1)")
  expect_identical(format(+dist_normal(1)), "N(1, 1)")
})
