hdr_ends <- function(x) {
  list(
    lower = vctrs::field(x, "lower")[[1]],
    upper = vctrs::field(x, "upper")[[1]]
  )
}

# The smallest set of the points `k` (with masses `p`) with probability at
# least `size`%
hdr_set <- function(k, p, size) {
  o <- order(p, decreasing = TRUE)
  sort(k[o[seq_len(which(cumsum(p[o]) >= size / 100)[1])]])
}

test_that("hdr() of discrete distributions is the smallest set of points", {
  # {0, ..., 6} has probability 0.9665
  expect_equal(hdr_set(0:100, dpois(0:100, 3), 95), 0:6)
  expect_equal(hdr_ends(hdr(dist_poisson(3))), list(lower = 0, upper = 6))
  expect_equal(hdr_ends(hdr(dist_poisson(3), 80)), list(lower = 1, upper = 5))
  expect_equal(range(hdr_set(0:100, dpois(0:100, 3), 80)), c(1, 5))

  # The result doesn't depend on `n`
  expect_equal(hdr(dist_poisson(3), n = 10), hdr(dist_poisson(3)))
  expect_equal(hdr(dist_poisson(3), n = 1e4), hdr(dist_poisson(3)))

  # Binomial distributions (also when symmetric)
  expect_equal(range(hdr_set(0:10, dbinom(0:10, 10, 0.3), 95)), c(1, 6))
  expect_equal(hdr_ends(hdr(dist_binomial(10, 0.3))), list(lower = 1, upper = 6))
  expect_equal(range(hdr_set(0:10, dbinom(0:10, 10, 0.5), 95)), c(2, 8))
  expect_equal(hdr_ends(hdr(dist_binomial(10, 0.5))), list(lower = 2, upper = 8))
  # Points tied with the last one needed are kept: 3 and 7 are equally likely
  expect_equal(hdr_ends(hdr(dist_binomial(10, 0.5), 80)), list(lower = 3, upper = 7))

  # Negative binomial, truncated and inflated distributions
  expect_equal(
    hdr_ends(hdr(dist_negative_binomial(3, 0.4))),
    as.list(stats::setNames(range(hdr_set(0:200, dnbinom(0:200, 3, 0.4), 95)), c("lower", "upper")))
  )
  p <- dpois(2:8, 3) / sum(dpois(2:8, 3))
  expect_equal(range(hdr_set(2:8, p, 95)), c(2, 6))
  expect_equal(hdr_ends(hdr(dist_truncated(dist_poisson(3), 2, 8))), list(lower = 2, upper = 6))
  p <- 0.8 * dpois(0:100, 3) + 0.2 * (0:100 == 0)
  expect_equal(range(hdr_set(0:100, p, 95)), c(0, 6))
  expect_equal(hdr_ends(hdr(dist_inflated(dist_poisson(3), 0.2))), list(lower = 0, upper = 6))

  # Logical support
  expect_equal(hdr_ends(hdr(dist_bernoulli(0.3))), list(lower = 0, upper = 1))
  expect_equal(hdr_ends(hdr(dist_bernoulli(0.3), 50)), list(lower = 0, upper = 0))

  # Vectors of discrete and continuous distributions
  x <- hdr(c(dist_poisson(3), dist_poisson(10), dist_normal()))
  expect_equal(vctrs::field(x, "upper")[[1]], 6)
  expect_equal(
    c(vctrs::field(x, "lower")[[2]], vctrs::field(x, "upper")[[2]]),
    range(hdr_set(0:100, dpois(0:100, 10), 95))
  )
  expect_equal(vctrs::field(x, "upper")[[3]], qnorm(0.975))
})

test_that("hdr() of multimodal discrete distributions has gaps", {
  # A mixture of Poisson(2) and Poisson(15)
  k <- 0:200
  p <- 0.5 * dpois(k, 2) + 0.5 * dpois(k, 15)
  set <- hdr_set(k, p, 80)
  expect_equal(set, c(0:4, 11:18))
  x <- hdr(dist_mixture(dist_poisson(2), dist_poisson(15), weights = c(0.5, 0.5)), 80)
  expect_equal(hdr_ends(x), list(lower = c(0, 11), upper = c(4, 18)))
  x <- hdr(dist_mixture(dist_poisson(2), dist_poisson(15), weights = c(0.5, 0.5)), 80, n = 1e4)
  expect_equal(hdr_ends(x), list(lower = c(0, 11), upper = c(4, 18)))
})

test_that("hdr() of transformed discrete distributions enumerates their points", {
  # (Y - 3)^2 for Y ~ Poisson(3) has points 0, 1, 4, 9, ... with masses
  # P(Y = 3), P(Y = 2) + P(Y = 4), P(Y = 1) + P(Y = 5), ...
  y <- 0:100
  g <- (y - 3)^2
  p <- tapply(dpois(y, 3), g, sum)
  k <- as.numeric(names(p))
  expect_equal(hdr_set(k, p, 95), c(0, 1, 4, 9))
  expect_equal(hdr_ends(hdr((dist_poisson(3) - 3)^2)), list(lower = 0, upper = 9))
  # Without 0: {1, 4}
  expect_equal(hdr_set(k, p, 60), c(1, 4))
  expect_equal(hdr_ends(hdr((dist_poisson(3) - 3)^2, 60)), list(lower = 1, upper = 4))

  # |Y - 5| for Y ~ Binomial(10, 0.5) is most likely 1 (not 0), so its 40% HDR
  # is {1}, and its 70% HDR is {0, 1, 2}
  y <- 0:10
  p <- tapply(dbinom(y, 10, 0.5), abs(y - 5), sum)
  expect_equal(hdr_set(0:5, p, 40), 1)
  expect_equal(hdr_ends(hdr(abs(dist_binomial(10, 0.5) - 5), 40)), list(lower = 1, upper = 1))
  expect_equal(hdr_set(0:5, p, 70), 0:2)
  expect_equal(hdr_ends(hdr(abs(dist_binomial(10, 0.5) - 5), 70)), list(lower = 0, upper = 2))
})
