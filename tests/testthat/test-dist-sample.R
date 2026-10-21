test_that("Emperical/sample distribution", {
  x <- generate(dist_normal(0, 1), 100)
  dist <- dist_sample(x)

  expect_equal(format(dist), "sample[100]")

  # quantiles
  expect_equal(quantile(dist, 0.6), unname(quantile(x[[1]], 0.6)))
  expect_equal(quantile(dist, 0.24), unname(quantile(x[[1]], 0.24)))

  # pdf

  # cdf

  # F(Finv(a)) ~= a

  # stats
  expect_equal(mean(dist), mean(x[[1]]))
  expect_equal(median(dist), median(x[[1]]))
  expect_equal(median(dist[[1]]), median(x[[1]]))
  expect_equal(variance(dist), var(x[[1]]))

  # transform
  expect_equal(
    dist,
    dist + 1 - 1
  )
})

test_that("CDF of degenerate dist_sample() is correct", {
  expect_equal(cdf(dist_sample(list(2)), 2)[[1]], 1)
})

test_that("dist_sample() methods are vectorised over samples of different lengths", {
  set.seed(1)
  xs <- list(rnorm(10), rnorm(25, 2), rnorm(40, 5))
  dist <- dist_sample(xs)

  expect_equal(format(dist), c("sample[10]", "sample[25]", "sample[40]"))
  expect_equal(mean(dist), vapply(xs, mean, numeric(1L)))
  expect_equal(median(dist), vapply(xs, median, numeric(1L)))
  expect_equal(variance(dist), vapply(xs, var, numeric(1L)))

  # Broadcast and paired quantiles, with `type` passed to stats::quantile()
  expect_equal(quantile(dist, 0.3), vapply(xs, quantile, numeric(1L), probs = 0.3, names = FALSE))
  expect_equal(
    quantile(dist, 0.3, type = 6),
    vapply(xs, quantile, numeric(1L), probs = 0.3, type = 6, names = FALSE)
  )
  expect_equal(
    quantile(dist, c(0.1, 0.5, 0.9), type = 1)[[3]],
    quantile(xs[[3]], c(0.1, 0.5, 0.9), type = 1, names = FALSE)
  )

  # cdf
  expect_equal(cdf(dist, 1), vapply(xs, function(x) mean(x <= 1), numeric(1L)))
  expect_equal(
    cdf(dist, 1, tail = "upper"),
    vapply(xs, function(x) mean(x > 1), numeric(1L))
  )
  expect_equal(
    cdf(dist, list(q = c(0, 2, 5)))[[1]],
    c(mean(xs[[1]] <= 0), mean(xs[[2]] <= 2), mean(xs[[3]] <= 5))
  )

  # density
  expect_equal(
    density(dist, 0.5),
    vapply(xs, function(x) density(x, from = 0.5, to = 0.5, n = 1)$y, numeric(1L))
  )

  # support
  expect_equal(
    vctrs::field(support(dist), "lim"),
    lapply(xs, range)
  )

  # generate draws from the right sample, in the same random stream as a loop
  set.seed(2)
  g <- generate(dist, list(c(3L, 5L, 2L)))
  expect_equal(lengths(g), c(3L, 5L, 2L))
  set.seed(2)
  expect_equal(g, lapply(seq_along(xs), function(i) xs[[i]][sample.int(length(xs[[i]]), c(3L, 5L, 2L)[i], replace = TRUE)]))

  # transformations
  expect_equal(mean(dist + 1:3), mean(dist) + 1:3)
  expect_equal(mean(exp(dist)), vapply(xs, function(x) mean(exp(x)), numeric(1L)))
})

test_that("multivariate dist_sample() methods are vectorised", {
  set.seed(1)
  xs <- list(cbind(rnorm(10), rnorm(10, 5)), cbind(rnorm(30, 1), rnorm(30, 6)))
  dist <- dist_sample(xs)

  expect_equal(format(dist), c("sample[10]", "sample[30]"))
  expect_equal(mean(dist), rbind(colMeans(xs[[1]]), colMeans(xs[[2]])))
  expect_equal(median(dist), rbind(apply(xs[[1]], 2, median), apply(xs[[2]], 2, median)))
  expect_equal(covariance(dist), lapply(xs, cov))
  expect_equal(variance(dist), rbind(diag(cov(xs[[1]])), diag(cov(xs[[2]]))))
  expect_equal(
    quantile(dist, 0.5),
    rbind(apply(xs[[1]], 2, quantile, 0.5, names = FALSE), apply(xs[[2]], 2, quantile, 0.5, names = FALSE))
  )
  expect_equal(
    cdf(dist, list(q = rbind(c(0, 5), c(1, 6))))[[1]],
    c(mean(t(t(xs[[1]]) <= c(0, 5))), mean(t(t(xs[[2]]) <= c(1, 6))))
  )
  expect_equal(
    cdf(dist, matrix(c(0, 5), nrow = 1)),
    c(mean(t(t(xs[[1]]) <= c(0, 5))), mean(t(t(xs[[2]]) <= c(0, 5))))
  )

  g <- generate(dist, list(c(4L, 7L)))
  expect_equal(vapply(g, nrow, integer(1L)), c(4L, 7L))
  expect_equal(vapply(g, ncol, integer(1L)), c(2L, 2L))
})
