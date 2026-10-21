test_that("dist_wrap() methods are vectorised over distributions", {
  x <- dist_wrap("norm", mean = 1:3, sd = c(1, 2, 3))
  expect_equal(format(x), c("norm(1, 1)", "norm(2, 2)", "norm(3, 3)"))
  expect_equal(density(x, 0.5), dnorm(0.5, 1:3, 1:3))
  expect_equal(density(x, c(0, 1, 2)), list(dnorm(c(0, 1, 2), 1, 1), dnorm(c(0, 1, 2), 2, 2), dnorm(c(0, 1, 2), 3, 3)))
  expect_equal(log_density(x, 0.5), dnorm(0.5, 1:3, 1:3, log = TRUE))
  expect_equal(cdf(x, 2), pnorm(2, 1:3, 1:3))
  expect_equal(cdf(x, 2, tail = "upper"), pnorm(2, 1:3, 1:3, lower.tail = FALSE))
  expect_equal(quantile(x, 0.3), qnorm(0.3, 1:3, 1:3))
  expect_equal(quantile(x, 0.3, tail = "upper"), qnorm(0.3, 1:3, 1:3, lower.tail = FALSE))
  expect_equal(median(x), c(1, 2, 3))

  set.seed(1)
  g <- generate(x, c(2, 3, 4))
  expect_equal(lengths(g), c(2, 3, 4))
  expect_equal(vapply(g, vec_size, 1L), c(2L, 3L, 4L))
})

test_that("dist_wrap() groups by function and environment", {
  env <- new.env()
  env$dmyexp <- function(x, rate = 1) dexp(x, rate)
  env$pmyexp <- function(q, rate = 1) 1 - exp(-rate * q)
  env$qmyexp <- function(p, rate = 1) -log(1 - p) / rate
  env$rmyexp <- function(n, rate = 1) rexp(n, rate)
  env2 <- new.env()
  env2$dmyexp <- function(x, rate = 1) rep(-1, length(x))
  env2$pmyexp <- function(q, rate = 1) rep(-2, length(q))
  env2$qmyexp <- function(p, rate = 1) rep(-3, length(p))
  env2$rmyexp <- function(n, rate = 1) rep(-4, n)

  x <- c(
    dist_wrap("myexp", rate = 1, package = env),
    dist_wrap("myexp", rate = 1, package = env2),
    dist_wrap("exp", rate = 3),
    dist_wrap("myexp", rate = 2, package = env),
    dist_wrap("exp", rate = 4)
  )
  expect_equal(density(x, 0.5), c(dexp(0.5, 1), -1, dexp(0.5, 3), dexp(0.5, 2), dexp(0.5, 4)))
  expect_equal(cdf(x, 0.5), c(pexp(0.5, 1), -2, pexp(0.5, 3), pexp(0.5, 2), pexp(0.5, 4)))
  expect_equal(cdf(x, 0.5, tail = "upper"), c(1 - pexp(0.5, 1), 3, pexp(0.5, 3, lower.tail = FALSE), 1 - pexp(0.5, 2), pexp(0.5, 4, lower.tail = FALSE)))
  expect_equal(quantile(x, 0.5), c(qexp(0.5, 1), -3, qexp(0.5, 3), qexp(0.5, 2), qexp(0.5, 4)))

  g <- generate(x, c(1, 2, 3, 2, 1))
  expect_equal(lengths(g), c(1, 2, 3, 2, 1))
  expect_equal(g[[2]], c(-4, -4))
  expect_equal(format(x), c("myexp(1)", "myexp(1)", "exp(3)", "myexp(2)", "exp(4)"))
})
