test_that("Dirichlet distribution", {
  alpha <- c(2, 5, 3)
  dist <- dist_dirichlet(alpha = list(alpha))

  # format
  expect_equal(format(dist), "Dirichlet[3]")

  # stats
  alpha_0 <- sum(alpha)
  expect_equal(
    mean(dist),
    matrix(alpha / alpha_0, nrow = 1)
  )

  expect_equal(
    variance(dist),
    matrix(alpha * (alpha_0 - alpha) / (alpha_0^2 * (alpha_0 + 1)), nrow = 1)
  )

  expected_cov <- matrix(NA_real_, nrow = 3, ncol = 3)
  denom <- alpha_0^2 * (alpha_0 + 1)
  for (i in 1:3) {
    for (j in 1:3) {
      expected_cov[i, j] <- if (i == j) {
        alpha[i] * (alpha_0 - alpha[i]) / denom
      } else {
        -alpha[i] * alpha[j] / denom
      }
    }
  }
  expect_equal(covariance(dist), list(expected_cov))

  # pdf
  x <- c(0.2, 0.5, 0.3)
  expect_equal(
    density(dist, cbind(x[1], x[2], x[3])),
    LaplacesDemon::ddirichlet(x, alpha)
  )
  expect_equal(
    density(dist, cbind(x[1], x[2], x[3]), log = TRUE),
    LaplacesDemon::ddirichlet(x, alpha, log = TRUE)
  )

  # support
  s <- support(dist)
  expect_s3_class(s, "support_region")
  expect_equal(format(s), "[0,1]^3")

  # generate
  set.seed(42)
  samples <- generate(dist, 10)[[1]]
  expect_equal(nrow(samples), 10L)
  expect_equal(ncol(samples), 3L)
  # rows should sum to 1
  expect_equal(rowSums(samples), rep(1, 10), tolerance = 1e-10)
  # all values non-negative
  expect_true(all(samples >= 0))
})

test_that("Dirichlet distribution - symmetric case", {
  alpha <- c(1, 1, 1)
  dist <- dist_dirichlet(alpha = list(alpha))
  alpha_0 <- sum(alpha)

  # uniform Dirichlet: mean is (1/3, 1/3, 1/3)
  expect_equal(mean(dist), matrix(rep(1 / 3, 3), nrow = 1), tolerance = 1e-10)

  # variance should be equal across components
  vars <- as.numeric(variance(dist))
  expect_true(all(abs(vars - vars[1]) < 1e-10))
})

test_that("Dirichlet distribution - two-component reduces to Beta", {
  # Dir(a, b) marginal X1 has mean a/(a+b) and variance matching Beta(a,b)
  a <- 3
  b <- 4
  dist_dir <- dist_dirichlet(alpha = list(c(a, b)))
  alpha_0 <- a + b

  expect_equal(
    as.numeric(mean(dist_dir)),
    c(a / alpha_0, b / alpha_0)
  )

  expected_var_x1 <- a * b / (alpha_0^2 * (alpha_0 + 1))
  expect_equal(as.numeric(variance(dist_dir))[1], expected_var_x1)
})

test_that("Dirichlet distribution with multiple distributions", {
  alpha <- list(c(1, 2), c(2, 2), c(3, 1))
  dist <- dist_dirichlet(alpha = alpha)

  expect_equal(format(dist), rep("Dirichlet[2]", 3))
  expect_equal(mean(dist), do.call(rbind, lapply(alpha, function(a) a / sum(a))))
  expect_equal(
    variance(dist),
    do.call(rbind, lapply(alpha, function(a) a * (sum(a) - a) / (sum(a)^2 * (sum(a) + 1))))
  )
  expect_equal(length(covariance(dist)), 3L)
  expect_equal(covariance(dist)[[2]], covariance(dist[2])[[1]])

  at <- rbind(c(.3, .7), c(.5, .5), c(.9, .1))
  expect_equal(
    density(dist, list(at))[[1]],
    vapply(1:3, function(i) LaplacesDemon::ddirichlet(at[i, ], alpha[[i]]), numeric(1))
  )
  expect_equal(
    quantile(dist, 0.5)[3, ],
    qbeta(0.5, c(3, 1), c(1, 3))
  )

  draws <- generate(dist, list(c(1, 2, 3)))
  expect_equal(vapply(draws, nrow, integer(1)), 1:3)
})

test_that("Dirichlet distribution with different numbers of categories", {
  dist <- dist_dirichlet(alpha = list(c(1, 2), c(2, 3, 4)))

  expect_equal(format(dist), c("Dirichlet[2]", "Dirichlet[3]"))
  expect_equal(mean(dist), rbind(c(1/3, 2/3, NA), c(2, 3, 4) / 9))
  expect_equal(vapply(covariance(dist), nrow, integer(1)), c(2L, 3L))
  expect_equal(vapply(generate(dist, list(c(2, 3))), ncol, integer(1)), c(2L, 3L))
})
