test_that("Categorical distribution", {
  dist <- dist_categorical(list(c(0.4, 0.2, 0.3, 0.1)))

  expect_equal(format(dist), "Categorical[4]")

  # quantiles
  expect_true(all(is.na(quantile(dist, 0.5))))
  expect_true(all(is.na(quantile(dist, 0.2))))
  expect_equal(quantile(dist, c(0.1, 0.9)), list(c(NA_real_, NA_real_)))

  # pdf
  expect_equal(density(dist, -1), NA_real_)
  expect_equal(density(dist, 0), NA_real_)
  expect_equal(density(dist, 1), 0.4)
  expect_equal(density(dist, 2), 0.2)
  expect_equal(density(dist, 5), NA_real_)
  expect_equal(density(dist, 3:5), list(c(0.3, 0.1, NA_real_)))

  # cdf
  expect_true(all(is.na(cdf(dist, 1))))
  expect_equal(cdf(dist, 1:2), list(c(NA_real_, NA_real_)))

  # stats
  expect_true(all(is.na(mean(dist))))
  expect_true(all(is.na(variance(dist))))
})

test_that("Categorical distribution with several distributions", {
  dist <- dist_categorical(list(c(0.2, 0.8), c(0.5, 0.5), c(0.1, 0.3, 0.6)))

  expect_equal(
    format(dist),
    c("Categorical[2]", "Categorical[2]", "Categorical[3]")
  )

  # Each point is paired with its own distribution
  expect_equal(density(dist, 2), c(0.8, 0.5, 0.3))
  expect_equal(density(dist, 3), c(NA, NA, 0.6))
  expect_equal(density(dist, 1:3)[[3]], c(0.1, 0.3, 0.6))
  expect_equal(density(dist, 1:3)[[1]], c(0.2, 0.8, NA))
  expect_equal(density(dist, list(1, 3))[[1]], c(0.2, 0.5, 0.1))
  expect_equal(density(dist, list(1, 3))[[2]], c(NA, NA, 0.6))

  # Unordered
  expect_equal(unname(quantile(dist, 0.5)), rep(NA_real_, 3))
  expect_equal(mean(dist), rep(NA_real_, 3))
  expect_equal(variance(dist), rep(NA_real_, 3))

  # Generate
  set.seed(1)
  z <- generate(dist, 20)
  expect_length(z, 3)
  expect_true(all(z[[1]] %in% 1:2) && all(z[[3]] %in% 1:3))
  expect_equal(lengths(z), rep(20L, 3))
  expect_equal(lengths(generate(dist, list(1:3))), 1:3)
})

test_that("Categorical distribution with outcomes", {
  dist <- dist_categorical(
    list(c(0.2, 0.8), c(0.5, 0.3, 0.2)),
    list(c("a", "b"), c("x", "y", "z"))
  )
  expect_equal(density(dist, "b"), c(0.8, NA))
  expect_equal(density(dist, list("a", "y"))[[2]], c(NA, 0.3))
  expect_equal(density(dist, c("a", "z"))[[2]], c(NA, 0.2))
  set.seed(1)
  z <- generate(dist, 10)
  expect_true(all(z[[1]] %in% c("a", "b")) && all(z[[2]] %in% c("x", "y", "z")))

  # Ordered outcomes with different numbers of categories
  dist <- dist_categorical(
    list(c(0.2, 0.3, 0.5), c(0.5, 0.5)),
    list(factor(c("a", "b", "c"), ordered = TRUE), factor(c("a", "b"), ordered = TRUE))
  )
  expect_equal(cdf(dist, "b"), c(0.5, 1))
  expect_equal(cdf(dist, "b", tail = "upper"), c(0.5, 0))
  expect_equal(cdf(dist, "c"), c(1, NA))
  expect_equal(as.character(quantile(dist, 0.6)), c("c", "b"))
  expect_equal(as.character(quantile(dist, 0.2)), c("a", "a"))
  expect_equal(as.character(quantile(dist, 0.2, tail = "upper")), c("c", "b"))

  # A mix of ordered and unordered
  dist <- dist_categorical(
    list(c(0.2, 0.8), c(0.5, 0.5)),
    list(factor(c("a", "b"), ordered = TRUE), c("x", "y"))
  )
  expect_equal(cdf(dist, "a"), c(0.2, NA))
})
