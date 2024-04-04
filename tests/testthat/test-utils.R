test_that('eval_transform returns correct structure', {
  # matches the return types of density, https://github.com/mitchelloharawild/distributional/issues/99

  d <- exp(dist_wrap('norm'))
  dmult <- c(d, d, d, d, d)

  res <- eval_transform(d, 1)
  expect_type(res, 'double')
  expect_length(res, 1)

  res <- eval_transform(d, c(1, 2, 3))
  expect_type(res, "list")
  expect_length(res, 1)
  expect_length(res[[1]], 3)

  res <- eval_transform(dmult, 1)
  expect_type(res, 'double')
  expect_length(res, 5)

  res <- eval_transform(dmult, c(1, 2, 3))
  expect_type(res, "list")
  expect_length(res, 5)
  expect_length(res[[1]], 3)
})


test_that('eval_inverse returns correct structure', {
  # matches the return types of density, https://github.com/mitchelloharawild/distributional/issues/99

  d <- exp(dist_wrap('norm'))
  dmult <- c(d, d, d, d, d)

  res <- eval_inverse(d, 1)
  expect_type(res, 'double')
  expect_length(res, 1)

  res <- eval_inverse(d, c(1, 2, 3))
  expect_type(res, "list")
  expect_length(res, 1)
  expect_length(res[[1]], 3)

  res <- eval_inverse(dmult, 1)
  expect_type(res, 'double')
  expect_length(res, 5)

  res <- eval_inverse(dmult, c(1, 2, 3))
  expect_type(res, "list")
  expect_length(res, 5)
  expect_length(res[[1]], 3)
})


