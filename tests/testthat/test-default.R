new_test_dist <- function(cls, methods) {
  ns <- asNamespace("distributional")
  for (generic in names(methods)) {
    registerS3method(generic, cls, methods[[generic]], envir = ns)
  }
  wrap_dist(list(structure(list(), class = c(cls, "dist_default"))))
}

test_that("quantile.dist_default() is vectorised over p", {
  x <- new_test_dist("dist_test_cdf_only", list(
    cdf = function(x, q, ...) stats::pexp(q)
  ))
  q <- suppressWarnings(quantile(x, c(0.1, 0.9))[[1]])
  expect_length(q, 2)
  expect_equal(q, stats::qexp(c(0.1, 0.9)), tolerance = 1e-3)
})

test_that("cdf.dist_default() works for univariate generate()", {
  x <- new_test_dist("dist_test_generate_only", list(
    generate = function(x, times, ...) stats::rexp(times)
  ))
  set.seed(1)
  expect_equal(cdf(x, c(1, 2))[[1]], stats::pexp(c(1, 2)), tolerance = 1e-2)
})
