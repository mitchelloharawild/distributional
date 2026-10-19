test_that("support gives the correct bounds", {
  s <- support(c(dist_normal(),
               dist_gamma(1, 1),
               dist_gamma(2, 1),
               dist_lognormal(),
               dist_beta(1, 1),
               dist_beta(1, 2),
               dist_beta(2, 1),
               dist_beta(2, 2),
               exp(dist_wrap('norm')),
               2*atan(dist_normal())))
  out <- unname(format(s))
  expect_equal(out, c("R","[0,Inf)","(0,Inf)","(0,Inf)","[0,1]","[0,1)","(0,1]","(0,1)","(0,Inf)","(-pi,pi)"))
})

test_that("parametric families have closed-form support", {
  s <- support(c(
    dist_bernoulli(0.3),
    dist_beta(0.5, 2),
    dist_binomial(10, 0.3),
    dist_burr(2, 3),
    dist_cauchy(0, 1),
    dist_chisq(2),
    dist_chisq(5, ncp = 1),
    dist_degenerate(1.5),
    dist_exponential(1),
    dist_f(5, 5),
    dist_gamma(0.5, 1),
    dist_geometric(0.3),
    dist_gev(0, 1, 0),
    dist_gev(0, 1, 0.5),
    dist_gev(0, 1, -0.5),
    dist_gh(0, 1, 0, 0.5),
    dist_gk(0, 1, 0, 0.5),
    dist_gumbel(0, 1),
    dist_hypergeometric(5, 3, 4),
    dist_inverse_exponential(1),
    dist_inverse_gamma(2, 1),
    dist_inverse_gaussian(1, 1),
    dist_laplace(0, 1),
    dist_logarithmic(0.5),
    dist_logistic(0, 1),
    dist_lognormal(0, 1),
    dist_negative_binomial(3, 0.4),
    dist_normal(0, 1),
    dist_pareto(2, 1),
    dist_poisson(3),
    dist_poisson_inverse_gaussian(2, 1),
    dist_student_t(3),
    dist_studentized_range(5, 10, 1),
    dist_tweedie(1, 1, 1.5),
    dist_uniform(-3, 5),
    dist_weibull(2, 1)
  ))
  expect_equal(
    unname(format(s)),
    c("lgl", "[0,1)", "{0,1,...,10}", "(0,Inf)", "R", "[0,Inf)", "(0,Inf)",
      "[1.5,1.5]", "[0,Inf)", "(0,Inf)", "[0,Inf)", "N0", "R", "(-2,Inf)",
      "(-Inf,2)", "R", "R", "R", "{1,2,...,4}", "(0,Inf)", "(0,Inf)",
      "(0,Inf)", "R", "N+", "R", "(0,Inf)", "N0", "R", "[0,Inf)", "N0", "N0",
      "R", "(0,Inf)", "[0,Inf)", "[-3,5]", "(0,Inf)")
  )

  # Multivariate families
  expect_equal(
    unname(format(support(c(
      dist_multivariate_normal(list(c(0, 0)), list(diag(2))),
      dist_multivariate_t(3, list(c(0, 0, 0)), list(diag(3)))
    )))),
    c("R^2", "R^3")
  )
  mn <- field(support(dist_multinomial(4, list(c(0.2, 0.3, 0.5)))), "x")[[1]]
  expect_true(is.integer(mn) && is.matrix(mn) && ncol(mn) == 3L)
})

test_that("support prototypes of discrete families are integer", {
  discrete <- c(
    dist_binomial(10, 0.3), dist_geometric(1e-12), dist_hypergeometric(5, 3, 4),
    dist_logarithmic(0.5), dist_negative_binomial(3, 0.4), dist_poisson(3),
    dist_poisson_inverse_gaussian(2, 1)
  )
  for (proto in field(support(discrete), "x")) {
    expect_true(is.integer(proto))
  }
  expect_true(is.logical(field(support(dist_bernoulli(0.3)), "x")[[1]]))
  expect_true(is.double(field(support(dist_tweedie(1, 1, 1.5)), "x")[[1]]))
})

test_that("support of degenerate parameterisations", {
  s <- support(c(
    dist_binomial(10, 0), dist_binomial(10, 1), dist_poisson(0),
    dist_geometric(1), dist_negative_binomial(3, 1), dist_normal(2, 0),
    dist_uniform(2, 2), dist_gev(1, 2, -1), dist_f(1, 5)
  ))
  expect_equal(
    unname(format(s)),
    c("{0,1,...,0}", "{10,11,...,10}", "{0,1,...,0}", "{0,1,...,0}",
      "{0,1,...,0}", "[2,2]", "[2,2]", "(-Inf,3]", "[0,Inf)")
  )
})
