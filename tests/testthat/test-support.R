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
