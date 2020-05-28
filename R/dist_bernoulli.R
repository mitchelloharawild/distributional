#' The Bernoulli distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams dist_binomial
#'
#' @examples
#' dist_bernoulli(prob = c(0.05, 0.5, 0.3, 0.9, 0.1))
#'
#' @export
dist_bernoulli <- function(prob){
  prob <- vec_cast(prob, double())
  if(any((prob < 0) | (prob > 1))){
    abort("The probability of success must be between 0 and 1.")
  }
  new_dist(p = prob, class = "dist_bernoulli")
}

#' @export
print.dist_bernoulli <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_bernoulli <- function(x, digits = 2, ...){
  sprintf(
    "Bernoulli(%s)",
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_bernoulli <- function(x, at, ...){
  stats::dbinom(at, 1, x[["p"]])
}

#' @export
quantile.dist_bernoulli <- function(x, p, ...){
  stats::qbinom(p, 1, x[["p"]])
}

#' @export
cdf.dist_bernoulli <- function(x, q, ...){
  stats::pbinom(q, 1, x[["p"]])
}

#' @export
generate.dist_bernoulli <- function(x, times, ...){
  stats::rbinom(times, 1, x[["p"]])
}

#' @export
mean.dist_bernoulli <- function(x, ...){
  x[["p"]]
}

#' @export
variance.dist_bernoulli <- function(x, ...){
  x[["p"]]*(1-x[["p"]])
}
