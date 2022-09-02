#' The Exponential Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams stats::dexp
#'
#' @seealso [stats::Exponential]
#'
#' @examples
#' dist <- dist_exponential(rate = c(2, 1, 2/3))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_exponential
#' @export
dist_exponential <- function(rate){
  rate <- vec_cast(rate, double())
  if(any(rate < 0)){
    abort("The rate parameter of an Exponential distribution must be non-negative.")
  }
  new_dist(rate = rate, class = "dist_exponential")
}

#' @export
format.dist_exponential <- function(x, digits = 2, ...){
  sprintf(
    "Exp(%s)",
    format(x[["rate"]], digits = digits, ...)
  )
}

#' @export
density.dist_exponential <- function(x, at, ...){
  stats::dexp(at, x[["rate"]])
}

#' @export
log_density.dist_exponential <- function(x, at, ...){
  stats::dexp(at, x[["rate"]], log = TRUE)
}

#' @export
quantile.dist_exponential <- function(x, p, ...){
  stats::qexp(p, x[["rate"]])
}

#' @export
cdf.dist_exponential <- function(x, q, ...){
  stats::pexp(q, x[["rate"]])
}

#' @export
generate.dist_exponential <- function(x, times, ...){
  stats::rexp(times, x[["rate"]])
}

#' @export
mean.dist_exponential <- function(x, ...){
  1/x[["rate"]]
}

#' @export
covariance.dist_exponential <- function(x, ...){
  1/x[["rate"]]^2
}

#' @export
skewness.dist_exponential <- function(x, ...) 2

#' @export
kurtosis.dist_exponential <- function(x, ...) 6
