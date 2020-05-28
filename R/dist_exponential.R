#' The Exponential Distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::dexp
#'
#' @seealso [stats::Exponential]
#'
#' @examples
#' dist_exponential(rate = c(2, 1, 2/3))
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
print.dist_exponential <- function(x, ...){
  cat(format(x, ...))
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
variance.dist_exponential <- function(x, ...){
  1/x[["rate"]]^2
}
