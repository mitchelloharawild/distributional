#' The Poisson Distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::dpois
#'
#' @seealso [stats::Poisson]
#'
#' @examples
#' dist_poisson(lambda = c(1, 4, 10))
#'
#' @name dist_poisson
#' @export
dist_poisson <- function(lambda){
  lambda <- vec_cast(lambda, double())
  if(any(lambda < 0)){
    abort("The lambda parameter of an Poisson distribution must be non-negative.")
  }
  new_dist(l = lambda, class = "dist_poisson")
}

#' @export
print.dist_poisson <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_poisson <- function(x, digits = 2, ...){
  sprintf(
    "Pois(%s)",
    format(x[["l"]], digits = digits, ...)
  )
}

#' @export
density.dist_poisson <- function(x, at, ...){
  stats::dpois(at, x[["l"]])
}

#' @export
quantile.dist_poisson <- function(x, p, ...){
  stats::qpois(p, x[["l"]])
}

#' @export
cdf.dist_poisson <- function(x, q, ...){
  stats::ppois(q, x[["l"]])
}

#' @export
generate.dist_poisson <- function(x, times, ...){
  stats::rpois(times, x[["l"]])
}

#' @export
mean.dist_poisson <- function(x, ...){
  x[["l"]]
}

#' @export
variance.dist_poisson <- function(x, ...){
  x[["l"]]
}
