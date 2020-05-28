#' The Uniform distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::dunif
#'
#' @seealso [stats::Uniform]
#'
#' @examples
#' dist_uniform(min = c(3, -2), max = c(5, 4))
#'
#' @name dist_uniform
#' @export
dist_uniform <- function(min, max){
  min <- vec_cast(min, double())
  max <- vec_cast(max, double())
  if(any(min > max)){
    abort("The min of a Uniform distribution must be less than max.")
  }
  new_dist(l = min, u = max, class = "dist_uniform")
}

#' @export
print.dist_uniform <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_uniform <- function(x, digits = 2, ...){
  sprintf(
    "U(%s, %s)",
    format(x[["l"]], digits = digits, ...),
    format(x[["u"]], digits = digits, ...)
  )
}

#' @export
density.dist_uniform <- function(x, at, ...){
  stats::dunif(at, x[["l"]], x[["u"]])
}

#' @export
quantile.dist_uniform <- function(x, p, ...){
  stats::qunif(p, x[["l"]], x[["u"]])
}

#' @export
cdf.dist_uniform <- function(x, q, ...){
  stats::punif(q, x[["l"]], x[["u"]])
}

#' @export
generate.dist_uniform <- function(x, times, ...){
  stats::runif(times, x[["l"]], x[["u"]])
}

#' @export
mean.dist_uniform <- function(x, ...){
  (x[["u"]]+x[["l"]])/2
}

#' @export
variance.dist_uniform <- function(x, ...){
  (x[["u"]]-x[["l"]])^2/12
}
