#' The Poisson-Inverse Gaussian distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams actuar::dpoisinvgauss
#'
#' @seealso actuar::dpoisinvgauss
#'
#' @examples
#' dist_poisson_inverse_gaussian(mean = c(1), shape = c(0.4))
#'
#' @name dist_poisson_inverse_gaussian
#' @export
dist_poisson_inverse_gaussian <- function(mean, shape){
  mean <- vec_cast(mean, double())
  shape <- vec_cast(shape, double())
  if(any(mean[!is.na(mean)] <= 0)){
    abort("The mean parameter of a Poisson-Inverse Gaussian distribution must be strictly positive.")
  }
  if(any(shape[!is.na(shape)] <= 0)){
    abort("The shape parameter of a Poisson-Inverse Gaussian distribution must be strictly positive.")
  }
  new_dist(m = mean, s = shape, class = "dist_poisson_inverse_gaussian")
}

#' @export
print.dist_poisson_inverse_gaussian <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_poisson_inverse_gaussian <- function(x, digits = 2, ...){
  sprintf(
    "PIG(%s, %s)",
    format(x[["m"]], digits = digits, ...),
    format(x[["s"]], digits = digits, ...)
  )
}

#' @export
density.dist_poisson_inverse_gaussian <- function(x, at, ...){
  require_package("actuar")
  actuar::dpoisinvgauss(at, x[["m"]], x[["s"]])
}

#' @export
quantile.dist_poisson_inverse_gaussian <- function(x, p, ...){
  require_package("actuar")
  actuar::qpoisinvgauss(p, x[["m"]], x[["s"]])
}

#' @export
cdf.dist_poisson_inverse_gaussian <- function(x, q, ...){
  require_package("actuar")
  actuar::ppoisinvgauss(q, x[["m"]], x[["s"]])
}

#' @export
generate.dist_poisson_inverse_gaussian <- function(x, times, ...){
  require_package("actuar")
  actuar::rpoisinvgauss(times, x[["m"]], x[["s"]])
}

#' @export
mean.dist_poisson_inverse_gaussian <- function(x, ...){
  x[["m"]]
}

#' @export
variance.dist_poisson_inverse_gaussian <- function(x, ...){
  x[["m"]]/x[["s"]] * (x[["m"]]^2 + x[["s"]])
}