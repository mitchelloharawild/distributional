#' The Poisson-Inverse Gaussian distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams actuar::dpoisinvgauss
#'
#' @seealso [actuar::PoissonInverseGaussian]
#'
#' @examples
#' dist <- dist_poisson_inverse_gaussian(mean = rep(0.1, 3), shape = c(0.4, 0.8, 1))
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
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
log_density.dist_poisson_inverse_gaussian <- function(x, at, ...){
  require_package("actuar")
  actuar::dpoisinvgauss(at, x[["m"]], x[["s"]], log = TRUE)
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
covariance.dist_poisson_inverse_gaussian <- function(x, ...){
  x[["m"]]/x[["s"]] * (x[["m"]]^2 + x[["s"]])
}
