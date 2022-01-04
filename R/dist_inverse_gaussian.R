#' The Inverse Gaussian distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams actuar::dinvgauss
#'
#' @seealso [actuar::InverseGaussian]
#'
#' @examples
#' dist <- dist_inverse_gaussian(mean = c(1,1,1,3,3), shape = c(0.2, 1, 3, 0.2, 1))
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
#' @name dist_inverse_gaussian
#' @export
dist_inverse_gaussian <- function(mean, shape){
  mean <- vec_cast(mean, double())
  shape <- vec_cast(shape, double())
  if(any(mean[!is.na(mean)] <= 0)){
    abort("The mean parameter of a Inverse Gaussian distribution must be strictly positive.")
  }
  if(any(shape[!is.na(shape)] <= 0)){
    abort("The shape parameter of a Inverse Gaussian distribution must be strictly positive.")
  }
  new_dist(m = mean, s = shape, class = "dist_inverse_gaussian")
}

#' @export
format.dist_inverse_gaussian <- function(x, digits = 2, ...){
  sprintf(
    "IG(%s, %s)",
    format(x[["m"]], digits = digits, ...),
    format(x[["s"]], digits = digits, ...)
  )
}

#' @export
density.dist_inverse_gaussian <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvgauss(at, x[["m"]], x[["s"]])
}

#' @export
log_density.dist_inverse_gaussian <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvgauss(at, x[["m"]], x[["s"]], log = TRUE)
}

#' @export
quantile.dist_inverse_gaussian <- function(x, p, ...){
  require_package("actuar")
  actuar::qinvgauss(p, x[["m"]], x[["s"]])
}

#' @export
cdf.dist_inverse_gaussian <- function(x, q, ...){
  require_package("actuar")
  actuar::pinvgauss(q, x[["m"]], x[["s"]])
}

#' @export
generate.dist_inverse_gaussian <- function(x, times, ...){
  require_package("actuar")
  actuar::rinvgauss(times, x[["m"]], x[["s"]])
}

#' @export
mean.dist_inverse_gaussian <- function(x, ...){
  x[["m"]]
}

#' @export
covariance.dist_inverse_gaussian <- function(x, ...){
  x[["m"]]^3/x[["s"]]
}
