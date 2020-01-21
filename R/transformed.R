dist_transformed <- function(dist, transform, inverse){
  vec_is(dist, new_dist())
  stopifnot(is.function(transform))
  stopifnot(is.function(inverse))
  new_dist(dist = dist, transform = list(transform), inverse = list(inverse),
           class = "dist_transformed")
}

#' @export
format.dist_transformed <- function(x, ...){
  sprintf(
    "t(%s)",
    format(x[["dist"]])
  )
}

#' @export
density.dist_transformed <- function(x, at, ...){
  density(x[["dist"]], x[["inverse"]](at))*numDeriv::jacobian(x[["inverse"]], at)
}

#' @export
cdf.dist_transformed <- function(x, q, ...){
  cdf(x[["dist"]], x[["inverse"]](q), ...)
}

#' @export
quantile.dist_transformed <- function(x, p, ...){
  x[["transform"]](quantile(x[["dist"]], p, ...))
}

#' @export
generate.dist_transformed <- function(x, ...){
  x[["transform"]](generate(x[["dist"]], ...))
}

#' @export
mean.dist_transformed <- function(x, ...){
  mu <- mean(x[["dist"]])
  sigma2 <- variance(x[["dist"]])
  x[["transform"]](mu) + numDeriv::hessian(x[["transform"]], mu)/2*sigma2
}

#' @export
variance.dist_transformed <- function(x, ...){
  mu <- mean(x[["dist"]])
  sigma2 <- variance(x[["dist"]])
  numDeriv::jacobian(x[["transform"]], mu)^2*sigma2 + (numDeriv::hessian(x[["transform"]], mu)*sigma2)^2/2
}
