#' The Beta distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param shape1,shape2 The non-negative shape parameters of the Beta distribution.
#'
#' @seealso [stats::Beta]
#'
#' @examples
#' dist <- dist_beta(shape1 = c(0.5, 5, 1, 2, 2), shape2 = c(0.5, 1, 3, 2, 5))
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
#' @name dist_beta
#' @export
dist_beta <- function(shape1, shape2){
  shape1 <- vec_cast(shape1, double())
  shape2 <- vec_cast(shape2, double())
  if(any((shape1 < 0) | shape2 < 0)){
    abort("Shape parameters of a Beta distribution must be non-negative.")
  }
  new_dist(shape1 = shape1, shape2 = shape2, class = "dist_beta")
}

#' @export
format.dist_beta <- function(x, digits = 2, ...){
  sprintf(
    "Beta(%s, %s)",
    format(x[["shape1"]], digits = digits, ...),
    format(x[["shape2"]], digits = digits, ...)
  )
}

#' @export
density.dist_beta <- function(x, at, ...){
  stats::dbeta(at, x[["shape1"]], x[["shape2"]])
}

#' @export
log_density.dist_beta <- function(x, at, ...){
  stats::dbeta(at, x[["shape1"]], x[["shape2"]], log = TRUE)
}

#' @export
quantile.dist_beta <- function(x, p, ...){
  stats::qbeta(p, x[["shape1"]], x[["shape2"]])
}

#' @export
cdf.dist_beta <- function(x, q, ...){
  stats::pbeta(q, x[["shape1"]], x[["shape2"]])
}

#' @export
generate.dist_beta <- function(x, times, ...){
  stats::rbeta(times, x[["shape1"]], x[["shape2"]])
}

#' @export
mean.dist_beta <- function(x, ...){
  x[["shape1"]]/(x[["shape1"]] + x[["shape2"]])
}

#' @export
covariance.dist_beta <- function(x, ...){
  a <- x[["shape1"]]
  b <- x[["shape2"]]
  a*b/((a+b)^2*(a+b+1))
}

#' @export
skewness.dist_beta <- function(x, ...) {
  a <- x[["shape1"]]
  b <- x[["shape2"]]
  2 * (b - a) * sqrt(a + b + 1) / (a + b + 2) * sqrt(a * b)
}

#' @export
kurtosis.dist_beta <- function(x, ...) {
  a <- x[["shape1"]]
  b <- x[["shape2"]]
  num <- 6 * ((a - b)^2 * (a + b + 1) - (a * b) * (a + b + 2))
  denom <- a * b * (a + b + 2) * (a + b + 3)
  num / denom
}
