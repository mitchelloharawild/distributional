#' The Weibull distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::dweibull
#'
#' @seealso [stats::Weibull]
#'
#' @examples
#' dist_weibull(shape = c(0.5, 1, 1.5, 5), scale = rep(1, 4))
#'
#' @name dist_weibull
#' @export
dist_weibull <- function(shape, scale){
  shape <- vec_cast(shape, double())
  scale <- vec_cast(scale, double())
  if(any(shape[!is.na(shape)] < 0)){
    abort("The shape parameter of a Weibull distribution must be non-negative.")
  }
  if(any(scale[!is.na(scale)] <= 0)){
    abort("The scale parameter of a Weibull distribution must be strictly positive.")
  }
  new_dist(shape = shape, scale = scale, class = "dist_weibull")
}

#' @export
print.dist_weibull <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_weibull <- function(x, digits = 2, ...){
  sprintf(
    "Weibull(%s, %s)",
    format(x[["shape"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...)
  )
}

#' @export
density.dist_weibull <- function(x, at, ...){
  stats::dweibull(at, x[["shape"]], x[["scale"]])
}

#' @export
quantile.dist_weibull <- function(x, p, ...){
  stats::qweibull(p, x[["shape"]], x[["scale"]])
}

#' @export
cdf.dist_weibull <- function(x, q, ...){
  stats::pweibull(q, x[["shape"]], x[["scale"]])
}

#' @export
generate.dist_weibull <- function(x, times, ...){
  stats::rweibull(times, x[["shape"]], x[["scale"]])
}

#' @export
mean.dist_weibull <- function(x, ...){
  x[["scale"]] * gamma(1 + 1/x[["shape"]])
}

#' @export
variance.dist_weibull <- function(x, ...){
  x[["scale"]]^2 * (gamma(1 + 2/x[["shape"]]) - gamma(1 + 1/x[["shape"]])^2)
}
