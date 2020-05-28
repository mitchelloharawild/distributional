#' The Gamma distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::dgamma
#'
#' @seealso [stats::GammaDist]
#'
#' @examples
#' dist_gamma(shape = c(1,2,3,5,9,7.5,0.5), rate = c(0.5,0.5,0.5,1,2,1,1))
#'
#' @name dist_gamma
#' @export
dist_gamma <- function(shape, rate){
  shape <- vec_cast(shape, double())
  rate <- vec_cast(rate, double())
  if(any(shape[!is.na(shape)] < 0)){
    abort("The shape parameter of a Gamma distribution must be non-negative.")
  }
  if(any(rate[!is.na(rate)] <= 0)){
    abort("The rate parameter of a Gamma distribution must be strictly positive.")
  }
  new_dist(shape = shape, rate = rate, class = "dist_gamma")
}

#' @export
print.dist_gamma <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_gamma <- function(x, digits = 2, ...){
  sprintf(
    if (is_utf8_output()) "\u0393(%s, %s)" else "Gamma(%s, %s)",
    format(x[["shape"]], digits = digits, ...),
    format(x[["rate"]], digits = digits, ...)
  )
}

#' @export
density.dist_gamma <- function(x, at, ...){
  stats::dgamma(at, x[["shape"]], x[["rate"]])
}

#' @export
quantile.dist_gamma <- function(x, p, ...){
  stats::qgamma(p, x[["shape"]], x[["rate"]])
}

#' @export
cdf.dist_gamma <- function(x, q, ...){
  stats::pgamma(q, x[["shape"]], x[["rate"]])
}

#' @export
generate.dist_gamma <- function(x, times, ...){
  stats::rgamma(times, x[["shape"]], x[["rate"]])
}

#' @export
mean.dist_gamma <- function(x, ...){
  x[["shape"]] / x[["rate"]]
}

#' @export
variance.dist_gamma <- function(x, ...){
  x[["shape"]] / x[["rate"]]^2
}
