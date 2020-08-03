#' The Inverse Gamma distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams actuar::dinvgamma
#'
#' @seealso [actuar::InverseGamma]
#'
#' @examples
#' dist_inverse_gamma(shape = c(1,2,3,3), rate = c(1,1,1,2))
#'
#' @name dist_inverse_gamma
#' @export
dist_inverse_gamma <- function(shape, rate = 1/scale, scale){
  shape <- vec_cast(shape, double())
  rate <- vec_cast(rate, double())
  if(any(shape <= 0)){
    abort("The shape parameter of a Inverse Gamma distribution must be strictly positive.")
  }
  if(any(rate <= 0)){
    abort("The rate/scale parameter of a Inverse Gamma distribution must be strictly positive.")
  }
  new_dist(s = shape, r = rate, class = "dist_inverse_gamma")
}

#' @export
print.dist_inverse_gamma <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_inverse_gamma <- function(x, digits = 2, ...){
  sprintf(
    "InvGamma(%s, %s)",
    format(x[["s"]], digits = digits, ...),
    format(1/x[["r"]], digits = digits, ...)
  )
}

#' @export
density.dist_inverse_gamma <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvgamma(at, x[["s"]], x[["r"]])
}

#' @export
log_density.dist_inverse_gamma <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvgamma(at, x[["s"]], x[["r"]], log = TRUE)
}

#' @export
quantile.dist_inverse_gamma <- function(x, p, ...){
  require_package("actuar")
  actuar::qinvgamma(p, x[["s"]], x[["r"]])
}

#' @export
cdf.dist_inverse_gamma <- function(x, q, ...){
  require_package("actuar")
  actuar::pinvgamma(q, x[["s"]], x[["r"]])
}

#' @export
generate.dist_inverse_gamma <- function(x, times, ...){
  require_package("actuar")
  actuar::rinvgamma(times, x[["s"]], x[["r"]])
}

#' @export
mean.dist_inverse_gamma <- function(x, ...){
  if(x[["s"]] <= 1) return(NA_real_)
  1/(x[["r"]]*(x[["s"]]-1))
}

#' @export
variance.dist_inverse_gamma <- function(x, ...){
  if(x[["s"]] <= 2) return(NA_real_)
  1/(x[["r"]]^2*(x[["s"]]-1)^2*(x[["s"]]-2))
}
