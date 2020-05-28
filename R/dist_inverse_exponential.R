#' The Inverse Exponential distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams actuar::dinvexp
#'
#' @seealso [actuar::InverseExponential]
#'
#' @examples
#' dist_inverse_exponential(rate = 1:5)
#'
#' @name dist_inverse_exponential
#' @export
dist_inverse_exponential <- function(rate){
  rate <- vec_cast(rate, double())
  if(any(rate <= 0)){
    abort("The rate parameter of a Inverse Exponential distribution must be strictly positive.")
  }
  new_dist(r = rate, class = "dist_inverse_exponential")
}

#' @export
print.dist_inverse_exponential <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_inverse_exponential <- function(x, digits = 2, ...){
  sprintf(
    "InvExp(%s)",
    format(x[["r"]], digits = digits, ...)
  )
}

#' @export
density.dist_inverse_exponential <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvexp(at, x[["r"]])
}

#' @export
quantile.dist_inverse_exponential <- function(x, p, ...){
  require_package("actuar")
  actuar::qinvexp(p, x[["r"]])
}

#' @export
cdf.dist_inverse_exponential <- function(x, q, ...){
  require_package("actuar")
  actuar::pinvexp(q, x[["r"]])
}

#' @export
generate.dist_inverse_exponential <- function(x, times, ...){
  require_package("actuar")
  actuar::rinvexp(times, x[["r"]])
}

#' @export
mean.dist_inverse_exponential <- function(x, ...){
  NA_real_
}

#' @export
variance.dist_inverse_exponential <- function(x, ...){
  NA_real_
}
