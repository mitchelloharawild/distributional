#' Truncate a distribution
#'
#' \lifecycle{experimental}
#'
#' Note that the samples are generated using inverse transform sampling, and the
#' means and variances are estimated from samples.
#'
#' @param dist The distribution(s) to truncate.
#' @param lower,upper The range of values to keep from a distribution.
#'
#' @name dist_truncated
#' @export
dist_truncated <- function(dist, lower = -Inf, upper = Inf){
  vec_is(dist, new_dist())
  vec_is(lower, numeric())
  vec_is(upper, numeric())
  if(any(lower >= upper)){
    abort("The `lower` truncation bound must be lower than the `upper` bound.")
  }
  new_dist(dist = dist, lower = lower, upper = upper,
           dimnames = dimnames(dist), class = "dist_truncated")
}

#' @export
format.dist_truncated <- function(x, ...){
  sprintf(
    "%s[%i,%i]",
    format(x[["dist"]]),
    x[["lower"]],
    x[["upper"]]
  )
}

#' @export
density.dist_truncated <- function(x, at, ...){
  if(at < x[["lower"]] || at > x[["upper"]]) return(0)
  cdf_upr <- cdf(x[["dist"]], x[["upper"]])
  cdf_lwr <- cdf(x[["dist"]], x[["lower"]])
  density(x[["dist"]], at = at, ...)/(cdf_upr - cdf_lwr)
}

#' @export
quantile.dist_truncated <- function(x, p, ...){
  F_lwr <- cdf(x[["dist"]], x[["lower"]], ...)
  F_upr <- cdf(x[["dist"]], x[["upper"]], ...)
  qt <- quantile(x[["dist"]], F_lwr + p * ( F_upr - F_lwr), ...)
  min(max(x[["lower"]], qt), x[["upper"]])
}

#' @export
cdf.dist_truncated <- function(x, q, ...){
  if(q < x[["lower"]]) return(0)
  if(q > x[["upper"]]) return(1)
  cdf_upr <- cdf(x[["dist"]], x[["upper"]])
  cdf_lwr <- cdf(x[["dist"]], x[["lower"]])
  (cdf(x[["dist"]], q = q, ...) - cdf_lwr)/(cdf_upr - cdf_lwr)
}
