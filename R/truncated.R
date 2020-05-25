#' Truncate a distribution
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

#' @rdname dist_truncated
#' @export
density.dist_truncated <- function(x, at, ...){
  if(at < x[["lower"]] || at > x[["upper"]]) return(0)
  cdf_upr <- cdf(x[["dist"]], x[["upper"]])
  cdf_lwr <- cdf(x[["dist"]], x[["lower"]])
  density(x[["dist"]], at = at, ...)/(cdf_upr - cdf_lwr)
}

#' @rdname dist_truncated
#' @export
cdf.dist_truncated <- function(x, q, ...){
  cdf_upr <- cdf(x[["dist"]], x[["upper"]])
  cdf_lwr <- cdf(x[["dist"]], x[["lower"]])
  (cdf(x[["dist"]], q = q, ...) - cdf_lwr)/(cdf_upr - cdf_lwr)
}
