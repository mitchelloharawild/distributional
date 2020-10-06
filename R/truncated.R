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
#'
#' @examples
#' dist <- dist_truncated(dist_normal(2,1), lower = 0)
#'
#' dist
#' mean(dist)
#' variance(dist)
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
#' if(requireNamespace("ggdist")) {
#' library(ggplot2)
#' ggplot() +
#'   ggdist::stat_dist_halfeye(
#'     aes(y = c("Normal", "Truncated"),
#'         dist = c(dist_normal(2,1), dist_truncated(dist_normal(2,1), lower = 0)))
#'   )
#' }
#'
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
    "%s[%s,%s]",
    format(x[["dist"]]),
    x[["lower"]],
    x[["upper"]]
  )
}

#' @export
density.dist_truncated <- function(x, at, ...){
  in_lim <- at >= x[["lower"]] & at <= x[["upper"]]
  cdf_upr <- cdf(x[["dist"]], x[["upper"]])
  cdf_lwr <- cdf(x[["dist"]], x[["lower"]])
  out <- numeric(length(at))
  out[in_lim] <- density(x[["dist"]], at = at[in_lim], ...)/(cdf_upr - cdf_lwr)
  out
}

#' @export
quantile.dist_truncated <- function(x, p, ...){
  F_lwr <- cdf(x[["dist"]], x[["lower"]])
  F_upr <- cdf(x[["dist"]], x[["upper"]])
  qt <- quantile(x[["dist"]], F_lwr + p * (F_upr - F_lwr), ...)
  pmin(pmax(x[["lower"]], qt), x[["upper"]])
}

#' @export
cdf.dist_truncated <- function(x, q, ...){
  cdf_upr <- cdf(x[["dist"]], x[["upper"]])
  cdf_lwr <- cdf(x[["dist"]], x[["lower"]])
  out <- numeric(length(q))
  q_lwr <- q < x[["lower"]] # out[q_lwr <- q < x[["lower"]]] <- 0
  out[q_upr <- q > x[["upper"]]] <- 1
  q_mid <- !(q_lwr|q_upr)
  out[q_mid] <- (cdf(x[["dist"]], q = q[q_mid], ...) - cdf_lwr)/(cdf_upr - cdf_lwr)
  out
}

#' @export
mean.dist_truncated <- function(x, ...) {
  if(inherits(x$dist, "dist_sample")) {
    y <- x$dist[[1]]
    mean(y[y >= x$lower & y <= x$upper])
  } else if(inherits(x$dist, "dist_normal")) {
    mu <- x$dist$mu
    s <- x$dist$sigma
    a <- (x$lower - mu) / s
    b <- (x$upper - mu) / s
    mu + (stats::dnorm(a) - stats::dnorm(b))/(stats::pnorm(b) - stats::pnorm(a))*s
  } else {
    NextMethod()
  }
}
