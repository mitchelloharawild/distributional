#' Percentile distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x A list of values
#' @param percentile A list of percentiles
#'
#' @examples
#' dist <- dist_normal()
#' percentiles <- seq(0.01, 0.99, by = 0.01)
#' x <- vapply(percentiles, quantile, double(1L), x = dist)
#' dist_percentile(list(x), list(percentiles*100))
#'
#' @export
dist_percentile <- function(x, percentile){
  x <- as_list_of(x, .ptype = double())
  percentile <- as_list_of(percentile, .ptype = double())
  new_dist(x = x, percentile = percentile, class = "dist_percentile")
}

#' @export
format.dist_percentile <- function(x, ...){
  sprintf(
    "percentile[%s]",
    length(x[["x"]])
  )
}

#' @export
density.dist_percentile <- function(x, at, ...){
  d <- density(generate(x, 1000), from = min(at), to = max(at), ..., na.rm=TRUE)
  stats::approx(d$x, d$y, xout = at)$y
}


#' @export
quantile.dist_percentile <- function(x, p, ...){
  out <- x[["x"]][match(p, x[["percentile"]])]
  out[is.na(out)] <- stats::approx(x = x[["percentile"]]/100, y = x[["x"]], xout = p[is.na(out)])$y
  out
}

#' @export
cdf.dist_percentile <- function(x, q, ...){
  stats::approx(x = x[["x"]], y = x[["percentile"]]/100, xout = q)$y
}

#' @export
generate.dist_percentile <- function(x, times, ...){
  stats::approx(x[["percentile"]], x[["x"]], xout=stats::runif(times,min(x[["percentile"]]),max(x[["percentile"]])))$y
}

#' @export
mean.dist_percentile <- function(x, ...) {
  # assumes percentile is sorted
  # probs <- x[["percentile"]]/100
  # i <- seq_along(probs)
  #
  # weights <- (probs[pmin(i+1, length(probs))] - probs[pmax(i-1, 1)]) / 2
  # sum(x[["x"]] * weights)


  # Fit a spline to the percentiles
  spline_fit <- stats::splinefun(x[["percentile"]], x[["x"]])

  # Use numerical integration to estimate the mean
  stats::integrate(spline_fit, lower = 0, upper = 1)$value
}

#' @export
support.dist_percentile <- function(x, ...) {
  new_support_region(
    list(vctrs::vec_init(x[["x"]], n = 0L)),
    list(range(x[["x"]])),
    list(!near(range(x[["percentile"]]), 0))
  )
}
