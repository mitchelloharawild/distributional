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

# #' @export
# density.dist_percentile <- function(x, at, ...){
# }
#

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
  stats::approx(x[["percentile"]]/100, x[["x"]], xout=stats::runif(times,0,1))$y
}
