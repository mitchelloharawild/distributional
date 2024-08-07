#' Missing distribution
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' A placeholder distribution for handling missing values in a vector of
#' distributions.
#'
#' @param length The number of missing distributions
#'
#' @name dist_missing
#'
#' @examples
#' dist <- dist_missing(3L)
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
#' @export
dist_missing <- function(length = 1) {
  vctrs::vec_rep(NA_dist_, length)
}

NA_dist_ <- structure(list(NULL), class = c("distribution", "vctrs_vctr", "list"))

#' @export
format.dist_na <- function(x, ...) {
  "NA"
}

#' @export
density.dist_na <- function(x, at, ...){
  rep_len(NA_real_, length(at))
}

#' @export
log_density.dist_na <- density.dist_na

#' @export
quantile.dist_na <- function(x, p, ...){
  rep_len(NA_real_, length(p))
}
#' @export
log_quantile.dist_na <- quantile.dist_na

#' @export
cdf.dist_na <- function(x, q, ...){
  rep_len(NA_real_, length(q))
}
#' @export
log_cdf.dist_na <- cdf.dist_na

#' @export
generate.dist_na <- function(x, times, ...){
  rep(NA_real_, times)
}

#' @export
mean.dist_na <- function(x, ...) NA_real_

#' @export
covariance.dist_na <- function(x, ...) NA_real_

#' @export
skewness.dist_na <- function(x, ...) NA_real_

#' @export
kurtosis.dist_na <- function(x, ...) NA_real_

#' @export
Math.dist_na <- function(x, ...) {
  x
}

#' @export
Ops.dist_na <- function(e1, e2) {
  dist_missing(max(length(e1), length(e2)))
}
