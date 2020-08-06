#' Missing distribution
#'
#' \lifecycle{experimental}
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
  new_dist(x = rep(NA, length), class = "dist_na")
}

# NA_dist_ <- dist_missing()

#' @export
format.dist_na <- function(x, ...){
  rep_len("NA", length(x[["x"]]))
}

#' @export
density.dist_na <- function(x, at, ...){
  NA_real_
}

#' @export
log_density.dist_na <- density.dist_na

#' @export
quantile.dist_na <- function(x, p, ...){
  NA_real_
}
#' @export
log_quantile.dist_na <- quantile.dist_na

#' @export
cdf.dist_na <- function(x, q, ...){
  NA_real_
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
variance.dist_na <- function(x, ...) NA_real_

#' @export
skewness.dist_na <- function(x, ...) NA_real_

#' @export
kurtosis.dist_na <- function(x, ...) NA_real_
