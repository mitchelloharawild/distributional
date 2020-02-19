#' Sampling distribution
#'
#' @param x A list of sampled values.
#'
#' @examples
#' dist_sample(x = list(rnorm(100), rnorm(100, 10)))
#'
#' @export
dist_sample <- function(x){
  x <- as_list_of(x, .ptype = double())
  new_dist(x = x, class = "dist_sample")
}

#' @export
print.dist_sample <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_sample <- function(x, ...){
  sprintf(
    "sample[%s]",
    vapply(x, length, integer(1L))
  )
}

# #' @export
# density.dist_sample <- function(x, at, ...){
# }
#

#' @export
quantile.dist_sample <- function(x, p, ...){
  vapply(x, quantile, numeric(1L), probs = p, ...)
}

# #' @export
# cdf.dist_sample <- function(x, q, ...){
# }
#
# #' @export
# generate.dist_sample <- function(x, times, ...){
# }

#' @export
mean.dist_sample <- function(x, ...){
  vapply(x, mean, numeric(1L), ...)
}

#' @export
variance.dist_sample <- function(x, ...){
  vapply(x, variance, numeric(1L), ...)
}
