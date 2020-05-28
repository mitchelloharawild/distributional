#' Sampling distribution
#'
#' \lifecycle{stable}
#'
#' @param x A list of sampled values.
#'
#' @examples
#' dist_sample(x = list(rnorm(100), rnorm(100, 10)))
#'
#' @export
dist_sample <- function(x){
  has_NA <- FALSE
  x <- lapply(x, function(.) if(any(na_pos <- is.na(.))){
    has_NA<<-TRUE
    .[!na_pos]
  } else .)
  if(has_NA){
    warn("Missing sampled values have been removed from the sample distribution.")
  }
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
quantile.dist_sample <- function(x, p, ..., na.rm = TRUE){
  vapply(x, quantile, numeric(1L), probs = p, ..., na.rm = na.rm, USE.NAMES = FALSE)
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
  vapply(x, mean, numeric(1L), ..., USE.NAMES = FALSE)
}

#' @export
variance.dist_sample <- function(x, ...){
  vapply(x, stats::var, numeric(1L), ..., USE.NAMES = FALSE)
}
