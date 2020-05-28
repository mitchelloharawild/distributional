#' The degenerate distribution
#'
#' \lifecycle{stable}
#'
#' @param x The value of the distribution.
#'
#' @examples
#' dist_degenerate(x = 1:5)
#'
#' @export
dist_degenerate <- function(x){
  vec_is(x, numeric())
  new_dist(x = x, class = "dist_degenerate")
}

#' @export
format.dist_degenerate <- function(x, ...){
  format(x[["x"]], ...)
}

#' @export
density.dist_degenerate <- function(x, at, ...){
  if(at == x[["x"]]) 1 else 0
}

#' @export
quantile.dist_degenerate <- function(x, p, ...){
  x[["x"]]
}

#' @export
cdf.dist_degenerate <- function(x, q, ...){
  if(q >= x[["x"]]) 1 else 0
}

#' @export
generate.dist_degenerate <- function(x, times, ...){
  rep(x[["x"]], times)
}

#' @export
mean.dist_degenerate <- function(x, ...){
  x[["x"]]
}

#' @export
variance.dist_degenerate <- function(x, ...){
  0
}
