#' The Geometric Distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::dgeom
#'
#' @seealso [stats::Geometric]
#'
#' @examples
#' dist_geometric(prob = c(0.2, 0.5, 0.8))
#'
#' @name dist_geometric
#' @export
dist_geometric <- function(prob){
  prob <- vec_cast(prob, double())
  if(any((prob < 0) | (prob > 1))){
    abort("The prob parameter of an Geometric distribution must be between 0 and 1.")
  }
  new_dist(p = prob, class = "dist_geometric")
}

#' @export
print.dist_geometric <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_geometric <- function(x, digits = 2, ...){
  sprintf(
    "Geometric(%s)",
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_geometric <- function(x, at, ...){
  stats::dgeom(at, x[["p"]])
}

#' @export
quantile.dist_geometric <- function(x, p, ...){
  stats::qgeom(p, x[["p"]])
}

#' @export
cdf.dist_geometric <- function(x, q, ...){
  stats::pgeom(q, x[["p"]])
}

#' @export
generate.dist_geometric <- function(x, times, ...){
  stats::rgeom(times, x[["p"]])
}

#' @export
mean.dist_geometric <- function(x, ...){
  1/x[["p"]] - 1
}

#' @export
variance.dist_geometric <- function(x, ...){
  (1 - x[["p"]])/x[["p"]]^2
}
