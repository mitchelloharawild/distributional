#' The Hypergeometric distribution
#'
#' \lifecycle{stable}
#'
#' @param m The number of type I elements available.
#' @param n The number of type II elements available.
#' @param k The size of the sample taken.
#'
#' @seealso [stats::Hypergeometric]
#'
#' @examples
#' dist_hypergeometric(m = rep(500, 3), n = c(50, 60, 70), k = c(100, 200, 300))
#'
#' @name dist_hypergeometric
#' @export
dist_hypergeometric <- function(m, n, k){
  m <- vec_cast(m, integer())
  n <- vec_cast(n, integer())
  k <- vec_cast(k, integer())
  new_dist(m = m, n = n, k = k, class = "dist_hypergeometric")
}

#' @export
print.dist_hypergeometric <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_hypergeometric <- function(x, digits = 2, ...){
  sprintf(
    "Hypergeometric(%s, %s, %s)",
    format(x[["m"]], digits = digits, ...),
    format(x[["n"]], digits = digits, ...),
    format(x[["k"]], digits = digits, ...)
  )
}

#' @export
density.dist_hypergeometric <- function(x, at, ...){
  stats::dhyper(at, x[["m"]], x[["n"]], x[["k"]])
}

#' @export
quantile.dist_hypergeometric <- function(x, p, ...){
  stats::qhyper(p, x[["m"]], x[["n"]], x[["k"]])
}

#' @export
cdf.dist_hypergeometric <- function(x, q, ...){
  stats::phyper(q, x[["m"]], x[["n"]], x[["k"]])
}

#' @export
generate.dist_hypergeometric <- function(x, times, ...){
  stats::rhyper(times, x[["m"]], x[["n"]], x[["k"]])
}

#' @export
mean.dist_hypergeometric <- function(x, ...){
  p <- x[["m"]]/(x[["m"]] + x[["n"]])
  x[["k"]] * p
}

#' @export
variance.dist_hypergeometric <- function(x, ...){
  m <- x[["m"]]
  n <- x[["n"]]
  k <- x[["k"]]
  p <- m/(m + n)
  k * p * (1 - p) * ((m + n - k) / (m + n - 1))
}
