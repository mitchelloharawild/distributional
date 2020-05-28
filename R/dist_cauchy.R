#' The Cauchy distribution
#'
#' \lifecycle{maturing}
#'
#' @inheritParams stats::dcauchy
#'
#' @seealso [stats::Cauchy]
#'
#' @examples
#' dist_cauchy(location = c(0, 0, 0, -2), scale = c(0.5, 1, 2, 1))
#'
#' @name dist_cauchy
#' @export
dist_cauchy <- function(location, scale){
  location <- vec_cast(location, double())
  scale <- vec_cast(scale, double())
  if(any(scale[!is.na(scale)] <= 0)){
    abort("The scale parameter of a Cauchy distribution must strictly positive.")
  }
  new_dist(location = location, scale = scale, class = "dist_cauchy")
}

#' @export
print.dist_cauchy <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_cauchy <- function(x, digits = 2, ...){
  sprintf(
    "Cauchy(%s, %s)",
    format(x[["location"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...)
  )
}

#' @export
density.dist_cauchy <- function(x, at, ...){
  stats::dcauchy(at, x[["location"]], x[["scale"]])
}

#' @export
quantile.dist_cauchy <- function(x, p, ...){
  stats::qcauchy(p, x[["location"]], x[["scale"]])
}

#' @export
cdf.dist_cauchy <- function(x, q, ...){
  stats::pcauchy(q, x[["location"]], x[["scale"]])
}

#' @export
generate.dist_cauchy <- function(x, times, ...){
  stats::rcauchy(times, x[["location"]], x[["scale"]])
}

#' @export
mean.dist_cauchy <- function(x, ...){
  NA_real_
}

#' @export
variance.dist_cauchy <- function(x, ...){
  NA_real_
}
