#' The Logistic distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::dlogis
#'
#' @seealso [stats::Logistic]
#'
#' @examples
#' dist_logistic(location = c(5,9,9,6,2), scale = c(2,3,4,2,1))
#'
#' @name dist_logistic
#' @export
dist_logistic <- function(location, scale){
  location <- vec_cast(location, double())
  scale <- vec_cast(scale, double())
  new_dist(l = location, s = scale, class = "dist_logistic")
}

#' @export
print.dist_logistic <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_logistic <- function(x, digits = 2, ...){
  sprintf(
    "Logistic(%s, %s)",
    format(x[["l"]], digits = digits, ...),
    format(x[["s"]], digits = digits, ...)
  )
}

#' @export
density.dist_logistic <- function(x, at, ...){
  stats::dlogis(at, x[["l"]], x[["s"]])
}

#' @export
quantile.dist_logistic <- function(x, p, ...){
  stats::qlogis(p, x[["l"]], x[["s"]])
}

#' @export
cdf.dist_logistic <- function(x, q, ...){
  stats::plogis(q, x[["l"]], x[["s"]])
}

#' @export
generate.dist_logistic <- function(x, times, ...){
  stats::rlogis(times, x[["l"]], x[["s"]])
}

#' @export
mean.dist_logistic <- function(x, ...){
  x[["l"]]
}

#' @export
variance.dist_logistic <- function(x, ...){
  (x[["s"]]*pi)^2/3
}
