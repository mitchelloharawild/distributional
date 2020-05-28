#' The Burr distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams actuar::dburr
#'
#' @seealso [actuar::Burr]
#'
#' @examples
#' dist_burr(shape1 = c(1,1,1,2,3,0.5), shape2 = c(1,2,3,1,1,2))
#'
#' @name dist_burr
#' @export
dist_burr <- function(shape1, shape2, rate = 1){
  shape1 <- vec_cast(shape1, double())
  shape2 <- vec_cast(shape2, double())
  if(any(shape1 <= 0)){
    abort("The shape1 parameter of a Burr distribution must be strictly positive.")
  }
  if(any(shape2 <= 0)){
    abort("The shape2 parameter of a Burr distribution must be strictly positive.")
  }
  if(any(rate <= 0)){
    abort("The rate parameter of a Burr distribution must be strictly positive.")
  }
  new_dist(s1 = shape1, s2 = shape2, r = rate, class = "dist_burr")
}

#' @export
print.dist_burr <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_burr <- function(x, digits = 2, ...){
  sprintf(
    "Burr12(%s, %s, %s)",
    format(x[["s1"]], digits = digits, ...),
    format(x[["s2"]], x[["r"]], digits = digits, ...),
    format(x[["r"]], digits = digits, ...)
  )
}

#' @export
density.dist_burr <- function(x, at, ...){
  require_package("actuar")
  actuar::dburr(at, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
quantile.dist_burr <- function(x, p, ...){
  require_package("actuar")
  actuar::qburr(p, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
cdf.dist_burr <- function(x, q, ...){
  require_package("actuar")
  actuar::pburr(q, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
generate.dist_burr <- function(x, times, ...){
  require_package("actuar")
  actuar::rburr(times, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
mean.dist_burr <- function(x, ...){
  require_package("actuar")
  actuar::mburr(1, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
variance.dist_burr <- function(x, ...){
  require_package("actuar")
  m1 <- actuar::mburr(1, x[["s1"]], x[["s2"]], x[["r"]])
  m2 <- actuar::mburr(2, x[["s1"]], x[["s2"]], x[["r"]])
  -m1^2 + m2
}
