#' The Gumbel distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams actuar::dgumbel
#'
#' @seealso [actuar::Gumbel]
#'
#' @examples
#' dist_gumbel(alpha = c(0.5, 1, 1.5, 3), scale = c(2, 2, 3, 4))
#'
#' @name dist_gumbel
#' @export
dist_gumbel <- function(alpha, scale){
  alpha <- vec_cast(alpha, double())
  scale <- vec_cast(scale, double())
  if(any(scale <= 0)){
    abort("The scale parameter of a Gumbel distribution must be strictly positive.")
  }
  new_dist(a = alpha, s = scale, class = "dist_gumbel")
}

#' @export
print.dist_gumbel <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_gumbel <- function(x, digits = 2, ...){
  sprintf(
    "Gumbel(%s, %s)",
    format(x[["a"]], digits = digits, ...),
    format(x[["s"]], digits = digits, ...)
  )
}

#' @export
density.dist_gumbel <- function(x, at, ...){
  require_package("actuar")
  actuar::dgumbel(at, x[["a"]], x[["s"]])
}

#' @export
quantile.dist_gumbel <- function(x, p, ...){
  require_package("actuar")
  actuar::qgumbel(p, x[["a"]], x[["s"]])
}

#' @export
cdf.dist_gumbel <- function(x, q, ...){
  require_package("actuar")
  actuar::pgumbel(q, x[["a"]], x[["s"]])
}

#' @export
generate.dist_gumbel <- function(x, times, ...){
  require_package("actuar")
  actuar::rgumbel(times, x[["a"]], x[["s"]])
}

#' @export
mean.dist_gumbel <- function(x, ...){
  actuar::mgumbel(1, x[["a"]], x[["s"]])
}

#' @export
variance.dist_gumbel <- function(x, ...){
  (pi*x[["s"]])^2/6
}
