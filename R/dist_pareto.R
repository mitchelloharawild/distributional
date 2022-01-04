#' The Pareto distribution
#'
#' \lifecycle{questioning}
#'
#' @inheritParams actuar::dpareto
#'
#' @seealso [actuar::Pareto]
#'
#' @examples
#' dist <- dist_pareto(shape = c(10, 3, 2, 1), scale = rep(1, 4))
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_pareto
#' @export
dist_pareto <- function(shape, scale){
  shape <- vec_cast(shape, double())
  scale <- vec_cast(scale, double())
  if(any(shape < 0)){
    abort("The shape parameter of a Pareto distribution must be non-negative.")
  }
  if(any(scale <= 0)){
    abort("The scale parameter of a Pareto distribution must be strictly positive.")
  }
  new_dist(shape = shape, scale = scale, class = "dist_pareto")
}

#' @export
format.dist_pareto <- function(x, digits = 2, ...){
  sprintf(
    "Pareto(%s, %s)",
    format(x[["shape"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...)
  )
}

#' @export
density.dist_pareto <- function(x, at, ...){
  require_package("actuar")
  actuar::dpareto(at, x[["shape"]], x[["scale"]])
}

#' @export
log_density.dist_pareto <- function(x, at, ...){
  require_package("actuar")
  actuar::dpareto(at, x[["shape"]], x[["scale"]], log = TRUE)
}

#' @export
quantile.dist_pareto <- function(x, p, ...){
  require_package("actuar")
  actuar::qpareto(p, x[["shape"]], x[["scale"]])
}

#' @export
cdf.dist_pareto <- function(x, q, ...){
  require_package("actuar")
  actuar::ppareto(q, x[["shape"]], x[["scale"]])
}

#' @export
generate.dist_pareto <- function(x, times, ...){
  require_package("actuar")
  actuar::rpareto(times, x[["shape"]], x[["scale"]])
}

#' @export
mean.dist_pareto <- function(x, ...){
  actuar::mpareto(1, x[["shape"]], x[["scale"]])
}

#' @export
covariance.dist_pareto <- function(x, ...){
  actuar::mpareto(2, x[["shape"]], x[["scale"]]) - actuar::mpareto(1, x[["shape"]], x[["scale"]])^2
}
