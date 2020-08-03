#' Modify a distribution with a transformation
#'
#' \lifecycle{experimental}
#'
#' The [`density()`], [`mean()`], and [`variance()`] methods are approximate as
#' they are based on numerical derivatives.
#'
#' @param dist A univariate distribution vector.
#' @param transform A function used to transform the distribution. This
#' transformation should be monotonic over appropriate domain.
#' @param inverse The inverse of the `transform` function.
#'
#' @examples
#' # Create a log normal distribution
#' dist <- dist_transformed(dist_normal(0, 0.5), exp, log)
#' density(dist, 1) # dlnorm(1, 0, 0.5)
#' cdf(dist, 4) # plnorm(4, 0, 0.5)
#' quantile(dist, 0.1) # qlnorm(0.1, 0, 0.5)
#' generate(dist, 10) # rlnorm(10, 0, 0.5)
#'
#' @export
dist_transformed <- function(dist, transform, inverse){
  vec_is(dist, new_dist())
  stopifnot(is.function(transform))
  stopifnot(is.function(inverse))
  new_dist(dist = vec_data(dist),
           transform = list(transform), inverse = list(inverse),
           dimnames = dimnames(dist), class = "dist_transformed")
}

#' @export
format.dist_transformed <- function(x, ...){
  sprintf(
    "t(%s)",
    format(x[["dist"]])
  )
}

#' @export
density.dist_transformed <- function(x, at, ...){
  drop(
    density(x[["dist"]], x[["inverse"]](at))*numDeriv::jacobian(x[["inverse"]], at)
  )
}

#' @export
cdf.dist_transformed <- function(x, q, ...){
  cdf(x[["dist"]], x[["inverse"]](q), ...)
}

#' @export
quantile.dist_transformed <- function(x, p, ...){
  x[["transform"]](quantile(x[["dist"]], p, ...))
}

#' @export
generate.dist_transformed <- function(x, ...){
  x[["transform"]](generate(x[["dist"]], ...))
}

#' @export
mean.dist_transformed <- function(x, ...){
  mu <- mean(x[["dist"]])
  sigma2 <- variance(x[["dist"]])
  if(is.na(sigma2)){
    # warning("Could not compute the transformed distribution's mean as the base distribution's variance is unknown. The transformed distribution's median has been returned instead.")
    return(x[["transform"]](mu))
  }
  drop(
    x[["transform"]](mu) + numDeriv::hessian(x[["transform"]], mu, method.args=list(d = 0.01))/2*sigma2
  )
}

#' @export
variance.dist_transformed <- function(x, ...){
  mu <- mean(x[["dist"]])
  sigma2 <- variance(x[["dist"]])
  if(is.na(sigma2)) return(NA_real_)
  drop(
    numDeriv::jacobian(x[["transform"]], mu)^2*sigma2 + (numDeriv::hessian(x[["transform"]], mu, method.args=list(d = 0.01))*sigma2)^2/2
  )
}

#' @method Math dist_transformed
#' @export
Math.dist_transformed <- function(x, ...) {
  trans <- new_function(exprs(x = ), body = expr((!!sym(.Generic))((!!x$transform)(x), !!!dots_list(...))))
  dist_transformed(wrap_dist(list(x[["dist"]])), trans, invert_fail)[[1]]
}

#' @method Ops dist_transformed
#' @export
Ops.dist_transformed <- function(e1, e2) {
  is_dist <- c(inherits(e1, "dist_default"), inherits(e2, "dist_default"))
  trans <- if(all(is_dist)) {
    if(identical(e1$dist, e2$dist)){
      new_function(exprs(x = ), expr((!!sym(.Generic))((!!e1$transform)(x), (!!e2$transform)(x))))
    } else {
      stop(sprintf("The %s operation is not supported for <%s> and <%s>", .Generic, class(e1)[1], class(e2)[1]))
    }
  } else if(is_dist[1]){
    new_function(exprs(x = ), body = expr((!!sym(.Generic))((!!e1$transform)(x), !!e2)))
  } else {
    new_function(exprs(x = ), body = expr((!!sym(.Generic))(!!e1, (!!e2$transform)(x))))
  }
  dist_transformed(wrap_dist(list(list(e1,e2)[[which(is_dist)[1]]][["dist"]])), trans, invert_fail)[[1]]
}
