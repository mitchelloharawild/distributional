#' @export
density.dist_default <- function(x, ...){
  abort(
    sprintf("The distribution class `%s` does not support `density()`",
            class(x)[1])
  )
}

#' @export
log_density.dist_default <- function(x, ...){
  log(density(x, ...))
}

#' @export
quantile.dist_default <- function(x, p, ...){
  # abort(
  #   sprintf("The distribution class `%s` does not support `quantile()`",
  #           class(x)[1])
  # )
  stats::optim(0, function(pos){
    (p - cdf(x, pos, ...))^2
  })$par
}
#' @export
log_quantile.dist_default <- function(x, p, ...){
  quantile(x, exp(p), ...)
}
#' @export
cdf.dist_default <- function(x, ...){
  abort(
    sprintf("The distribution class `%s` does not support `cdf()`",
            class(x)[1])
  )
}
#' @export
log_cdf.dist_default <- function(x, q, ...){
  log(cdf(x, q, ...))
}

#' @export
generate.dist_default <- function(x, times, ...){
  vapply(stats::runif(times,0,1), quantile, numeric(1L), x = x, ...)
}

#' @export
likelihood.dist_default <- function(x, sample, ...){
  prod(vapply(sample, density, numeric(1L), x = x))
}

#' @export
log_likelihood.dist_default <- function(x, sample, ...){
  sum(vapply(sample, log_density, numeric(1L), x = x))
}

#' @export
mean.dist_default <- function(x, ...){
  mean(generate(x, times = 1000), na.rm = TRUE)
}
#' @export
variance.dist_default <- function(x, ...){
  stats::var(generate(x, times = 1000), na.rm = TRUE)
}

#' @export
hilo.dist_default <- function(x, size = 95, ...){
  lower <- quantile(x, 0.5-size/200, ...)
  upper <- quantile(x, 0.5+size/200, ...)
  new_hilo(lower, upper, size)
}

#' @export
format.dist_default <- function(x, ...){
  rep_along("?", x)
}

#' @export
dim.dist_default <- function(x){
  1
}

invert_fail <- function(...) stop("Inverting transformations for distributions is not yet supported.")

#' @method Math dist_default
#' @export
Math.dist_default <- function(x, ...) {
  if(dim(x) > 1) stop("Transformations of multivariate distributions are not yet supported.")
  trans <- new_function(exprs(x = ), body = expr((!!sym(.Generic))(x, !!!dots_list(...))))
  dist_transformed(wrap_dist(list(x)), trans, invert_fail)[[1]]
}

#' @method Ops dist_default
#' @export
Ops.dist_default <- function(e1, e2) {
  is_dist <- c(inherits(e1, "dist_default"), inherits(e2, "dist_default"))
  if(any(vapply(list(e1, e2)[is_dist], dim, numeric(1L)) > 1)){
    stop("Transformations of multivariate distributions are not yet supported.")
  }

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

  dist_transformed(wrap_dist(list(e1,e2)[which(is_dist)]), trans, invert_fail)[[1]]
}
