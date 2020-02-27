#' @export
density.dist_default <- function(x, ...){
  abort(
    sprintf("The distribution class `%s` does not support `density()`",
            class(x)[1])
  )
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
cdf.dist_default <- function(x, ...){
  abort(
    sprintf("The distribution class `%s` does not support `cdf()`",
            class(x)[1])
  )
}
#' @export
generate.dist_default <- function(x, ...){
  abort(
    sprintf("The distribution class `%s` does not support `generate()`",
            class(x)[1])
  )
}
#' @export
mean.dist_default <- function(x, ...){
  abort(
    sprintf("The distribution class `%s` does not support `mean()`",
            class(x)[1])
  )
}
#' @export
variance.dist_default <- function(x, ...){
  abort(
    sprintf("The distribution class `%s` does not support `variance()`",
            class(x)[1])
  )
}

#' @export
format.dist_default <- function(x, ...){
  rep_along("?", x)
}

invert_fail <- function(...) stop("Inverting transformations for distributions is not yet supported")

#' @method Math dist_default
#' @export
Math.dist_default <- function(x, ...) {
  trans <- new_function(exprs(x = ), body = expr((!!sym(.Generic))(x, !!!dots_list(...))))
  dist_transformed(wrap_dist(list(x)), trans, invert_fail)
}

#' @method Ops dist_default
#' @export
Ops.dist_default <- function(e1, e2) {
  is_dist <- c(inherits(e1, "dist_default"), inherits(e2, "dist_default"))
  if(all(is_dist)) stop(sprintf("The %s operation is not supported for <%s> and <%s>", .Generic, class(e1)[1], class(e2)[1]))

  trans <- if(is_dist[1]){
    new_function(exprs(x = ), body = expr((!!sym(.Generic))(x, !!e2)))
  } else {
    new_function(exprs(x = ), body = expr((!!sym(.Generic))(!!e1, x)))
  }
  dist_transformed(wrap_dist(list(e1,e2)[which(is_dist)]), trans, invert_fail)
}
