#' @export
density.dist_default <- function(x, ...){
  abort(
    sprintf("The distribution class `%s` does not support `density()`",
            class(x)[1])
  )
}
#' @export
quantile.dist_default <- function(x, ...){
  abort(
    sprintf("The distribution class `%s` does not support `quantile()`",
            class(x)[1])
  )
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
