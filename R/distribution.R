#' @export
new_dist <- function(..., class = NULL){
  args <- transpose(vctrs::vec_recycle_common(...))
  vctrs::new_vctr(
    lapply(args, structure, class = class),
    class = "distribution"
  )
}

#' @export
vec_ptype_abbr.distribution <- function(x, ...){
  "dist"
}

#' @export
format.distribution <- function(x, ...){
  do.call(vec_c, lapply(vec_data(x), format, ...))
}

#' @export
density.distribution <- function(x, at, ...){
  vec_is(at, double(), 1L)
  vapply(vec_data(x), density, double(1L), at = at, ...)
}

#' @export
quantile.distribution <- function(x, p, ...){
  vec_is(p, double(), 1L)
  vapply(vec_data(x), quantile, double(1L), p = p, ...)
}

#' @export
cdf <- function (x, q, ...){
  ellipsis::check_dots_used()
  UseMethod("cdf")
}
#' @export
cdf.distribution <- function(x, q, ...){
  vec_is(q, double(), 1L)
  vapply(vec_data(x), cdf, double(1L), q = q, ...)
}

#' @export
generate.distribution <- function(x, times, ...){
  times <- vec_cast(times, integer())
  lapply(vec_data(x), generate, times = times, ...)
}

#' @export
mean.distribution <- function(x, ...){
  vapply(vec_data(x), mean, double(1L), ...)
}

#' @export
hilo.distribution <- function(x, size = 0.95, ...){
  lower <- quantile(x, 0.5-size/2)
  upper <- quantile(x, 0.5+size/2)
  new_hilo(lower, upper, size)
}
