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

#' @importFrom generics generate
#' @export
generate.distribution <- function(x, times, ...){
  times <- vec_cast(times, integer())
  lapply(vec_data(x), generate, times = times, ...)
}
