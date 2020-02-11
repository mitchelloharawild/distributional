#' Create a new distribution
#'
#' @param ... Parameters of the distribution (named)
#' @param class The class for S3 dispatch
#'
#' @export
new_dist <- function(..., class = NULL){
  args <- transpose(vctrs::vec_recycle_common(...))
  vctrs::new_vctr(
    lapply(args, structure, class = c(class, "dist_default")),
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

#' @importFrom stats density
#' @export
density.distribution <- function(x, at, ...){
  vec_is(at, double(), 1L)
  vapply(vec_data(x), density, double(1L), at = at, ...)
}

#' @importFrom stats quantile
#' @export
quantile.distribution <- function(x, p, ...){
  vec_is(p, double(), 1L)
  vapply(vec_data(x), quantile, double(1L), p = p, ...)
}

#' Cumulative distribution function
#'
#' @param x A distribution.
#' @param q The quantile at which the cdf is calculated.
#' @param ... Additional arguments used by methods.
#'
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

#' Distribution variance
#'
#' @param x A distribution
#' @param ... Additional arguments used by methods.
#'
#' @export
variance <- function(x, ...){
  UseMethod("variance")
}
#' @export
variance.distribution <- function(x, ...){
  vapply(vec_data(x), variance, double(1L), ...)
}

#' @export
hilo.distribution <- function(x, size = 0.95, ...){
  lower <- quantile(x, 0.5-size/2)
  upper <- quantile(x, 0.5+size/2)
  new_hilo(lower, upper, size)
}

#' @export
hdr.distribution <- function(x, size = 0.95, n = 512, ...){
  dist_x <- purrr::map_dbl(seq(0.5/n, 1 - 0.5/n, length.out = n), quantile, x = x)
  dist_y <- purrr::map_dbl(dist_x, density, x = x)
  alpha <- quantile(dist_y, probs = size)

  crossing_alpha <- function(alpha, x, y){
    it <- seq_len(length(y) - 1)
    dd <- y - alpha
    dd <- dd[it + 1] * dd[it]
    index <- it[dd <= 0]
    # unique() removes possible duplicates if sequential dd has same value.
    # More robust approach is required.
    unique(purrr::map_dbl(index, ~ approx(y[.x + c(0,1)], x[.x + c(0,1)], xout = alpha)$y))
  }

  # purrr::map(alpha, crossing_alpha, dist_x, dist_y)
  hdr <- crossing_alpha(alpha, dist_x, dist_y)
  lower_hdr <- seq_along(hdr)%%2==1
  hdr <- new_hilo(hdr[lower_hdr], hdr[!lower_hdr], size = size)
  new_hdr(list(hdr))
}

#' @export
vec_arith.distribution <- function(op, x, y, ...){
  if(is_empty(y)){
    out <- lapply(x, get(op))
  }
  else {
    x <- vec_recycle_common(x = x, y = y)
    y <- x[["y"]]
    x <- x[["x"]]
    out <- mapply(get(op), x = x, y = y, SIMPLIFY = FALSE)
  }
  vec_restore(out, x)
}

#' @export
vec_ptype2.distribution <- function(x, y, ...) UseMethod("vec_ptype2.distribution", y)
#' @export
vec_ptype2.distribution.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @export
vec_ptype2.distribution.distribution <- function(x, y, ...) new_dist()

#' @export
vec_cast.distribution <- function(x, to, ...) UseMethod("vec_cast.distribution")
#' @export
vec_cast.distribution.default <- function(x, to, ...) vec_default_cast(x, to)
#' @export
vec_cast.distribution.distribution <- function(x, to, ...) x
