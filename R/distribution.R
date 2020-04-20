#' Create a new distribution
#'
#' @param ... Parameters of the distribution (named)
#' @param class The class for S3 dispatch
#'
#' @export
new_dist <- function(..., class = NULL){
  args <- transpose(vctrs::vec_recycle_common(...))
  wrap_dist(
    lapply(args, structure, class = c(class, "dist_default"))
  )
}

wrap_dist <- function(x){
  vctrs::new_vctr(x, class = "distribution")
}

#' @export
vec_ptype_abbr.distribution <- function(x, ...){
  "dist"
}

#' @export
format.distribution <- function(x, ...){
  x <- vec_data(x)
  out <- vapply(x, format, character(1L), ...)
  out[vapply(x, is.null, logical(1L))] <- "?"
  out
}

#' @importFrom stats density
#' @export
density.distribution <- function(x, at, ...){
  vec_is(at, double(), 1L)
  dist_apply(x, density, at = at, ...)
}

#' @importFrom stats quantile
#' @export
quantile.distribution <- function(x, p, ...){
  vec_is(p, double(), 1L)
  dist_apply(x, quantile, p = p, ...)
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
  dist_apply(x, cdf, q = q, ...)
}

#' @export
generate.distribution <- function(x, times, ...){
  times <- vec_cast(times, integer())
  times <- vec_recycle(times, size = length(x))
  mapply(generate, vec_data(x), times = times, ..., SIMPLIFY = FALSE)
  # dist_apply(x, generate, times = times, ...)
  # Needs work to structure MV appropriately.
}

#' @export
mean.distribution <- function(x, ...){
  dist_apply(x, mean, ...)
  # x <- vec_data(x)
  # out <- do.call("rbind", lapply(x, mean, ...))
  # if(ncol(out) == 1)
  #   drop(out)
  # else {
  #   colnames(out) <- vctrs::vec_as_names(character(ncol(out)), repair = "unique", quiet = TRUE)
  #   as.data.frame(out)
  # }
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
  dist_apply(x, variance, ...)
}

#' @importFrom stats median
#' @export
median.distribution <- function(x, na.rm = FALSE, ...){
  quantile(x, p = 0.5, na.rm = na.rm, ...)
}

#' @export
hilo.distribution <- function(x, size = 95, ...){
  dist_apply(x, hilo, size = size, ...)
}

#' @export
hdr.distribution <- function(x, size = 95, n = 512, ...){
  dist_x <- vapply(seq(0.5/n, 1 - 0.5/n, length.out = n), quantile, numeric(1L), x = x)
  dist_y <- vapply(dist_x, density, numeric(1L), x = x)
  alpha <- quantile(dist_y, probs = size/100)

  crossing_alpha <- function(alpha, x, y){
    it <- seq_len(length(y) - 1)
    dd <- y - alpha
    dd <- dd[it + 1] * dd[it]
    index <- it[dd <= 0]
    # unique() removes possible duplicates if sequential dd has same value.
    # More robust approach is required.
    unique(vapply(index, function(.x) stats::approx(y[.x + c(0,1)], x[.x + c(0,1)], numeric(1L), xout = alpha)$y))
  }

  # purrr::map(alpha, crossing_alpha, dist_x, dist_y)
  hdr <- crossing_alpha(alpha, dist_x, dist_y)
  lower_hdr <- seq_along(hdr)%%2==1
  hdr <- new_hilo(hdr[lower_hdr], hdr[!lower_hdr], size = size)
  new_hdr(list(hdr))
}

#' @method vec_arith distribution
#' @export
vec_arith.distribution <- function(op, x, y, ...){
  UseMethod("vec_arith.distribution", y)
}
#' @method vec_arith.distribution default
#' @export
vec_arith.distribution.default <- function(op, x, y, ...){
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

#' @method vec_arith.numeric distribution
#' @export
vec_arith.numeric.distribution <- function(op, x, y, ...){
  x <- vec_recycle_common(x = x, y = y)
  y <- x[["y"]]
  x <- x[["x"]]
  out <- mapply(get(op), x = x, y = y, SIMPLIFY = FALSE)
  vec_restore(out, y)
}

#' @method vec_math distribution
#' @export
vec_math.distribution <- function(.fn, .x, ...) {
  if(.fn %in% c("is.nan", "is.infinite")) return(rep_len(FALSE, length(.x)))
  if(.fn == "is.finite") return(rep_len(TRUE, length(.x)))
  out <- lapply(.x, get(.fn), ...)
  vec_restore(out, .x)
}

#' @method vec_ptype2 distribution
#' @export
vec_ptype2.distribution <- function(x, y, ...) UseMethod("vec_ptype2.distribution", y)
#' @method vec_ptype2.distribution default
#' @export
vec_ptype2.distribution.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.distribution distribution
#' @export
vec_ptype2.distribution.distribution <- function(x, y, ...) new_dist()
#' @method vec_ptype2.double distribution
#' @export
vec_ptype2.double.distribution <- function(x, y, ...) new_dist()
#' @method vec_ptype2.distribution double
#' @export
vec_ptype2.distribution.double <- function(x, y, ...) new_dist()
#' @method vec_ptype2.integer distribution
#' @export
vec_ptype2.integer.distribution <- function(x, y, ...) new_dist()
#' @method vec_ptype2.distribution integer
#' @export
vec_ptype2.distribution.integer <- function(x, y, ...) new_dist()

#' @method vec_cast distribution
#' @export
vec_cast.distribution <- function(x, to, ...) UseMethod("vec_cast.distribution")
#' @method vec_cast.distribution default
#' @export
vec_cast.distribution.default <- function(x, to, ...) vec_default_cast(x, to)
#' @method vec_cast.distribution distribution
#' @export
vec_cast.distribution.distribution <- function(x, to, ...) x
#' @method vec_cast.distribution double
#' @export
vec_cast.distribution.double <- function(x, to, ...) dist_degenerate(x)
#' @method vec_cast.distribution integer
#' @export
vec_cast.distribution.integer <- vec_cast.distribution.double
