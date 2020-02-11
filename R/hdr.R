#' Construct hdr intervals
#'
#' @param lower,upper A numeric vector of values for lower and upper limits.
#' @param size Size of the interval between \[0, 1\].
#'
#' @return A "hdr" vector
#'
#' @author Mitchell O'Hara-Wild
#'
#' @examples
#' new_hilo(lower = rnorm(10), upper = rnorm(10) + 5, size = 0.95)
#'
#' @export
new_hdr <- function(x = list()) {
  vec_assert(x, list())

  vctrs::new_list_of(x, ptype = new_hilo(), class = "hdr")
}

#' Compute highest density regions
#'
#' Used to extract a specified prediction interval at a particular confidence
#' level from a distribution.
#'
#' @param x Object to create hilo from.
#' @param ... Additional arguments used by methods.
#'
#' @export
hdr <- function(x, ...){
  UseMethod("hdr")
}

#' @export
hdr.default <- function(x, ...){
  abort(sprintf(
    "Objects of type `%s` are not supported by `hdr()`, you can create a custom `hdr` with `new_hdr()`",
    class(x)
  ))
}

#' Is the object a hdr
#'
#' @param x An object.
#'
#' @export
is_hdr <- function(x) {
  inherits(x, "hdr")
}

#' @export
format.hdr <- function(x, justify = "right", ...) {
  rep_along("hdr", x)
}
