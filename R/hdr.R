#' Construct hdr intervals
#'
#' @param x A list of [`hilo()`] objects.
#'
#' @return A "hdr" vector
#'
#' @author Mitchell O'Hara-Wild
#'
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
