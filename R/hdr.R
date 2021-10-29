#' Construct hdr intervals
#'
#' @param lower,upper A list of numeric vectors specifying the region's lower
#' and upper bounds.
#' @param size A numeric vector specifying the coverage size of the region.
#'
#' @return A "hdr" vector
#'
#' @author Mitchell O'Hara-Wild
#'
#' @examples
#'
#' new_hdr(lower = list(1, c(3,6)), upper = list(10, c(5, 8)), size = c(80, 95))
#'
#' @export
new_hdr <- function(lower = list_of(.ptype = double()),
                    upper = list_of(.ptype = double()),
                    size = double()) {
  lower <- as_list_of(lower)
  upper <- as_list_of(upper)
  vec_assert(lower, list_of(.ptype = double()))
  vec_assert(upper, list_of(.ptype = double()))
  vec_assert(size, double())
  if (any(size < 0 | size > 100, na.rm = TRUE))
    abort("'size' must be between [0, 100].")


  out <- vec_recycle_common(lower = lower, upper = upper)
  mapply(
    function(l,u) if (any(u<l, na.rm = TRUE)) abort("`upper` can't be lower than `lower`."),
    l = out[["lower"]], u = out[["upper"]]
  )
  out[["level"]] <- vctrs::vec_recycle(size, vec_size(out[[1]]))

  vctrs::new_rcrd(out, class = "hdr")
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
  out <- mapply(function(l,u,s) {
    limit <- paste(
      format(l, justify = justify, ...),
      format(u, justify = justify, ...),
      sep = ", "
    )
    limit <- paste0("[", limit, "]", collapse = "")
    paste0(limit, s)
  }, l = field(x, "lower"), u = field(x, "upper"), s = field(x, "level"))
  as.vector(out, "character")
}
