#' Construct hilo intervals
#'
#' @param lower,upper A numeric vector of values for lower and upper limits.
#' @param size Size of the interval between [0, 1].
#'
#' @return A "hilo" vector
#'
#' @author Earo Wang & Mitchell O'Hara-Wild
#'
#' @examples
#' new_hilo(lower = rnorm(10), upper = rnorm(10) + 5, size = 0.95)
#'
#' @export
new_hilo <- function(lower, upper, size) {
  vec_assert(lower, double())
  vec_assert(upper, double())
  vec_assert(size, double())
  if (any(size < 0 | size > 1, na.rm = TRUE))
    abort("'size' must be between [0, 1].")

  out <- vec_recycle_common(lower = lower, upper = upper)
  if (any(out[["upper"]] < out[["lower"]], na.rm = TRUE)) {
    abort("`upper` can't be lower than `lower`.")
  }
  len <- length(out[[1]])
  out[["level"]] <- vctrs::vec_recycle(size, len)*100

  vctrs::new_rcrd(out, class = "hilo")
}

#' Compute intervals
#'
#' Used to extract a specified prediction interval at a particular confidence
#' level from a distribution.
#'
#' @param x Object to create hilo from
#'
#' @export
hilo <- function(x, ...){
  UseMethod("hilo")
}

#' @export
hilo.default <- function(x, ...){
  abort(sprintf(
    "Objects of type `%s` are not supported by `hilo()`, you can create a custom `hilo` with `new_hilo()`",
    class(x)
  ))
}

#' Is the object a hilo
#'
#' @param x An object.
#'
#' @export
is_hilo <- function(x) {
  inherits(x, "hilo")
}

#' @export
format.hilo <- function(x, justify = "right", ...) {
  x <- vec_data(x)
  limit <- paste(
    format(x$lower, justify = justify, ...),
    format(x$upper, justify = justify, ...),
    sep = ", "
  )
  paste0("[", limit, "]", x$level)
}

#' @export
is.na.hilo <- function(x) {
  # both lower and upper are NA's
  x <- vec_data(x)
  is.na(x$lower) & is.na(x$upper)
}

#' @export
vec_math.hilo <- function(.fn, .x, ...){
  out <- vec_data(.x)
  if(.fn == "mean")
    abort("Cannot compute the mean of hilo intervals.")
  out[["lower"]] <- get(.fn)(out[["lower"]], ...)
  out[["upper"]] <- get(.fn)(out[["upper"]], ...)
  if(.fn %in% c("is.nan", "is.finie", "is.infinite"))
    return(out[["lower"]] | out[["upper"]])
  vec_restore(out, .x)
}

#' @export
vec_arith.hilo <- function(op, x, y, ...){
  out <- dt_x <- vec_data(x)
  if(is_hilo(y)){
    dt_y <- vec_data(y)
    out[["lower"]] <- get(op)(dt_x[["lower"]], dt_y[["lower"]])
    out[["upper"]] <- get(op)(dt_x[["upper"]], dt_y[["upper"]])
  }
  else if(is_empty(y)){
    if(op == "-"){
      out[["upper"]] <- get(op)(dt_x[["lower"]])
      out[["lower"]] <- get(op)(dt_x[["upper"]])
    }
  }
  else{
    out[["lower"]] <- get(op)(dt_x[["lower"]], y)
    out[["upper"]] <- get(op)(dt_x[["upper"]], y)
  }
  vec_restore(out, x)
}
