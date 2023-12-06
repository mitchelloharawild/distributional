#' Construct hilo intervals
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Class constructor function to help with manually creating hilo interval
#' objects.
#'
#' @param lower,upper A numeric vector of values for lower and upper limits.
#' @param size Size of the interval between \[0, 100\].
#'
#' @return A "hilo" vector
#'
#' @author Earo Wang & Mitchell O'Hara-Wild
#'
#' @examples
#' new_hilo(lower = rnorm(10), upper = rnorm(10) + 5, size = 95)
#'
#' @export
new_hilo <- function(lower = double(), upper = double(), size = double()) {
  vec_assert(size, double())
  if (any(size < 0 | size > 100, na.rm = TRUE))
    abort("'size' must be between [0, 100].")

  out <- vec_recycle_common(lower = lower, upper = upper)
  if(vec_is(lower, double()) && vec_is(upper, double())) {
    if (any(out[["upper"]] < out[["lower"]], na.rm = TRUE)) {
      abort("`upper` can't be lower than `lower`.")
    }
  }
  len <- vec_size(out[[1]])
  out[["level"]] <- vctrs::vec_recycle(size, len)

  vctrs::new_rcrd(out, class = "hilo")
}

#' Compute intervals
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Used to extract a specified prediction interval at a particular confidence
#' level from a distribution.
#'
#' The numeric lower and upper bounds can be extracted from the interval using
#' `<hilo>$lower` and `<hilo>$upper` as shown in the examples below.
#'
#' @param x Object to create hilo from.
#' @param ... Additional arguments used by methods.
#'
#' @examples
#' # 95% interval from a standard normal distribution
#' interval <- hilo(dist_normal(0, 1), 95)
#' interval
#'
#' # Extract the individual quantities with `$lower`, `$upper`, and `$level`
#' interval$lower
#' interval$upper
#' interval$level
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
  lwr <- field(x, "lower")
  upr <- field(x, "upper")
  if(is.matrix(lwr)) {
    lwr <- if(ncol(lwr) > 1) vctrs::vec_ptype_abbr(lwr) else drop(lwr)
  }
  if(is.matrix(upr)) {
    upr <- if(ncol(upr) > 1) vctrs::vec_ptype_abbr(upr) else drop(upr)
  }
  limit <- paste(
    format(lwr, justify = justify, ...),
    format(upr, justify = justify, ...),
    sep = ", "
  )
  paste0("[", limit, "]", field(x, "level"))
}

#' @export
is.na.hilo <- function(x) {
  # both lower and upper are NA's
  x <- vec_data(x)
  is.na(x$lower) & is.na(x$upper)
}

#' @export
vec_ptype2.hilo.hilo <- function(x, y, ...){
  x
}

#' @export
vec_cast.character.hilo <- function(x, to, ...){
  sprintf(
    "[%s, %s]%s",
    as.character(x$lower), as.character(x$upper), as.character(x$level)
  )
}

#' @method vec_math hilo
#' @export
vec_math.hilo <- function(.fn, .x, ...){
  out <- vec_data(.x)
  if(.fn == "mean")
    abort("Cannot compute the mean of hilo intervals.")
  out[["lower"]] <- get(.fn)(out[["lower"]], ...)
  out[["upper"]] <- get(.fn)(out[["upper"]], ...)
  if(.fn %in% c("is.nan", "is.finite", "is.infinite"))
    return(out[["lower"]] | out[["upper"]])
  vec_restore(out, .x)
}

#' @method vec_arith hilo
#' @export
vec_arith.hilo <- function(op, x, y, ...){
  out <- dt_x <- vec_data(x)
  if(is_hilo(y)){
    abort("Intervals should not be added to other intervals, the sum of intervals is not the interval from a sum of distributions.")
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

#' @method vec_arith.numeric hilo
#' @export
vec_arith.numeric.hilo <- function(op, x, y, ...){
  out <- hl <- vec_data(y)
  out[["lower"]] <- get(op)(x, hl[["lower"]])
  out[["upper"]] <- get(op)(x, hl[["upper"]])
  if(x < 0 && op %in% c("*", "/")){
    out[c("lower", "upper")] <- out[c("upper", "lower")]
  }
  vec_restore(out, y)
}

#' @importFrom utils .DollarNames
#' @export
.DollarNames.hilo <- function(x, pattern){
  utils::.DollarNames(vec_data(x), pattern)
}

#' @export
`$.hilo` <- function(x, name){
  field(x, name)
}

#' @export
`names<-.hilo` <- function(x, value) {
  # abort("A <hilo> object cannot be named.")
  x
}
