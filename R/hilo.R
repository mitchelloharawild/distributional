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

# Graphics ---------------------------------------------------------------------

#' @importFrom ggplot2 scale_type
#' @export
scale_type.hilo <- function(x){
  "continuous"
}

#' Hilo interval scales
#'
#' @inheritParams ggplot2::scale_y_continuous
#'
#' @export
scale_hilo_continuous <- function(name = waiver(), breaks = waiver(),
                         minor_breaks = waiver(), n.breaks = NULL,
                         labels = waiver(), limits = NULL,
                         expand = waiver(), oob = identity,
                         na.value = NA, trans = "identity",
                         guide = waiver(), position = "left",
                         sec.axis = waiver()) {

  sc <- ggplot2::scale_y_continuous(
    name = name, breaks = breaks, minor_breaks = minor_breaks, n.breaks = n.breaks,
    labels = labels, limits = limits, expand = expand, oob = oob,
    na.value = na.value, trans = trans, guide = guide, position = position,
    sec.axis = sec.axis
  )

  ggplot2::ggproto(
    NULL, sc,
    aesthetics = c("hilo"),
    map = function(self, x, limits = self$get_limits()) {
      scaled <- self$oob(x, limits)
      scaled[is.na(scaled)] <- self$na.value
      scaled
    },
    oob = function(x, range = c(0, 1), only.finite = TRUE){
      force(range)
      finite <- if (only.finite)
        is.finite(x)
      else TRUE
      dt <- vec_data(x)
      x[finite & dt$lower < range[1]] <- NA
      x[finite & dt$upper > range[2]] <- NA
      x
    },
    clone = function(self) {
      new <- ggplot2::ggproto(NULL, self)
      new$range <- hilo_range()
      new
    },
    range = hilo_range()
  )
}

RangeHilo <- ggplot2::ggproto("RangeHilo", NULL,
                              train = function(self, x) {
                                self$range <- scales::train_continuous(c(vec_data(x)$lower, vec_data(x)$upper), self$range)
                              },
                              reset = function(self) {
                                self$range <- NULL
                              }
)

hilo_range <- function() {
  ggplot2::ggproto(NULL, RangeHilo)
}
