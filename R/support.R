#' Create a new support region vector
#'
#' @param x A list of prototype vectors defining the distribution type.
#' @param limits A list of value limits for the distribution.
#' @param interval The
#'
new_support_region <- function(x, limits = NULL, interval = NULL) {
  vctrs::new_vctr(x, class = "support_region")
}

#' @export
format.support_region <- function(x) {
  vapply(vec_data(x), function(z) {
    out <- if(is.integer(z)) "Z"
    else if(is.numeric(z)) "R"
    else if(is.complex(z)) "C"
    else vec_ptype_abbr(z)
    if(is.matrix(z)) {
      if(ncol(z) > 1) {
        out <- paste(out, ncol(z), sep = "^")
      }
    }
    out
  }, FUN.VALUE = character(1L))
}
