#' Create a new support region vector
#'
#' @param x A list of prototype vectors defining the distribution type.
#' @param limits A list of value limits for the distribution.
#'
new_support_region <- function(x, limits = NULL) {
  vctrs::new_rcrd(list(x = x, lim = limits), class = "support_region")
}

#' @export
format.support_region <- function(x, ...) {
  type <- vapply(field(x, "x"), function(z) {
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
  mapply(function(type, z) {
    if(any(is.na(z)) || all(is.infinite(z))) type
    else if (type == "Z" && identical(z[2], Inf)) {
      if(z[1] == 0L) "N0" else if (z[2] == 1L) "N+" else paste0("[", z[1], ",", z[1]+1L, ",...,", z[2], "]")
    }
    else if (type == "R") paste0("[", z[1], ",", z[2], "]")
    else if (type == "Z") paste0("[", z[1], ",", z[1]+1L, ",...,", z[2], "]")
    else type
  }, type, field(x, "lim"))
}

#' @export
vec_ptype_abbr.support_region <- function(x, ...){
  "support"
}
