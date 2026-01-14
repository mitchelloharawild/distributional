#' Construct support regions
#'
#' @param x A list of prototype vectors defining the distribution type.
#' @param limits A list of value limits for the distribution.
#' @param closed A list of logical(2L) indicating whether the limits are closed.
#'
new_support_region <- function(x = numeric(), limits = list(), closed = list()) {
  vctrs::new_rcrd(list(x = x, lim = limits, closed = closed), class = "support_region")
}

#' @export
format.support_region <- function(x, digits = 3, ...) {
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
  brackets <- list(c("(","["), c(")","]"))
  mapply(function(type, z, closed) {
    br1 <- brackets[[1]][closed[1] + 1L]
    br2 <- brackets[[2]][closed[2] + 1L]
    fz <- sapply(z, function(x) format(x, digits = digits))
    fz <- gsub("3.14", "pi", fz, fixed = TRUE)
    if (any(is.na(z)) || all(is.infinite(z))) type
    else if (type == "Z") {
      if (identical(z, c(0L, Inf))) "N0"
      else if (identical(z, c(1L, Inf))) "N+"
      else paste0("{", z[1], ",", z[1]+1L, ",...,", z[2], "}")
    }
    else if (type == "R") paste0(br1, fz[1], ",", fz[2], br2)
    else type
  }, type, field(x, "lim"), field(x, "closed"))
}

#' @export
vec_ptype_abbr.support_region <- function(x, ...){
  "support"
}
