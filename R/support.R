#' Construct support regions
#'
#' @param x A list of prototype vectors defining the distribution type.
#' @param limits A list of value limits for the distribution.
#' @param closed A list of logical(2L) indicating whether the limits are closed.
#'
new_support_region <- function(x = list(), limits = list(), closed = list()) {
  vctrs::new_rcrd(list(x = x, lim = limits, closed = closed), class = "support_region")
}

#' @export
format.support_region <- function(x, digits = 3, ...) {
  # Missing regions (of missing distributions) are formatted as NA
  out <- rep_len(NA_character_, vec_size(x))
  is_missing <- vec_detect_missing(x)
  if (all(is_missing)) return(out)
  x <- vec_slice(x, !is_missing)
  type <- vapply(field(x, "x"), function(z) {
    if(is.integer(z)) "Z"
    else if(is.numeric(z)) "R"
    else if(is.complex(z)) "C"
    else vec_ptype_abbr(z)
  }, FUN.VALUE = character(1L))
  dim_k <- vapply(field(x, "x"), function(z) {
    if(is.matrix(z)) ncol(z) else 1L
  }, FUN.VALUE = integer(1L))
  brackets <- list(c("(","["), c(")","]"))
  formatted <- mapply(function(type, k, z, closed) {
    br1 <- brackets[[1]][closed[1] + 1L]
    br2 <- brackets[[2]][closed[2] + 1L]
    fz <- sapply(z, function(x) format(x, digits = digits))
    fz <- gsub("3.14", "pi", fz, fixed = TRUE)
    suffix <- if(k > 1L) paste0("^", k) else ""
    if (any(is.na(z)) || all(is.infinite(z))) paste0(type, suffix)
    else if (type == "Z") {
      if (identical(z, c(0L, Inf))) "N0"
      else if (identical(z, c(1L, Inf))) "N+"
      else if (is.infinite(z[1])) paste0("{...,", z[2]-1L, ",", z[2], "}")
      else paste0("{", z[1], ",", z[1]+1L, ",...,", z[2], "}")
    }
    else if (type == "R") paste0(br1, fz[1], ",", fz[2], br2, suffix)
    else paste0(type, suffix)
  }, type, dim_k, field(x, "lim"), field(x, "closed"), USE.NAMES = FALSE)
  out[!is_missing] <- formatted
  out
}

#' @export
vec_ptype_abbr.support_region <- function(x, ...){
  "support"
}
