#' The Studentized Range distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::qtukey
#'
#' @seealso [stats::Tukey]
#'
#' @examples
#' dist_studentized_range(nmeans = c(6, 2), df = c(5, 4), nranges = c(1, 1))
#'
#' @name dist_studentized_range
#' @export
dist_studentized_range <- function(nmeans, df, nranges){
  nmeans <- vec_cast(nmeans, double())
  df <- vec_cast(df, double())
  new_dist(nm = nmeans, df = df, nr = nranges, class = "dist_studentized_range")
}

#' @export
print.dist_studentized_range <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_studentized_range <- function(x, digits = 2, ...){
  sprintf(
    "StudentizedRange(%s, %s, %s)",
    format(x[["nm"]], digits = digits, ...),
    format(x[["df"]], digits = digits, ...),
    format(x[["nr"]], digits = digits, ...)
  )
}

#' @export
quantile.dist_studentized_range <- function(x, p, ...){
  stats::qtukey(p, x[["nm"]], x[["df"]], x[["nr"]])
}

#' @export
cdf.dist_studentized_range <- function(x, q, ...){
  stats::ptukey(q, x[["nm"]], x[["df"]], x[["nr"]])
}
