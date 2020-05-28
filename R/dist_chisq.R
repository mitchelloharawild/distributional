#' The (non-central) Chi-Squared Distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::dchisq
#'
#' @seealso [stats::Chisquare]
#'
#' @examples
#' dist_chisq(df = c(1,2,3,4,6,9))
#'
#' @name dist_chisq
#' @export
dist_chisq <- function(df, ncp = 0){
  df <- vec_cast(df, double())
  ncp <- vec_cast(ncp, double())
  if(any(df < 0)){
    abort("The degrees of freedom parameter of a Chi-Squared distribution must be non-negative.")
  }
  new_dist(df = df, ncp = ncp, class = "dist_chisq")
}

#' @export
print.dist_chisq <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_chisq <- function(x, digits = 2, ...){
  sprintf(
    if (is_utf8_output()) "\u1d6a\u00b2(%s)" else "x2(%s)",
    format(x[["df"]], digits = digits, ...)
  )
}

#' @export
density.dist_chisq <- function(x, at, ...){
  stats::dchisq(at, x[["df"]], x[["ncp"]])
}

#' @export
quantile.dist_chisq <- function(x, p, ...){
  stats::qchisq(p, x[["df"]], x[["ncp"]])
}

#' @export
cdf.dist_chisq <- function(x, q, ...){
  stats::pchisq(q, x[["df"]], x[["ncp"]])
}

#' @export
generate.dist_chisq <- function(x, times, ...){
  stats::rchisq(times, x[["df"]], x[["ncp"]])
}

#' @export
mean.dist_chisq <- function(x, ...){
  x[["df"]] + x[["ncp"]]
}

#' @export
variance.dist_chisq <- function(x, ...){
  2*(x[["df"]] + 2*x[["ncp"]])
}
