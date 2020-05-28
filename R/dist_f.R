#' The (non-central) Chi-Squared Distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::df
#'
#' @seealso [stats::FDist]
#'
#' @examples
#' dist_f(df1 = c(1,2,5,10,100), df2 = c(1,1,2,1,100))
#'
#' @name dist_f
#' @export
dist_f <- function(df1, df2, ncp = NULL){
  df1 <- vec_cast(df1, double())
  df2 <- vec_cast(df2, double())
  ncp <- vec_cast(ncp, double())
  if(any((df1 < 0) | (df2 < 0))){
    abort("The degrees of freedom parameters of the F distribution must be non-negative.")
  }
  if(is.null(ncp)){
    new_dist(df1 = df1, df2 = df2, class = "dist_f")
  } else {
    new_dist(df1 = df1, df2 = df2, ncp = ncp, class = "dist_f")
  }
}

#' @export
print.dist_f <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_f <- function(x, digits = 2, ...){
  sprintf(
    "F(%s, %s)",
    format(x[["df1"]], digits = digits, ...),
    format(x[["df2"]], digits = digits, ...)
  )
}

#' @export
density.dist_f <- function(x, at, ...){
  if(is.null(x[["ncp"]])) {
    stats::df(at, x[["df1"]], x[["df2"]])
  } else {
    stats::df(at, x[["df1"]], x[["df2"]], x[["ncp"]])
  }
}

#' @export
quantile.dist_f <- function(x, p, ...){
  if(is.null(x[["ncp"]])) {
    stats::qf(p, x[["df1"]], x[["df2"]])
  } else {
    stats::qf(p, x[["df1"]], x[["df2"]], x[["ncp"]])
  }
}

#' @export
cdf.dist_f <- function(x, q, ...){
  if(is.null(x[["ncp"]])) {
    stats::pf(q, x[["df1"]], x[["df2"]])
  } else {
    stats::pf(q, x[["df1"]], x[["df2"]], x[["ncp"]])
  }
}

#' @export
generate.dist_f <- function(x, times, ...){
  if(is.null(x[["ncp"]])) {
    stats::rf(times, x[["df1"]], x[["df2"]])
  } else {
    stats::rf(times, x[["df1"]], x[["df2"]], x[["ncp"]])
  }
}

#' @export
mean.dist_f <- function(x, ...){
  df1 <- x[["df1"]]
  df2 <- x[["df2"]]
  if(df2 > 2) {
    if(is.null(x[["ncp"]])){
      df2 / (df2 - 2)
    } else {
      (df2 * (df1 + x[["ncp"]])) / (df1 * (df2 - 2))
    }
  } else {
    NA_real_
  }
}

#' @export
variance.dist_f <- function(x, ...){
  df1 <- x[["df1"]]
  df2 <- x[["df2"]]
  if(df2 > 4) {
    if(is.null(x[["ncp"]])){
      (2 * df2^2 * (df1 + df2 - 2))/(df1*(df2-2)^2*(df2-4))
    } else {
      2*((df1 + x[["ncp"]])^2 + (df1 + 2*x[["ncp"]])*(df2 - 2))/((df2-2)^2*(df2-4)) * (df2^2/df1^2)
    }
  } else {
    NA_real_
  }
}
