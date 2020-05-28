#' The (non-central) Student t Distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::dt
#'
#' @seealso [stats::TDist]
#'
#' @examples
#' dist_student_t(df = c(1,2,5))
#'
#' @name dist_student_t
#' @export
dist_student_t <- function(df, ncp = NULL){
  df <- vec_cast(df, if(any(is.infinite(df))) numeric() else integer())
  if(any(df <= 0)){
    abort("The degrees of freedom parameter of a Student t distribution must be strictly positive.")
  }
  if(is.null(ncp)){
    new_dist(df = df, class = "dist_student_t")
  } else {
    new_dist(df = df, ncp = ncp, class = "dist_student_t")
  }
}

#' @export
print.dist_student_t <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_student_t <- function(x, digits = 2, ...){
  sprintf(
    "t(%s)",
    format(x[["df"]], digits = digits, ...)
  )
}

#' @export
density.dist_student_t <- function(x, at, ...){
  if(is.null(x[["ncp"]])){
    stats::dt(at, x[["df"]])
  } else {
    stats::dt(at, x[["df"]], x[["ncp"]])
  }
}

#' @export
quantile.dist_student_t <- function(x, p, ...){
  if(is.null(x[["ncp"]])){
    stats::qt(p, x[["df"]])
  } else {
    stats::qt(p, x[["df"]], x[["ncp"]])
  }
}

#' @export
cdf.dist_student_t <- function(x, q, ...){
  if(is.null(x[["ncp"]])){
    stats::pt(q, x[["df"]])
  } else {
    stats::pt(q, x[["df"]], x[["ncp"]])
  }
}

#' @export
generate.dist_student_t <- function(x, times, ...){
  if(is.null(x[["ncp"]])){
    stats::rt(times, x[["df"]])
  } else {
    stats::rt(times, x[["df"]], x[["ncp"]])
  }
}

#' @export
mean.dist_student_t <- function(x, ...){
  df <- x[["df"]]
  if(df <= 1) return(NA_real_)
  if(is.null(x[["ncp"]])){
    0
  } else {
    x[["ncp"]] * sqrt(df/2) * (gamma((df-1)/2)/gamma(df/2))
  }
}

#' @export
variance.dist_student_t <- function(x, ...){
  df <- x[["df"]]
  if(df <= 1) return(NA_real_)
  if(df <= 2) return(Inf)
  if(is.null(x[["ncp"]])){
    df / (df - 2)
  } else {
    (df*(1+x[["ncp"]]^2))/(df-2) - mean(x)^2
  }
}
