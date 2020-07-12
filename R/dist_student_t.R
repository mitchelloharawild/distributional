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
dist_student_t <- function(df, mu = 0, sigma = 1, ncp = NULL){
  df <- vec_cast(df, if(any(is.infinite(df))) numeric() else integer())
  if(any(df <= 0)){
    abort("The degrees of freedom parameter of a Student t distribution must be strictly positive.")
  }
  mu <- vec_cast(mu, double())
  sigma <- vec_cast(sigma, double())
  if(any(sigma[!is.na(sigma)] <= 0)){
    abort("The scale parameter of a Student t distribution must be strictly positive.")
  }
  new_dist(df = df, mu = mu, sigma = sigma, ncp = ncp, class = "dist_student_t")
}

#' @export
print.dist_student_t <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_student_t <- function(x, digits = 2, ...){
  out <- sprintf(
    "t(%s, %s, %s%s)",
    format(x[["df"]], digits = digits, ...),
    format(x[["mu"]], digits = digits, ...),
    format(x[["sigma"]], digits = digits, ...),
    if(is.null(x[["ncp"]])) "" else paste(",", format(x[["ncp"]], digits = digits, ...))
  )
}

#' @export
density.dist_student_t <- function(x, at, ...){
  ncp <- x[["ncp"]] %||% missing_arg()

  stats::dt(at, x[["df"]], ncp)
}

#' @export
quantile.dist_student_t <- function(x, p, ...){
  ncp <- x[["ncp"]] %||% missing_arg()

  stats::qt(p, x[["df"]], ncp)
}

#' @export
cdf.dist_student_t <- function(x, q, ...){
  ncp <- x[["ncp"]] %||% missing_arg()

  stats::pt(q, x[["df"]], ncp)
}

#' @export
generate.dist_student_t <- function(x, times, ...){
  ncp <- x[["ncp"]] %||% missing_arg()

  stats::rt(times, x[["df"]], ncp)
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
