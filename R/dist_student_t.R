#' The (non-central) location-scale Student t Distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::dt
#' @param mu The location parameter of the distribution.
#'   If `ncp == 0` (or `NULL`), this is the median.
#' @param sigma The scale parameter of the distribution.
#'
#' @seealso [stats::TDist]
#'
#' @examples
#' dist_student_t(df = c(1,2,5), mu = c(0,1,2), sigma = c(1,2,3))
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
  sigma <- x[["sigma"]]

  stats::dt((at - x[["mu"]])/sigma, x[["df"]], ncp) / sigma
}

#' @export
quantile.dist_student_t <- function(x, p, ...){
  ncp <- x[["ncp"]] %||% missing_arg()

  stats::qt(p, x[["df"]], ncp) * x[["sigma"]] + x[["mu"]]
}

#' @export
cdf.dist_student_t <- function(x, q, ...){
  ncp <- x[["ncp"]] %||% missing_arg()

  stats::pt((q - x[["mu"]])/x[["sigma"]], x[["df"]], ncp)
}

#' @export
generate.dist_student_t <- function(x, times, ...){
  ncp <- x[["ncp"]] %||% missing_arg()

  stats::rt(times, x[["df"]], ncp) * x[["sigma"]] + x[["mu"]]
}

#' @export
mean.dist_student_t <- function(x, ...){
  df <- x[["df"]]
  if(df <= 1) return(NA_real_)
  if(is.null(x[["ncp"]])){
    x[["mu"]]
  } else {
    x[["mu"]] + x[["ncp"]] * sqrt(df/2) * (gamma((df-1)/2)/gamma(df/2)) * x[["sigma"]]
  }
}

#' @export
variance.dist_student_t <- function(x, ...){
  df <- x[["df"]]
  ncp <- x[["ncp"]]
  if(df <= 1) return(NA_real_)
  if(df <= 2) return(Inf)
  if(is.null(ncp)){
    df / (df - 2) * x[["sigma"]]^2
  } else {
    ((df*(1+ncp^2))/(df-2) - (ncp * sqrt(df/2) * (gamma((df-1)/2)/gamma(df/2)))^2) * x[["sigma"]]^2
  }
}
