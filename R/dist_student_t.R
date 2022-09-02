#' The (non-central) location-scale Student t Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Student's T distribution is closely related to the [Normal()]
#' distribution, but has heavier tails. As \eqn{\nu} increases to \eqn{\infty},
#' the Student's T converges to a Normal. The T distribution appears
#' repeatedly throughout classic frequentist hypothesis testing when
#' comparing group means.
#'
#' @inheritParams stats::dt
#' @param mu The location parameter of the distribution.
#'   If `ncp == 0` (or `NULL`), this is the median.
#' @param sigma The scale parameter of the distribution.
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a **central** Students T random variable
#'   with `df` = \eqn{\nu}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers
#'
#'   **Mean**: Undefined unless \eqn{\nu \ge 2}, in which case the mean is
#'     zero.
#'
#'   **Variance**:
#'
#'   \deqn{
#'     \frac{\nu}{\nu - 2}
#'   }{
#'     \nu / (\nu - 2)
#'   }
#'
#'   Undefined if \eqn{\nu < 1}, infinite when \eqn{1 < \nu \le 2}.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\Gamma(\frac{\nu + 1}{2})}{\sqrt{\nu \pi} \Gamma(\frac{\nu}{2})} (1 + \frac{x^2}{\nu} )^{- \frac{\nu + 1}{2}}
#'   }{
#'     f(x) = \Gamma((\nu + 1) / 2) / (\sqrt(\nu \pi) \Gamma(\nu / 2)) (1 + x^2 / \nu)^(- (\nu + 1) / 2)
#'   }
#'
#' @seealso [stats::TDist]
#'
#' @examples
#' dist <- dist_student_t(df = c(1,2,5), mu = c(0,1,2), sigma = c(1,2,3))
#'
#' dist
#' mean(dist)
#' variance(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_student_t
#' @export
dist_student_t <- function(df, mu = 0, sigma = 1, ncp = NULL){
  df <- vec_cast(df, numeric())
  if(any(df <= 0)){
    abort("The degrees of freedom parameter of a Student t distribution must be strictly positive.")
  }
  mu <- vec_cast(mu, double())
  sigma <- vec_cast(sigma, double())
  if(any(sigma[!is.na(sigma)] <= 0)){
    abort("The scale (sigma) parameter of a Student t distribution must be strictly positive.")
  }
  new_dist(df = df, mu = mu, sigma = sigma, ncp = ncp, class = "dist_student_t")
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
log_density.dist_student_t <- function(x, at, ...){
  ncp <- x[["ncp"]] %||% missing_arg()
  sigma <- x[["sigma"]]

  stats::dt((at - x[["mu"]])/sigma, x[["df"]], ncp, log = TRUE) - log(sigma)
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
covariance.dist_student_t <- function(x, ...){
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
