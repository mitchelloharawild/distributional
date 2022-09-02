#' The Gamma distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Several important distributions are special cases of the Gamma
#' distribution. When the shape parameter is `1`, the Gamma is an
#' exponential distribution with parameter \eqn{1/\beta}. When the
#' \eqn{shape = n/2} and \eqn{rate = 1/2}, the Gamma is a equivalent to
#' a chi squared distribution with n degrees of freedom. Moreover, if
#' we have \eqn{X_1} is \eqn{Gamma(\alpha_1, \beta)} and
#' \eqn{X_2} is \eqn{Gamma(\alpha_2, \beta)}, a function of these two variables
#' of the form \eqn{\frac{X_1}{X_1 + X_2}} \eqn{Beta(\alpha_1, \alpha_2)}.
#' This last property frequently appears in another distributions, and it
#' has extensively been used in multivariate methods. More about the Gamma
#' distribution will be added soon.
#'
#' @inheritParams stats::dgamma
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Gamma random variable
#'   with parameters
#'   `shape` = \eqn{\alpha} and
#'   `rate` = \eqn{\beta}.
#'
#'   **Support**: \eqn{x \in (0, \infty)}
#'
#'   **Mean**: \eqn{\frac{\alpha}{\beta}}
#'
#'   **Variance**: \eqn{\frac{\alpha}{\beta^2}}
#'
#'   **Probability density function (p.m.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\beta^{\alpha}}{\Gamma(\alpha)} x^{\alpha - 1} e^{-\beta x}
#'   }{
#'     f(x) = \frac{\beta^{\alpha}}{\Gamma(\alpha)} x^{\alpha - 1} e^{-\beta x}
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\Gamma(\alpha, \beta x)}{\Gamma{\alpha}}
#'   }{
#'     f(x) = \frac{\Gamma(\alpha, \beta x)}{\Gamma{\alpha}}
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \Big(\frac{\beta}{ \beta - t}\Big)^{\alpha}, \thinspace t < \beta
#'   }{
#'     E(e^(tX)) = \Big(\frac{\beta}{ \beta - t}\Big)^{\alpha}, \thinspace t < \beta
#'   }
#'
#' @seealso [stats::GammaDist]
#'
#' @examples
#' dist <- dist_gamma(shape = c(1,2,3,5,9,7.5,0.5), rate = c(0.5,0.5,0.5,1,2,1,1))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
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
#' @name dist_gamma
#' @export
dist_gamma <- function(shape, rate, scale = 1/rate){
  shape <- vec_cast(shape, double())
  rate <- vec_cast(rate, double())
  if(any(shape[!is.na(shape)] < 0)){
    abort("The shape parameter of a Gamma distribution must be non-negative.")
  }
  if(any(rate[!is.na(rate)] <= 0)){
    abort("The rate parameter of a Gamma distribution must be strictly positive.")
  }
  new_dist(shape = shape, rate = 1/scale, class = "dist_gamma")
}

#' @export
format.dist_gamma <- function(x, digits = 2, ...){
  sprintf(
    if (is_utf8_output()) "\u0393(%s, %s)" else "Gamma(%s, %s)",
    format(x[["shape"]], digits = digits, ...),
    format(x[["rate"]], digits = digits, ...)
  )
}

#' @export
density.dist_gamma <- function(x, at, ...){
  stats::dgamma(at, x[["shape"]], x[["rate"]])
}

#' @export
log_density.dist_gamma <- function(x, at, ...){
  stats::dgamma(at, x[["shape"]], x[["rate"]], log = TRUE)
}

#' @export
quantile.dist_gamma <- function(x, p, ...){
  stats::qgamma(p, x[["shape"]], x[["rate"]])
}

#' @export
cdf.dist_gamma <- function(x, q, ...){
  stats::pgamma(q, x[["shape"]], x[["rate"]])
}

#' @export
generate.dist_gamma <- function(x, times, ...){
  stats::rgamma(times, x[["shape"]], x[["rate"]])
}

#' @export
mean.dist_gamma <- function(x, ...){
  x[["shape"]] / x[["rate"]]
}

#' @export
covariance.dist_gamma <- function(x, ...){
  x[["shape"]] / x[["rate"]]^2
}

#' @export
skewness.dist_gamma <- function(x, ...) 2 / sqrt(x[["shape"]])

#' @export
kurtosis.dist_gamma <- function(x, ...) 6 / x[["shape"]]
