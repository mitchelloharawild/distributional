#' The Inverse Gamma distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Inverse Gamma distribution is commonly used as a prior distribution 
#' in Bayesian statistics, particularly for variance parameters.
#'
#' @inheritParams actuar::dinvgamma
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_inverse_gamma")`
#'
#'   In the following, let \eqn{X} be an Inverse Gamma random variable with
#'   shape parameter `shape` = \eqn{\alpha} and rate parameter 
#'   `rate` = \eqn{\beta} (equivalently, scale = \eqn{1/\beta}).
#'
#'   **Support**: \eqn{x \in (0, \infty)}
#'
#'   **Mean**: \eqn{\frac{\beta}{\alpha - 1}} for \eqn{\alpha > 1}, 
#'   otherwise undefined
#'
#'   **Variance**: \eqn{\frac{\beta^2}{(\alpha - 1)^2 (\alpha - 2)}} 
#'   for \eqn{\alpha > 2}, otherwise undefined
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\beta^\alpha}{\Gamma(\alpha)} x^{-\alpha - 1} 
#'     e^{-\beta/x}
#'   }{
#'     f(x) = \beta^\alpha / \Gamma(\alpha) x^(-\alpha - 1) e^(-\beta/x)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = \frac{\Gamma(\alpha, \beta/x)}{\Gamma(\alpha)} = 
#'     Q(\alpha, \beta/x)
#'   }{
#'     F(x) = \Gamma(\alpha, \beta/x) / \Gamma(\alpha) = Q(\alpha, \beta/x)
#'   }
#'
#'   where \eqn{\Gamma(\alpha, z)} is the upper incomplete gamma function and 
#'   \eqn{Q} is the regularized incomplete gamma function.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     M_X(t) = \frac{2 (-\beta t)^{\alpha/2}}{\Gamma(\alpha)} 
#'     K_\alpha\left(\sqrt{-4\beta t}\right)
#'   }{
#'     M_X(t) = 2 (-\beta t)^(\alpha/2) / \Gamma(\alpha) K_\alpha(sqrt(-4\beta t))
#'   }
#'
#'   for \eqn{t < 0}, where \eqn{K_\alpha} is the modified Bessel function 
#'   of the second kind. The MGF does not exist for \eqn{t \ge 0}.
#'
#' @seealso [actuar::InverseGamma]
#'
#' @examples
#' dist <- dist_inverse_gamma(shape = c(1,2,3,3), rate = c(1,1,1,2))
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_inverse_gamma
#' @export
dist_inverse_gamma <- function(shape, rate = 1/scale, scale){
  shape <- vec_cast(shape, double())
  rate <- vec_cast(rate, double())
  if(any(shape <= 0)){
    abort("The shape parameter of a Inverse Gamma distribution must be strictly positive.")
  }
  if(any(rate <= 0)){
    abort("The rate/scale parameter of a Inverse Gamma distribution must be strictly positive.")
  }
  new_dist(s = shape, r = rate, class = "dist_inverse_gamma")
}

#' @export
format.dist_inverse_gamma <- function(x, digits = 2, ...){
  sprintf(
    "InvGamma(%s, %s)",
    format(x[["s"]], digits = digits, ...),
    format(1/x[["r"]], digits = digits, ...)
  )
}

#' @export
density.dist_inverse_gamma <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvgamma(at, x[["s"]], x[["r"]])
}

#' @export
log_density.dist_inverse_gamma <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvgamma(at, x[["s"]], x[["r"]], log = TRUE)
}

#' @export
quantile.dist_inverse_gamma <- function(x, p, ...){
  require_package("actuar")
  actuar::qinvgamma(p, x[["s"]], x[["r"]])
}

#' @export
cdf.dist_inverse_gamma <- function(x, q, ...){
  require_package("actuar")
  actuar::pinvgamma(q, x[["s"]], x[["r"]])
}

#' @export
generate.dist_inverse_gamma <- function(x, times, ...){
  require_package("actuar")
  actuar::rinvgamma(times, x[["s"]], x[["r"]])
}

#' @export
mean.dist_inverse_gamma <- function(x, ...){
  if(x[["s"]] <= 1) return(NA_real_)
  1/(x[["r"]]*(x[["s"]]-1))
}

#' @export
covariance.dist_inverse_gamma <- function(x, ...){
  if(x[["s"]] <= 2) return(NA_real_)
  1/(x[["r"]]^2*(x[["s"]]-1)^2*(x[["s"]]-2))
}
