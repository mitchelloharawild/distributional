#' The Inverse Gaussian distribution
#'
#' The inverse Gaussian distribution (also known as the Wald distribution) is
#' commonly used to model positive-valued data, particularly in contexts
#' involving first passage times and reliability analysis.
#' 
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams actuar::dinvgauss
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_inverse_gaussian")`
#'
#'   In the following, let \eqn{X} be an Inverse Gaussian random variable with
#'   parameters `mean` = \eqn{\mu} and `shape` = \eqn{\lambda}.
#'
#'   **Support**: \eqn{(0, \infty)}
#'
#'   **Mean**: \eqn{\mu}
#'
#'   **Variance**: \eqn{\frac{\mu^3}{\lambda}}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \sqrt{\frac{\lambda}{2\pi x^3}}
#'     \exp\left(-\frac{\lambda(x - \mu)^2}{2\mu^2 x}\right)
#'   }{
#'     f(x) = sqrt(\lambda / (2 \pi x^3))
#'     exp(-\lambda (x - \mu)^2 / (2 \mu^2 x))
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = \Phi\left(\sqrt{\frac{\lambda}{x}}
#'     \left(\frac{x}{\mu} - 1\right)\right) +
#'     \exp\left(\frac{2\lambda}{\mu}\right)
#'     \Phi\left(-\sqrt{\frac{\lambda}{x}}
#'     \left(\frac{x}{\mu} + 1\right)\right)
#'   }{
#'     F(x) = \Phi(sqrt(\lambda/x) (x/\mu - 1)) +
#'     exp(2\lambda/\mu) \Phi(-sqrt(\lambda/x) (x/\mu + 1))
#'   }
#'
#'   where \eqn{\Phi} is the standard normal c.d.f.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \exp\left(\frac{\lambda}{\mu}
#'     \left(1 - \sqrt{1 - \frac{2\mu^2 t}{\lambda}}\right)\right)
#'   }{
#'     E(e^(tX)) = exp((\lambda/\mu) (1 - sqrt(1 - 2\mu^2 t/\lambda)))
#'   }
#'
#'   for \eqn{t < \frac{\lambda}{2\mu^2}}.
#'
#'   **Skewness**: \eqn{3\sqrt{\frac{\mu}{\lambda}}}
#'
#'   **Excess Kurtosis**: \eqn{\frac{15\mu}{\lambda}}
#'
#'   **Quantiles**: No closed-form expression, approximated numerically.
#'
#' @seealso [actuar::InverseGaussian]
#'
#' @examples
#' dist <- dist_inverse_gaussian(mean = c(1,1,1,3,3), shape = c(0.2, 1, 3, 0.2, 1))
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
#' @name dist_inverse_gaussian
#' @export
dist_inverse_gaussian <- function(mean, shape){
  mean <- vec_cast(mean, double())
  shape <- vec_cast(shape, double())
  if(any(mean[!is.na(mean)] <= 0)){
    abort("The mean parameter of a Inverse Gaussian distribution must be strictly positive.")
  }
  if(any(shape[!is.na(shape)] <= 0)){
    abort("The shape parameter of a Inverse Gaussian distribution must be strictly positive.")
  }
  new_dist(m = mean, s = shape, class = "dist_inverse_gaussian")
}

#' @export
format.dist_inverse_gaussian <- function(x, digits = 2, ...){
  sprintf(
    "IG(%s, %s)",
    format(x[["m"]], digits = digits, ...),
    format(x[["s"]], digits = digits, ...)
  )
}

#' @export
density.dist_inverse_gaussian <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvgauss(at, x[["m"]], x[["s"]])
}

#' @export
log_density.dist_inverse_gaussian <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvgauss(at, x[["m"]], x[["s"]], log = TRUE)
}

#' @export
quantile.dist_inverse_gaussian <- function(x, p, ...){
  require_package("actuar")
  actuar::qinvgauss(p, x[["m"]], x[["s"]])
}

#' @export
cdf.dist_inverse_gaussian <- function(x, q, ...){
  require_package("actuar")
  actuar::pinvgauss(q, x[["m"]], x[["s"]])
}

#' @export
generate.dist_inverse_gaussian <- function(x, times, ...){
  require_package("actuar")
  actuar::rinvgauss(times, x[["m"]], x[["s"]])
}

#' @export
mean.dist_inverse_gaussian <- function(x, ...){
  x[["m"]]
}

#' @export
covariance.dist_inverse_gaussian <- function(x, ...){
  x[["m"]]^3/x[["s"]]
}
