#' The Poisson-Inverse Gaussian distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Poisson-Inverse Gaussian distribution is a compound Poisson distribution
#' where the rate parameter follows an Inverse Gaussian distribution. It is
#' useful for modeling overdispersed count data.
#'
#' @inheritParams actuar::dpoisinvgauss
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_poisson_inverse_gaussian")`
#'
#'
#'   In the following, let \eqn{X} be a Poisson-Inverse Gaussian random variable
#'   with parameters `mean` = \eqn{\mu} and `shape` = \eqn{\phi}.
#'
#'   **Support**: \eqn{\{0, 1, 2, 3, ...\}}{{0, 1, 2, 3, ...}}
#'
#'   **Mean**: \eqn{\mu}
#'
#'   **Variance**: \eqn{\frac{\mu}{\phi}(\mu^2 + \phi)}{\mu/\phi (\mu^2 + \phi)}
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = x) = \frac{e^{\phi}}{\sqrt{2\pi}}
#'     \left(\frac{\phi}{\mu^2}\right)^{x/2}
#'     \frac{1}{x!}
#'     \int_0^\infty u^{x-1/2}
#'     \exp\left(-\frac{\phi u}{2} - \frac{\phi}{2\mu^2 u}\right) du
#'   }{
#'     P(X = x) = (e^\phi / sqrt(2\pi)) (\phi/\mu^2)^(x/2) (1/x!)
#'     \int_0^\infty u^(x-1/2) exp(-\phi u/2 - \phi/(2\mu^2 u)) du
#'   }
#'
#'   for \eqn{x = 0, 1, 2, \ldots}{x = 0, 1, 2, ...}
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(X \le x) = \sum_{k=0}^{\lfloor x \rfloor} P(X = k)
#'   }{
#'     P(X <= x) = sum_{k=0}^x P(X = k)
#'   }
#'
#'   The c.d.f does not have a closed form and is approximated numerically.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \exp\left\{\phi\left[1 - \sqrt{1 - \frac{2\mu^2}{\phi}(e^t - 1)}\right]\right\}
#'   }{
#'     E(e^(tX)) = exp{\phi [1 - sqrt(1 - 2\mu^2/\phi (e^t - 1))]}
#'   }
#'
#'   for \eqn{t < -\log(1 + \phi/(2\mu^2))}{t < -log(1 + \phi/(2\mu^2))}
#'
#' @seealso [actuar::PoissonInverseGaussian], [actuar::dpoisinvgauss()],
#'   [actuar::ppoisinvgauss()], [actuar::qpoisinvgauss()], [actuar::rpoisinvgauss()]
#'
#' @examples
#' dist <- dist_poisson_inverse_gaussian(mean = rep(0.1, 3), shape = c(0.4, 0.8, 1))
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
#' @name dist_poisson_inverse_gaussian
#' @export
dist_poisson_inverse_gaussian <- function(mean, shape){
  mean <- vec_cast(mean, double())
  shape <- vec_cast(shape, double())
  if(any(mean[!is.na(mean)] <= 0)){
    abort("The mean parameter of a Poisson-Inverse Gaussian distribution must be strictly positive.")
  }
  if(any(shape[!is.na(shape)] <= 0)){
    abort("The shape parameter of a Poisson-Inverse Gaussian distribution must be strictly positive.")
  }
  new_dist(m = mean, s = shape, class = "dist_poisson_inverse_gaussian")
}

#' @export
format.dist_poisson_inverse_gaussian <- function(x, digits = 2, ...){
  sprintf(
    "PIG(%s, %s)",
    format(x[["m"]], digits = digits, ...),
    format(x[["s"]], digits = digits, ...)
  )
}

#' @export
density.dist_poisson_inverse_gaussian <- function(x, at, ...){
  require_package("actuar")
  actuar::dpoisinvgauss(at, x[["m"]], x[["s"]])
}

#' @export
log_density.dist_poisson_inverse_gaussian <- function(x, at, ...){
  require_package("actuar")
  actuar::dpoisinvgauss(at, x[["m"]], x[["s"]], log = TRUE)
}

#' @export
quantile.dist_poisson_inverse_gaussian <- function(x, p, ...){
  require_package("actuar")
  actuar::qpoisinvgauss(p, x[["m"]], x[["s"]])
}

#' @export
cdf.dist_poisson_inverse_gaussian <- function(x, q, ...){
  require_package("actuar")
  actuar::ppoisinvgauss(q, x[["m"]], x[["s"]])
}

#' @export
generate.dist_poisson_inverse_gaussian <- function(x, times, ...){
  require_package("actuar")
  actuar::rpoisinvgauss(times, x[["m"]], x[["s"]])
}

#' @export
mean.dist_poisson_inverse_gaussian <- function(x, ...){
  x[["m"]]
}

#' @export
covariance.dist_poisson_inverse_gaussian <- function(x, ...){
  x[["m"]]/x[["s"]] * (x[["m"]]^2 + x[["s"]])
}
