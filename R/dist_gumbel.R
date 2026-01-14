#' The Gumbel distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Gumbel distribution is a special case of the Generalized Extreme Value
#' distribution, obtained when the GEV shape parameter \eqn{\xi} is equal to 0.
#' It may be referred to as a type I extreme value distribution.
#'
#' @inheritParams actuar::dgumbel
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_gumbel")`
#'
#'   In the following, let \eqn{X} be a Gumbel random variable with location
#'   parameter  `alpha` = \eqn{\alpha} and scale parameter `scale` = \eqn{\sigma}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers.
#'
#'   **Mean**: 
#'   
#'   \deqn{
#'     E(X) = \alpha + \sigma\gamma
#'   }{
#'     E(X) = alpha + sigma*gamma
#'   }
#'   
#'   where \eqn{\gamma} is the Euler-Mascheroni constant, 
#'   approximately equal to 0.5772157.
#'
#'   **Variance**: 
#'   
#'   \deqn{
#'     \textrm{Var}(X) = \frac{\pi^2 \sigma^2}{6}
#'   }{
#'     Var(X) = (pi^2 * sigma^2) / 6
#'   }
#'
#'   **Skewness**: 
#'   
#'   \deqn{
#'     \textrm{Skew}(X) = \frac{12\sqrt{6}\zeta(3)}{\pi^3} \approx 1.1395
#'   }{
#'     Skew(X) = (12*sqrt(6)*zeta(3)) / pi^3
#'   }
#'   
#'   where \eqn{\zeta(3)} is Apery's constant, 
#'   approximately equal to 1.2020569. Note that skewness is independent 
#'   of the distribution parameters.
#'
#'   **Kurtosis (excess)**: 
#'   
#'   \deqn{
#'     \textrm{Kurt}(X) = \frac{12}{5} = 2.4
#'   }{
#'     Kurt(X) = 12/5
#'   }
#'   
#'   Note that excess kurtosis is independent of the distribution parameters.
#'
#'   **Median**: 
#'   
#'   \deqn{
#'     \textrm{Median}(X) = \alpha - \sigma\ln(\ln 2)
#'   }{
#'     Median(X) = alpha - sigma*ln(ln(2))
#'   }
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{1}{\sigma} \exp\left[-\frac{x - \alpha}{\sigma}\right]
#'            \exp\left\{-\exp\left[-\frac{x - \alpha}{\sigma}\right]\right\}
#'   }{
#'     f(x) = (1/sigma) * exp[-(x - alpha)/sigma] * exp(-exp[-(x - alpha)/sigma])
#'   }
#'   
#'   for \eqn{x} in \eqn{R}, the set of all real numbers.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = \exp\left\{-\exp\left[-\frac{x - \alpha}{\sigma}\right]\right\}
#'   }{
#'     F(x) = exp(-exp[-(x - alpha)/sigma])
#'   }
#'   
#'   for \eqn{x} in \eqn{R}, the set of all real numbers.
#'
#'   **Quantile function (inverse c.d.f)**:
#'
#'   \deqn{
#'     F^{-1}(p) = \alpha - \sigma \ln(-\ln p)
#'   }{
#'     F^(-1)(p) = alpha - sigma * ln(-ln(p))
#'   }
#'   
#'   for \eqn{p} in (0, 1).
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \Gamma(1 - \sigma t) e^{\alpha t}
#'   }{
#'     E(e^(tX)) = Gamma(1 - sigma*t) * e^(alpha*t)
#'   }
#'   
#'   for \eqn{\sigma t < 1}, where \eqn{\Gamma} is the gamma function.
#'
#' @seealso [actuar::Gumbel], [actuar::dgumbel()], [actuar::pgumbel()], 
#'   [actuar::qgumbel()], [actuar::rgumbel()], [actuar::mgumbel()]
#'
#' @examples
#' dist <- dist_gumbel(alpha = c(0.5, 1, 1.5, 3), scale = c(2, 2, 3, 4))
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
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
#' @name dist_gumbel
#' @export
dist_gumbel <- function(alpha, scale){
  alpha <- vec_cast(alpha, double())
  scale <- vec_cast(scale, double())
  if(any(scale <= 0)){
    abort("The scale parameter of a Gumbel distribution must be strictly positive.")
  }
  new_dist(a = alpha, s = scale, class = "dist_gumbel")
}

#' @export
format.dist_gumbel <- function(x, digits = 2, ...){
  sprintf(
    "Gumbel(%s, %s)",
    format(x[["a"]], digits = digits, ...),
    format(x[["s"]], digits = digits, ...)
  )
}

#' @export
density.dist_gumbel <- function(x, at, ...){
  require_package("actuar")
  actuar::dgumbel(at, x[["a"]], x[["s"]])
}

#' @export
log_density.dist_gumbel <- function(x, at, ...){
  require_package("actuar")
  actuar::dgumbel(at, x[["a"]], x[["s"]], log = TRUE)
}

#' @export
quantile.dist_gumbel <- function(x, p, ...){
  require_package("actuar")
  actuar::qgumbel(p, x[["a"]], x[["s"]])
}

#' @export
cdf.dist_gumbel <- function(x, q, ...){
  require_package("actuar")
  actuar::pgumbel(q, x[["a"]], x[["s"]])
}

#' @export
generate.dist_gumbel <- function(x, times, ...){
  require_package("actuar")
  actuar::rgumbel(times, x[["a"]], x[["s"]])
}

#' @export
mean.dist_gumbel <- function(x, ...){
  actuar::mgumbel(1, x[["a"]], x[["s"]])
}

#' @export
covariance.dist_gumbel <- function(x, ...){
  (pi*x[["s"]])^2/6
}

#' @export
skewness.dist_gumbel <- function(x, ...) {
  1.13954709940465
  # zeta3 <- 1.20205690315959401459612
  # (12 * sqrt(6) * zeta3) / pi^3
}

#' @export
kurtosis.dist_gumbel <- function(x, ...) 12/5
