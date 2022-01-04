#' The Gumbel distribution
#'
#' \lifecycle{stable}
#'
#' The Gumbel distribution is a special case of the Generalized Extreme Value
#' distribution, obtained when the GEV shape parameter \eqn{\xi} is equal to 0.
#' It may be referred to as a type I extreme value distribution.
#'
#' @inheritParams actuar::dgumbel
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Gumbel random variable with location
#'   parameter  `mu` = \eqn{\mu}, scale parameter `sigma` = \eqn{\sigma}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers.
#'
#'   **Mean**: \eqn{\mu + \sigma\gamma}, where \eqn{\gamma} is Euler's
#'   constant, approximately equal to 0.57722.
#'
#'   **Median**: \eqn{\mu - \sigma\ln(\ln 2)}{\mu - \sigma ln(ln 2)}.
#'
#'   **Variance**: \eqn{\sigma^2 \pi^2 / 6}.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{f(x) = \sigma ^ {-1} \exp[-(x - \mu) / \sigma]%
#'         \exp\{-\exp[-(x - \mu) / \sigma] \}}{%
#'        f(x) = (1 / \sigma) exp[-(x - \mu) / \sigma]%
#'         exp{-exp[-(x - \mu) / \sigma]}}
#'   for \eqn{x} in \eqn{R}, the set of all real numbers.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   In the \eqn{\xi = 0} (Gumbel) special case
#'   \deqn{F(x) = \exp\{-\exp[-(x - \mu) / \sigma] \}}{%
#'         F(x) = exp{ - exp[-(x - \mu) / \sigma]} }
#'   for \eqn{x} in \eqn{R}, the set of all real numbers.
#'
#' @seealso [actuar::Gumbel]
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
  zeta3 <- 1.20205690315959401459612
  (12 * sqrt(6) * zeta3) / pi^3
}

#' @export
kurtosis.dist_gumbel <- function(x, ...) 12/5
