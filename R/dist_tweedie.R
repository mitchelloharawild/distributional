#' The Tweedie distribution
#'
#' @description
#'
#' The Tweedie distribution is a family of exponential dispersion models
#' characterised by a power variance function \eqn{\mathrm{Var}(X) = \phi
#' \mu^p}. For a power parameter \eqn{p \in (1, 2)}, it corresponds to a
#' compound Poisson-Gamma distribution: a Poisson-distributed number of
#' events occur, each contributing a Gamma-distributed amount. This produces
#' a distribution with a point mass at zero (when no events occur) mixed
#' with a continuous, positive, right-skewed component, making it well
#' suited to intermittent or non-negative data such as insurance claims and
#' rainfall.
#'
#' @inheritParams tweedieDistr::dtweedie
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_tweedie")`
#'
#'   In the following, let \eqn{X} be a Tweedie random variable with
#'   parameters `mean` = \eqn{\mu > 0}, `dispersion` = \eqn{\phi > 0}, and
#'   `power` = \eqn{p \in (1, 2)}.
#'
#'   **Support**: \eqn{\{0\} \cup (0, \infty)}
#'
#'   **Mean**: \eqn{\mu}
#'
#'   **Variance**: \eqn{\phi \mu^p}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   The Tweedie distribution does not have a closed-form density. Instead,
#'   it is defined as a Poisson sum of Gamma random variables, with
#'   \eqn{P(X = 0) = \exp(-\lambda)} and, for \eqn{x > 0},
#'
#'   \deqn{
#'     f(x) = \sum_{j=1}^{\infty} \frac{\lambda^j e^{-\lambda}}{j!}
#'       \frac{x^{j \alpha - 1} e^{-x / \gamma}}{\gamma^{j \alpha} \Gamma(j \alpha)}
#'   }{
#'     f(x) = sum_{j=1}^Inf (\lambda^j exp(-\lambda) / j!) *
#'       (x^(j \alpha - 1) exp(-x / \gamma) / (\gamma^(j \alpha) \Gamma(j \alpha)))
#'   }
#'
#'   where \eqn{\lambda = \mu^{2 - p} / (\phi (2 - p))} is the Poisson rate,
#'   \eqn{\alpha = (2 - p) / (p - 1)} is the Gamma shape per event, and
#'   \eqn{\gamma = \phi (p - 1) \mu^{p - 1}} is the Gamma scale. The density
#'   is evaluated using the series expansion of Dunn & Smyth (2005).
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cumulative distribution function is evaluated numerically from the
#'   density series, as it does not have a closed-form expression.
#'
#' @references
#'   Dunn, P. K., & Smyth, G. K. (2005). Series evaluation of Tweedie
#'   exponential dispersion model densities. *Statistics and Computing*,
#'   15(4), 267-280. \doi{10.1007/s11222-005-4070-y}.
#'
#' @seealso [tweedieDistr::tweedie]
#'
#' @examples
#' dist <- dist_tweedie(mean = c(1, 2, 5), dispersion = 0.8, power = 1.5)
#' dist
#'
#' @examplesIf requireNamespace("tweedieDistr", quietly = TRUE)
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
#' @name dist_tweedie
#' @export
dist_tweedie <- function(mean = 1, dispersion = 1, power = 1.5){
  mean <- vec_cast(mean, double())
  dispersion <- vec_cast(dispersion, double())
  power <- vec_cast(power, double())
  if(any(mean <= 0)){
    abort("The mean parameter of a Tweedie distribution must be strictly positive.")
  }
  if(any(dispersion <= 0)){
    abort("The dispersion parameter of a Tweedie distribution must be strictly positive.")
  }
  if(any(power <= 1 | power >= 2)){
    abort("The power parameter of a Tweedie distribution must be in the interval (1, 2).")
  }
  new_dist(mu = mean, phi = dispersion, p = power, class = "dist_tweedie")
}

#' @export
format.dist_tweedie <- function(x, digits = 2, ...){
  sprintf(
    "Tweedie(%s, %s, %s)",
    format(x[["mu"]], digits = digits, ...),
    format(x[["phi"]], digits = digits, ...),
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_tweedie <- function(x, at, ...){
  require_package("tweedieDistr")
  tweedieDistr::dtweedie(at, x[["mu"]], x[["phi"]], x[["p"]])
}

#' @export
log_density.dist_tweedie <- function(x, at, ...){
  require_package("tweedieDistr")
  tweedieDistr::dtweedie(at, x[["mu"]], x[["phi"]], x[["p"]], log = TRUE)
}

#' @export
quantile.dist_tweedie <- function(x, p, ...){
  require_package("tweedieDistr")
  tweedieDistr::qtweedie(p, x[["mu"]], x[["phi"]], x[["p"]])
}

#' @export
log_quantile.dist_tweedie <- function(x, p, ...){
  require_package("tweedieDistr")
  tweedieDistr::qtweedie(p, x[["mu"]], x[["phi"]], x[["p"]], log.p = TRUE)
}

#' @export
cdf.dist_tweedie <- function(x, q, ...){
  require_package("tweedieDistr")
  tweedieDistr::ptweedie(q, x[["mu"]], x[["phi"]], x[["p"]])
}

#' @export
log_cdf.dist_tweedie <- function(x, q, ...){
  require_package("tweedieDistr")
  tweedieDistr::ptweedie(q, x[["mu"]], x[["phi"]], x[["p"]], log.p = TRUE)
}

#' @export
generate.dist_tweedie <- function(x, times, ...){
  require_package("tweedieDistr")
  tweedieDistr::rtweedie(times, x[["mu"]], x[["phi"]], x[["p"]])
}

#' @export
mean.dist_tweedie <- function(x, ...){
  x[["mu"]]
}

#' @export
covariance.dist_tweedie <- function(x, ...){
  x[["phi"]] * x[["mu"]]^x[["p"]]
}

#' @export
skewness.dist_tweedie <- function(x, ...){
  x[["p"]] * sqrt(x[["phi"]]) * x[["mu"]]^(x[["p"]] / 2 - 1)
}

#' @export
kurtosis.dist_tweedie <- function(x, ...){
  x[["p"]] * (2 * x[["p"]] - 1) * x[["phi"]] * x[["mu"]]^(x[["p"]] - 2)
}
