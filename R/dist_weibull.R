#' The Weibull distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Generalization of the gamma distribution. Often used in survival and
#' time-to-event analyses.
#'
#' @inheritParams stats::dweibull
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_weibull")`
#'
#'   In the following, let \eqn{X} be a Weibull random variable with
#'   shape parameter `shape` = \eqn{k} and scale parameter `scale` = \eqn{\lambda}.
#'
#'   **Support**: \eqn{[0, \infty)}
#'
#'   **Mean**: 
#'
#'   \deqn{
#'     E(X) = \lambda \Gamma\left(1 + \frac{1}{k}\right)
#'   }{
#'     E(X) = \lambda \Gamma(1 + 1/k)
#'   }
#'
#'   where \eqn{\Gamma} is the gamma function.
#'
#'   **Variance**: 
#'
#'   \deqn{
#'     \text{Var}(X) = \lambda^2 \left[\Gamma\left(1 + \frac{2}{k}\right) - \left(\Gamma\left(1 + \frac{1}{k}\right)\right)^2\right]
#'   }{
#'     Var(X) = \lambda^2 [\Gamma(1 + 2/k) - (\Gamma(1 + 1/k))^2]
#'   }
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{k}{\lambda}\left(\frac{x}{\lambda}\right)^{k-1}e^{-(x/\lambda)^k}, \quad x \ge 0
#'   }{
#'     f(x) = (k/\lambda)(x/\lambda)^(k-1) e^(-(x/\lambda)^k), x >= 0
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = 1 - e^{-(x/\lambda)^k}, \quad x \ge 0
#'   }{
#'     F(x) = 1 - e^(-(x/\lambda)^k), x >= 0
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \sum_{n=0}^\infty \frac{t^n\lambda^n}{n!} \Gamma\left(1+\frac{n}{k}\right)
#'   }{
#'     E(e^(tX)) = \sum_(n=0)^\infty (t^n \lambda^n / n!) \Gamma(1 + n/k)
#'   }
#'
#'   **Skewness**:
#'
#'   \deqn{
#'     \gamma_1 = \frac{\mu^3 - 3\mu\sigma^2 - \mu^3}{\sigma^3}
#'   }{
#'     \gamma_1 = (\mu_3 - 3\mu\sigma^2 - \mu^3) / \sigma^3
#'   }
#'
#'   where \eqn{\mu = E(X)}, \eqn{\sigma^2 = \text{Var}(X)}, and the third 
#'   raw moment is
#'
#'   \deqn{
#'     \mu^3 = \lambda^3 \Gamma\left(1 + \frac{3}{k}\right)
#'   }{
#'     \mu_3 = \lambda^3 \Gamma(1 + 3/k)
#'   }
#'
#'   **Excess Kurtosis**:
#'
#'   \deqn{
#'     \gamma_2 = \frac{\mu^4 - 4\gamma_1\mu\sigma^3 - 6\mu^2\sigma^2 - \mu^4}{\sigma^4} - 3
#'   }{
#'     \gamma_2 = (\mu_4 - 4\gamma_1\mu\sigma^3 - 6\mu^2\sigma^2 - \mu^4) / \sigma^4 - 3
#'   }
#'
#'   where the fourth raw moment is
#'
#'   \deqn{
#'     \mu^4 = \lambda^4 \Gamma\left(1 + \frac{4}{k}\right)
#'   }{
#'     \mu_4 = \lambda^4 \Gamma(1 + 4/k)
#'   }
#'
#' @seealso [stats::Weibull]
#'
#' @examples
#' dist <- dist_weibull(shape = c(0.5, 1, 1.5, 5), scale = rep(1, 4))
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
#' @name dist_weibull
#' @export
dist_weibull <- function(shape, scale){
  shape <- vec_cast(shape, double())
  scale <- vec_cast(scale, double())
  if(any(shape[!is.na(shape)] < 0)){
    abort("The shape parameter of a Weibull distribution must be non-negative.")
  }
  if(any(scale[!is.na(scale)] <= 0)){
    abort("The scale parameter of a Weibull distribution must be strictly positive.")
  }
  new_dist(shape = shape, scale = scale, class = "dist_weibull")
}

#' @export
format.dist_weibull <- function(x, digits = 2, ...){
  sprintf(
    "Weibull(%s, %s)",
    format(x[["shape"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...)
  )
}

#' @export
density.dist_weibull <- function(x, at, ...){
  stats::dweibull(at, x[["shape"]], x[["scale"]])
}

#' @export
log_density.dist_weibull <- function(x, at, ...){
  stats::dweibull(at, x[["shape"]], x[["scale"]], log = TRUE)
}

#' @export
quantile.dist_weibull <- function(x, p, ...){
  stats::qweibull(p, x[["shape"]], x[["scale"]])
}

#' @export
cdf.dist_weibull <- function(x, q, ...){
  stats::pweibull(q, x[["shape"]], x[["scale"]])
}

#' @export
generate.dist_weibull <- function(x, times, ...){
  stats::rweibull(times, x[["shape"]], x[["scale"]])
}

#' @export
mean.dist_weibull <- function(x, ...){
  x[["scale"]] * gamma(1 + 1/x[["shape"]])
}

#' @export
covariance.dist_weibull <- function(x, ...){
  x[["scale"]]^2 * (gamma(1 + 2/x[["shape"]]) - gamma(1 + 1/x[["shape"]])^2)
}

#' @export
skewness.dist_weibull <- function(x, ...) {
  mu <- mean(x)
  sigma <- sqrt(variance(x))
  r <- mu / sigma
  gamma(1 + 3/x[["shape"]]) * (x[["scale"]]/sigma)^3 - 3*r - r^3
}

#' @export
kurtosis.dist_weibull <- function(x, ...) {
  mu <- mean(x)
  sigma <- sqrt(variance(x))
  gamma <- skewness(x)
  r <- mu / sigma
  (x[["scale"]]/sigma)^4 * gamma(1 + 4/x[["shape"]]) - 4*gamma*r -6*r^2 - r^4 - 3
}
