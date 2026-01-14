#' The Laplace distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Laplace distribution, also known as the double exponential distribution,
#' is a continuous probability distribution that is symmetric around its location
#' parameter.
#'
#' @param mu The location parameter (mean) of the Laplace distribution.
#' @param sigma The positive scale parameter of the Laplace distribution.
#'
#' @details
#' 
#' `r pkgdown_doc_link("dist_laplace")`
#'
#'   In the following, let \eqn{X} be a Laplace random variable with location
#'   parameter `mu` = \eqn{\mu} and scale parameter `sigma` = \eqn{\sigma}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers
#'
#'   **Mean**: \eqn{\mu}
#'
#'   **Variance**: \eqn{2\sigma^2}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{1}{2\sigma} \exp\left(-\frac{|x - \mu|}{\sigma}\right)
#'   }{
#'     f(x) = 1 / (2 * sigma) * exp(-|x - mu| / sigma)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = \begin{cases}
#'       \frac{1}{2} \exp\left(\frac{x - \mu}{\sigma}\right) & \text{if } x < \mu \\
#'       1 - \frac{1}{2} \exp\left(-\frac{x - \mu}{\sigma}\right) & \text{if } x \geq \mu
#'     \end{cases}
#'   }{
#'     F(x) = 1/2 * exp((x - mu) / sigma) if x < mu,
#'            1 - 1/2 * exp(-(x - mu) / sigma) if x >= mu
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \frac{\exp(\mu t)}{1 - \sigma^2 t^2} \text{ for } |t| < \frac{1}{\sigma}
#'   }{
#'     E(e^(tX)) = exp(mu * t) / (1 - sigma^2 * t^2) for |t| < 1/sigma
#'   }
#'
#' @seealso [extraDistr::Laplace]
#'
#' @examples
#' dist <- dist_laplace(mu = c(0, 2, -1), sigma = c(1, 2, 0.5))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 0)
#' density(dist, 0, log = TRUE)
#'
#' cdf(dist, 1)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_laplace
#' @export
dist_laplace <- function(mu, sigma) {
  mu <- vec_cast(mu, double())
  sigma <- vec_cast(sigma, double())
  if (any(sigma <= 0)) {
    abort("The scale parameter of a Laplace distribution must be positive.")
  }
  new_dist(mu = mu, sigma = sigma, class = "dist_laplace")
}

#' @export
format.dist_laplace <- function(x, digits = 2, ...) {
  sprintf(
    "Laplace(%s, %s)",
    format(x[["mu"]], digits = digits, ...),
    format(x[["sigma"]], digits = digits, ...)
  )
}

#' @export
density.dist_laplace <- function(x, at, ...) {
  mu <- x[["mu"]]
  sigma <- x[["sigma"]]
  (1 / (2 * sigma)) * exp(-abs(at - mu) / sigma)
}

#' @export
log_density.dist_laplace <- function(x, at, ...) {
  mu <- x[["mu"]]
  sigma <- x[["sigma"]]
  -log(2 * sigma) - abs(at - mu) / sigma
}

#' @export
quantile.dist_laplace <- function(x, p, ...) {
  mu <- x[["mu"]]
  sigma <- x[["sigma"]]
  mu - sigma * sign(p - 0.5) * log(1 - 2 * abs(p - 0.5))
}

#' @export
cdf.dist_laplace <- function(x, q, ...) {
  mu <- x[["mu"]]
  sigma <- x[["sigma"]]
  ifelse(q < mu,
    0.5 * exp((q - mu) / sigma),
    1 - 0.5 * exp(-(q - mu) / sigma)
  )
}

#' @export
generate.dist_laplace <- function(x, times, ...) {
  mu <- x[["mu"]]
  sigma <- x[["sigma"]]
  u <- stats::runif(times) - 0.5
  mu - sigma * sign(u) * log(1 - 2 * abs(u))
}

#' @export
mean.dist_laplace <- function(x, ...) {
  x[["mu"]]
}

#' @export
variance.dist_laplace <- function(x, ...) {
  2 * x[["sigma"]]^2
}

#' @export
skewness.dist_laplace <- function(x, ...) {
  0
}

#' @export
kurtosis.dist_laplace <- function(x, ...) {
  3
}

#' @export
has_symmetry.dist_laplace <- function(x, ...) {
  TRUE
}