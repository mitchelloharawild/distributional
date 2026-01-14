#' The Generalized Extreme Value Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The GEV distribution is widely used in extreme value theory to model
#' the distribution of maxima (or minima) of samples. The parametric form
#' encompasses the Gumbel, Frechet, and reverse Weibull distributions.
#' 
#' @param location the location parameter \eqn{\mu} of the GEV distribution.
#' @param scale the scale parameter \eqn{\sigma} of the GEV distribution.
#' Must be strictly positive.
#' @param shape the shape parameter \eqn{\xi} of the GEV distribution.
#' Determines the tail behavior: \eqn{\xi = 0} gives Gumbel,
#' \eqn{\xi > 0} gives Frechet, \eqn{\xi < 0} gives reverse Weibull.
#' 
#' @details
#'
#' `r pkgdown_doc_link("dist_gev")`
#'
#' In the following, let \eqn{X} be a GEV random variable with parameters
#' `location` = \eqn{\mu}, `scale` = \eqn{\sigma}, and `shape` = \eqn{\xi}.
#'
#' **Support**:
#' * \eqn{x \in \mathbb{R}} (all real numbers) if \eqn{\xi = 0}
#' * \eqn{x \geq \mu - \sigma/\xi} if \eqn{\xi > 0}
#' * \eqn{x \leq \mu - \sigma/\xi} if \eqn{\xi < 0}
#'
#' **Mean**: 
#' \deqn{
#'   E(X) = \begin{cases}
#'     \mu + \sigma \gamma & \text{if } \xi = 0 \\
#'     \mu + \sigma \frac{\Gamma(1-\xi) - 1}{\xi} & \text{if } \xi < 1 \\
#'     \infty & \text{if } \xi \geq 1
#'   \end{cases}
#' }{
#'   E(X) = \mu + \sigma \gamma  (if \xi = 0)
#'         \mu + \sigma (\Gamma(1-\xi) - 1) / \xi  (if \xi < 1)
#'         Inf  (if \xi >= 1)
#' }
#' where \eqn{\gamma \approx 0.5772} is the Euler-Mascheroni constant and
#' \eqn{\Gamma(\cdot)} is the gamma function.
#'
#' **Median**:
#' \deqn{
#'   \text{Median}(X) = \begin{cases}
#'     \mu - \sigma \log(\log 2) & \text{if } \xi = 0 \\
#'     \mu + \sigma \frac{(\log 2)^{-\xi} - 1}{\xi} & \text{if } \xi \neq 0
#'   \end{cases}
#' }{
#'   Median(X) = \mu - \sigma log(log 2)  (if \xi = 0)
#'               \mu + \sigma (log(2)^(-\xi) - 1) / \xi  (if \xi != 0)
#' }
#'
#' **Variance**:
#' \deqn{
#'   \text{Var}(X) = \begin{cases}
#'     \frac{\pi^2 \sigma^2}{6} & \text{if } \xi = 0 \\
#'     \frac{\sigma^2}{\xi^2} [\Gamma(1-2\xi) - \Gamma(1-\xi)^2] & \text{if } \xi < 0.5 \\
#'     \infty & \text{if } \xi \geq 0.5
#'   \end{cases}
#' }{
#'   Var(X) = \pi^2 \sigma^2 / 6  (if \xi = 0)
#'            \sigma^2 [\Gamma(1-2\xi) - \Gamma(1-\xi)^2] / \xi^2  (if \xi < 0.5)
#'            Inf  (if \xi >= 0.5)
#' }
#'
#' **Probability density function (p.d.f)**:
#'
#' For \eqn{\xi = 0} (Gumbel):
#' \deqn{
#'   f(x) = \frac{1}{\sigma} \exp\left(-\frac{x-\mu}{\sigma}\right)
#'          \exp\left[-\exp\left(-\frac{x-\mu}{\sigma}\right)\right]
#' }{
#'   f(x) = (1/\sigma) exp(-(x-\mu)/\sigma) exp(-exp(-(x-\mu)/\sigma))
#' }
#'
#' For \eqn{\xi \neq 0}:
#' \deqn{
#'   f(x) = \frac{1}{\sigma} \left[1 + \xi\left(\frac{x-\mu}{\sigma}\right)\right]^{-1/\xi-1}
#'          \exp\left\{-\left[1 + \xi\left(\frac{x-\mu}{\sigma}\right)\right]^{-1/\xi}\right\}
#' }{
#'   f(x) = (1/\sigma) [1 + \xi(x-\mu)/\sigma]^(-1/\xi-1) exp(-[1 + \xi(x-\mu)/\sigma]^(-1/\xi))
#' }
#' where \eqn{1 + \xi(x-\mu)/\sigma > 0}.
#'
#' **Cumulative distribution function (c.d.f)**:
#'
#' For \eqn{\xi = 0} (Gumbel):
#' \deqn{
#'   F(x) = \exp\left[-\exp\left(-\frac{x-\mu}{\sigma}\right)\right]
#' }{
#'   F(x) = exp(-exp(-(x-\mu)/\sigma))
#' }
#'
#' For \eqn{\xi \neq 0}:
#' \deqn{
#'   F(x) = \exp\left\{-\left[1+\xi\left(\frac{x-\mu}{\sigma}\right)\right]^{-1/\xi}\right\}
#' }{
#'   F(x) = exp(-[1 + \xi(x-\mu)/\sigma]^(-1/\xi))
#' }
#' where \eqn{1 + \xi(x-\mu)/\sigma > 0}.
#'
#' **Quantile function**:
#'
#' For \eqn{\xi = 0} (Gumbel):
#' \deqn{
#'   Q(p) = \mu - \sigma \log(-\log p)
#' }{
#'   Q(p) = \mu - \sigma log(-log p)
#' }
#'
#' For \eqn{\xi \neq 0}:
#' \deqn{
#'   Q(p) = \mu + \frac{\sigma}{\xi}\left[(-\log p)^{-\xi} - 1\right]
#' }{
#'   Q(p) = \mu + \sigma [(-log p)^(-\xi) - 1] / \xi
#' }
#'
#' @references Jenkinson, A. F. (1955) The frequency distribution of the annual
#' maximum (or minimum) of meteorological elements. \emph{Quart. J. R. Met. Soc.},
#' \bold{81}, 158–171.
#' 
#' @seealso [evd::dgev()]
#' 
#' @examples
#' # Create GEV distributions with different shape parameters
#' 
#' # Gumbel distribution (shape = 0)
#' gumbel <- dist_gev(location = 0, scale = 1, shape = 0)
#' 
#' # Frechet distribution (shape > 0, heavy-tailed)
#' frechet <- dist_gev(location = 0, scale = 1, shape = 0.3)
#' 
#' # Reverse Weibull distribution (shape < 0, bounded above)
#' weibull <- dist_gev(location = 0, scale = 1, shape = -0.2)
#' 
#' dist <- c(gumbel, frechet, weibull)
#' dist
#' 
#' # Statistical properties
#' mean(dist)
#' median(dist)
#' variance(dist)
#' 
#' # Generate random samples
#' generate(dist, 10)
#' 
#' # Evaluate density
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#' 
#' # Evaluate cumulative distribution
#' cdf(dist, 4)
#' 
#' # Calculate quantiles
#' quantile(dist, 0.95)
#' 
#' @export
dist_gev <- function(location, scale, shape) {
  location <- vctrs::vec_cast(unname(location), double())
  shape <- vctrs::vec_cast(unname(shape), double())
  scale <- vctrs::vec_cast(unname(scale), double())
  if (any(scale <= 0)) {
    stop("The scale parameter of a GEV distribution must be strictly positive")
  }
  distributional::new_dist(location = location, scale = scale, shape = shape, class = "dist_gev")
}

#' @export
format.dist_gev <- function(x, digits = 2, ...) {
  sprintf(
    "GEV(%s, %s, %s)",
    format(x[["location"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...),
    format(x[["shape"]], digits = digits, ...)
  )
}

#' @exportS3Method distributional::log_density
log_density.dist_gev <- function(x, at, ...) {
  z <- (at - x[["location"]]) / x[["scale"]]
  if (x[["shape"]] == 0) {
    pdf <- -z - exp(-z)
  } else {
    xx <- 1 + x[["shape"]] * z
    xx[xx <= 0] <- NA_real_
    pdf <- -xx^(-1 / x[["shape"]]) - (1 / x[["shape"]] + 1) * log(xx)
    pdf[is.na(pdf)] <- -Inf
  }
  pdf - log(x[["scale"]])
}

#' @exportS3Method stats::density
density.dist_gev <- function(x, at, ...) {
  exp(log_density.dist_gev(x, at, ...))
}

#' @exportS3Method distributional::cdf
cdf.dist_gev <- function(x, q, ...) {
  z <- (q - x[["location"]]) / x[["scale"]]
  if (x[["shape"]] == 0) {
    exp(-exp(-z))
  } else {
    exp(-pmax(1 + x[["shape"]] * z, 0)^(-1 / x[["shape"]]))
  }
}

#' @exportS3Method stats::quantile
quantile.dist_gev <- function(x, p, ...) {
  if (x[["shape"]] == 0) {
    x[["location"]] - x[["scale"]] * log(-log(p))
  } else {
    x[["location"]] + x[["scale"]] * ((-log(p))^(-x[["shape"]]) - 1) / x[["shape"]]
  }
}

#' @exportS3Method distributional::generate
generate.dist_gev <- function(x, times, ...) {
  z <- stats::rexp(times)
  if (x[["shape"]] == 0) {
    x[["location"]] - x[["scale"]] * log(z)
  } else {
    x[["location"]] + x[["scale"]] * (z^(-x[["shape"]]) - 1) / x[["shape"]]
  }
}

#' @export
mean.dist_gev <- function(x, ...) {
  if (x[["shape"]] == 0) {
    x[["location"]] + x[["scale"]] * 0.57721566490153286
  } else if (x[["shape"]] < 1) {
    x[["location"]] + x[["scale"]] * (gamma(1 - x[["shape"]]) - 1) / x[["shape"]]
  } else {
    Inf
  }
}

#' @exportS3Method stats::median
median.dist_gev <- function(x, ...) {
  if (x[["shape"]] == 0) {
    x[["location"]] - x[["scale"]] * log(log(2))
  } else {
    x[["location"]] + x[["scale"]] * (log(2)^(-x[["shape"]]) - 1) / x[["shape"]]
  }
}

#' @exportS3Method distributional::covariance
covariance.dist_gev <- function(x, ...) {
  if (x[["shape"]] == 0) {
    x[["scale"]]^2 * pi^2 / 6
  } else if (x[["shape"]] < 0.5) {
    g2 <- gamma(1 - 2 * x[["shape"]])
    g1 <- gamma(1 - x[["shape"]])
    x[["scale"]]^2 * (g2 - g1^2) / x[["shape"]]^2
  } else {
    Inf
  }
}
