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
    vapply(x[["location"]], format, character(1L), digits = digits, ...),
    vapply(x[["scale"]], format, character(1L), digits = digits, ...),
    vapply(x[["shape"]], format, character(1L), digits = digits, ...)
  )
}

#' @exportS3Method distributional::log_density
log_density.dist_gev <- function(x, at, ...) {
  z <- (at - x[["location"]]) / x[["scale"]]
  shape <- x[["shape"]]
  zero <- shape == 0
  # Avoid dividing by zero in the shape != 0 branch
  sh <- ifelse(zero, 1, shape)
  xx <- 1 + sh * z
  xx[!is.na(xx) & xx <= 0] <- NA_real_
  pdf <- -xx^(-1 / sh) - (1 / sh + 1) * log(xx)
  pdf[is.na(pdf)] <- -Inf
  pdf <- ifelse(zero, -z - exp(-z), pdf)
  pdf - log(x[["scale"]])
}

#' @exportS3Method stats::density
density.dist_gev <- function(x, at, ...) {
  exp(log_density.dist_gev(x, at, ...))
}

#' @exportS3Method distributional::cdf
cdf.dist_gev <- function(x, q, ..., tail = "lower") {
  z <- (q - x[["location"]]) / x[["scale"]]
  shape <- x[["shape"]]
  zero <- shape == 0
  sh <- ifelse(zero, 1, shape)
  t <- ifelse(zero, exp(-z), pmax(1 + sh * z, 0)^(-1 / sh))
  if (tail == "lower") exp(-t) else -expm1(-t)
}

#' @exportS3Method stats::quantile
quantile.dist_gev <- function(x, p, ..., tail = "lower") {
  # -log(F), where F is the lower tail probability
  t <- if (tail == "lower") -log(p) else -log1p(-p)
  gev_quantile_t(x[["location"]], x[["scale"]], x[["shape"]], t)
}

# Quantile as a function of t = -log(F), selecting the Gumbel form per element
gev_quantile_t <- function(location, scale, shape, t) {
  zero <- shape == 0
  sh <- ifelse(zero, 1, shape)
  ifelse(
    zero,
    location - scale * log(t),
    location + scale * (t^(-sh) - 1) / sh
  )
}

#' @exportS3Method distributional::generate
generate.dist_gev <- function(x, times, ...) {
  z <- stats::rexp(sum(times))
  vec_chop(
    gev_quantile_t(
      rep(x[["location"]], times), rep(x[["scale"]], times),
      rep(x[["shape"]], times), z
    ),
    sizes = times
  )
}

#' @export
support.dist_gev <- function(x, ...) {
  location <- x[["location"]]
  scale <- x[["scale"]]
  shape <- x[["shape"]]
  n <- vec_size(x)
  pos <- !is.na(shape) & shape > 0
  neg <- !is.na(shape) & shape < 0
  # shape > 0: (location - scale/shape, Inf), the density vanishes at the bound
  # shape < 0: (-Inf, location - scale/shape], the density at the bound is
  # positive (shape = -1) or infinite (shape < -1) when shape <= -1
  bound <- location - scale / ifelse(pos | neg, shape, 1)
  lower <- ifelse(pos, bound, -Inf)
  upper <- ifelse(neg, bound, Inf)
  closed_upper <- neg & shape <= -1
  new_support_region(
    rep_len(list(numeric()), n),
    .mapply(c, list(lower, upper), NULL),
    lapply(closed_upper, function(cl) c(FALSE, cl))
  )
}

#' @export
mean.dist_gev <- function(x, ...) {
  shape <- x[["shape"]]
  zero <- shape == 0
  finite <- shape < 1
  # Avoid dividing by zero and evaluating gamma() at its poles
  sh <- ifelse(zero | !finite, 0.5, shape)
  ifelse(
    zero,
    x[["location"]] + x[["scale"]] * 0.57721566490153286,
    ifelse(
      finite,
      x[["location"]] + x[["scale"]] * (gamma(1 - sh) - 1) / sh,
      Inf
    )
  )
}

#' @exportS3Method stats::median
median.dist_gev <- function(x, ...) {
  shape <- x[["shape"]]
  zero <- shape == 0
  sh <- ifelse(zero, 1, shape)
  ifelse(
    zero,
    x[["location"]] - x[["scale"]] * log(log(2)),
    x[["location"]] + x[["scale"]] * (log(2)^(-sh) - 1) / sh
  )
}

#' @exportS3Method distributional::covariance
covariance.dist_gev <- function(x, ...) {
  shape <- x[["shape"]]
  zero <- shape == 0
  finite <- shape < 0.5
  sh <- ifelse(zero | !finite, 0.25, shape)
  g2 <- gamma(1 - 2 * sh)
  g1 <- gamma(1 - sh)
  ifelse(
    zero,
    x[["scale"]]^2 * pi^2 / 6,
    ifelse(finite, x[["scale"]]^2 * (g2 - g1^2) / sh^2, Inf)
  )
}
