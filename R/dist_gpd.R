#' The Generalized Pareto Distribution
#'
#' @description
#' The GPD distribution is commonly used to model the tails of distributions,
#' particularly in extreme value theory.
#'
#' The Pickands–Balkema–De Haan theorem states that for a large class of
#' distributions, the tail (above some threshold) can be approximated by a GPD.
#'
#' @param location the location parameter \eqn{a} of the GPD distribution.
#' @param scale the scale parameter \eqn{b} of the GPD distribution.
#' @param shape the shape parameter \eqn{s} of the GPD distribution.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_gpd")`
#'
#' In the following, let \eqn{X} be a Generalized Pareto random variable with
#' parameters `location` = \eqn{a}, `scale` = \eqn{b > 0}, and
#' `shape` = \eqn{s}.
#'
#' **Support**:
#' \eqn{x \ge a} if \eqn{s \ge 0},
#' \eqn{a \le x \le a - b/s} if \eqn{s < 0}
#'
#' **Mean**:
#' \deqn{
#'   E(X) = a + \frac{b}{1 - s} \quad \textrm{for } s < 1
#' }{
#'   E(X) = a + b/(1 - s) for s < 1
#' }
#' \eqn{E(X) = \infty} for \eqn{s \ge 1}
#'
#' **Variance**:
#' \deqn{
#'   \textrm{Var}(X) = \frac{b^2}{(1-s)^2(1-2s)} \quad \textrm{for } s < 0.5
#' }{
#'   Var(X) = b^2 / ((1-s)^2 (1-2s)) for s < 0.5
#' }
#' \eqn{\textrm{Var}(X) = \infty} for \eqn{s \ge 0.5}
#'
#' **Probability density function (p.d.f)**:
#'
#' For \eqn{s = 0}:
#' \deqn{
#'   f(x) = \frac{1}{b}\exp\left(-\frac{x-a}{b}\right) \quad \textrm{for } x \ge a
#' }{
#'   f(x) = (1/b) exp(-(x-a)/b) for x >= a
#' }
#'
#' For \eqn{s \ne 0}:
#' \deqn{
#'   f(x) = \frac{1}{b}\left(1 + s\frac{x-a}{b}\right)^{-1/s - 1}
#' }{
#'   f(x) = (1/b)(1 + s(x-a)/b)^(-1/s - 1)
#' }
#' where \eqn{1 + s(x-a)/b > 0}
#'
#' **Cumulative distribution function (c.d.f)**:
#'
#' For \eqn{s = 0}:
#' \deqn{
#'   F(x) = 1 - \exp\left(-\frac{x-a}{b}\right) \quad \textrm{for } x \ge a
#' }{
#'   F(x) = 1 - exp(-(x-a)/b) for x >= a
#' }
#'
#' For \eqn{s \ne 0}:
#' \deqn{
#'   F(x) = 1 - \left(1 + s\frac{x-a}{b}\right)^{-1/s}
#' }{
#'   F(x) = 1 - (1 + s(x-a)/b)^(-1/s)
#' }
#' where \eqn{1 + s(x-a)/b > 0}
#'
#' **Quantile function**:
#'
#' For \eqn{s = 0}:
#' \deqn{
#'   Q(p) = a - b\log(1-p)
#' }{
#'   Q(p) = a - b log(1-p)
#' }
#'
#' For \eqn{s \ne 0}:
#' \deqn{
#'   Q(p) = a + \frac{b}{s}\left[(1-p)^{-s} - 1\right]
#' }{
#'   Q(p) = a + (b/s)[(1-p)^(-s) - 1]
#' }
#'
#' **Median**:
#'
#' For \eqn{s = 0}:
#' \deqn{
#'   \textrm{Median}(X) = a + b\log(2)
#' }{
#'   Median(X) = a + b log(2)
#' }
#'
#' For \eqn{s \ne 0}:
#' \deqn{
#'   \textrm{Median}(X) = a + \frac{b}{s}\left(2^s - 1\right)
#' }{
#'   Median(X) = a + (b/s)(2^s - 1)
#' }
#'
#' **Skewness and Kurtosis**: No closed-form expressions; approximated numerically.
#'
#' @seealso [evd::dgpd()]
#' @examples
#' dist <- dist_gpd(location = 0, scale = 1, shape = 0)
#'
#' dist
#' mean(dist)
#' variance(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#' @export
dist_gpd <- function(location, scale, shape) {
  location <- vctrs::vec_cast(unname(location), double())
  shape <- vctrs::vec_cast(unname(shape), double())
  scale <- vctrs::vec_cast(unname(scale), double())
  if (any(scale <= 0)) {
    stop("The scale parameter of a GPD distribution must be strictly positive")
  }
  distributional::new_dist(location = location, scale = scale, shape = shape, class = "dist_gpd")
}

#' @export
format.dist_gpd <- function(x, digits = 2, ...) {
  sprintf(
    "GPD(%s, %s, %s)",
    format(x[["location"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...),
    format(x[["shape"]], digits = digits, ...)
  )
}

#' @exportS3Method distributional::log_density
log_density.dist_gpd <- function(x, at, ...) {
  z <- (at - x[["location"]]) / x[["scale"]]
  if (x[["shape"]] == 0) {
    pdf <- -z
  } else {
    xx <- 1 + x[["shape"]] * z
    xx[xx <= 0] <- NA_real_
    pdf <- -(1 / x[["shape"]] + 1) * log(xx)
    pdf[is.na(pdf)] <- -Inf
  }
  if (x[["shape"]] >= 0) {
    pdf[z < 0] <- -Inf
  } else {
    pdf[z < 0 | z > -1 / x[["shape"]]] <- -Inf
  }
  pdf - log(x[["scale"]])
}

#' @exportS3Method stats::density
density.dist_gpd <- function(x, at, ...) {
  exp(log_density.dist_gpd(x, at, ...))
}

#' @exportS3Method distributional::cdf
cdf.dist_gpd <- function(x, q, ...) {
  z <- pmax(q - x[["location"]], 0) / x[["scale"]]
  if (x[["shape"]] == 0) {
    1 - exp(-z)
  } else {
    1 - pmax(1 + x[["shape"]] * z, 0)^(-1 / x[["shape"]])
  }
}

#' @exportS3Method stats::quantile
quantile.dist_gpd <- function(x, p, ...) {
  if (x[["shape"]] == 0) {
    x[["location"]] - x[["scale"]] * log(1 - p)
  } else {
    x[["location"]] + x[["scale"]] * ((1 - p)^(-x[["shape"]]) - 1) / x[["shape"]]
  }
}

#' @exportS3Method distributional::generate
generate.dist_gpd <- function(x, times, ...) {
  if (x[["shape"]] == 0) {
    x[["location"]] + x[["scale"]] * stats::rexp(times)
  } else {
    quantile(x, stats::runif(times))
  }
}

#' @export
mean.dist_gpd <- function(x, ...) {
  if (x[["shape"]] < 1) {
    x[["location"]] + x[["scale"]] / (1 - x[["shape"]])
  } else {
    Inf
  }
}

#' @exportS3Method stats::median
median.dist_gpd <- function(x, ...) {
  if (x[["shape"]] == 0) {
    x[["location"]] - x[["scale"]] * log(0.5)
  } else {
    x[["location"]] + x[["scale"]] * (2^x[["shape"]] - 1) / x[["shape"]]
  }
}

#' @exportS3Method distributional::covariance
covariance.dist_gpd <- function(x, ...) {
  if (x[["shape"]] < 0.5) {
    x[["scale"]]^2 / (1 - x[["shape"]])^2 / (1 - 2 * x[["shape"]])
  } else {
    Inf
  }
}

#' @export
support.dist_gpd <- function(x, ...) {
  shape <- x[["shape"]]
  location <- x[["location"]]
  scale <- x[["scale"]]
  
  # Determine limits based on shape parameter
  if (shape >= 0) {
    # Support: [location, Inf)
    lims <- list(c(location, Inf))
    closed <- list(c(TRUE, FALSE))
  } else {
    # Support: [location, location - scale/shape]
    upper <- location - scale / shape
    lims <- list(c(location, upper))
    closed <- list(c(TRUE, TRUE))
  }
  
  x_proto <- list(vctrs::vec_init(numeric(), n = 0L))
  
  new_support_region(x_proto, lims, closed)
}