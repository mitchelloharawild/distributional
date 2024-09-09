#' The Generalized Extreme Value Distribution
#'
#' The GEV distribution function with parameters \eqn{\code{location} = a},
#' \eqn{\code{scale} = b} and \eqn{\code{shape} = s} is
#'
#' \deqn{F(x) = \exp\left[-\{1+s(x-a)/b\}^{-1/s}\right]}
#'
#' for \eqn{1+s(x-a)/b > 0}, where \eqn{b > 0}. If \eqn{s = 0} the distribution
#' is defined by continuity, giving
#'
#' \deqn{F(x) = \exp\left[-\exp\left(-\frac{x-a}{b}\right)\right]}
#'
#' The support of the distribution is the real line if \eqn{s = 0},
#' \eqn{x \geq a - b/s} if \eqn{s \neq 0}, and
#' \eqn{x \leq a - b/s} if \eqn{s < 0}.
#'
#' The parametric form of the GEV encompasses that of the Gumbel, Frechet and
#' reverse Weibull distributions, which are obtained for \eqn{s = 0},
#' \eqn{s > 0} and \eqn{s < 0} respectively. It was first introduced by
#' Jenkinson (1955).
#'
#' @references Jenkinson, A. F. (1955) The frequency distribution of the annual
#' maximum (or minimum) of meteorological elements. \emph{Quart. J. R. Met. Soc.},
#' \bold{81}, 158–171.
#' @param location the location parameter \eqn{a} of the GEV distribution.
#' @param scale the scale parameter \eqn{b} of the GEV distribution.
#' @param shape the shape parameter \eqn{s} of the GEV distribution.
#' @seealso \code{\link[evd]{gev}}
#' @examples
#' dist <- dist_gev(location = 0, scale = 1, shape = 0)
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
