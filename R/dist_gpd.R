#' The Generalized Pareto Distribution
#'
#' The GPD distribution function with parameters \eqn{\code{location} = a},
#' \eqn{\code{scale} = b} and \eqn{\code{shape} = s} is
#'
#' \deqn{F(x) = 1 - \left(1+s(x-a)/b\right)^{-1/s}}
#'
#' for \eqn{1+s(x-a)/b > 0}, where \eqn{b > 0}. If \eqn{s = 0} the distribution
#' is defined by continuity, giving
#'
#' \deqn{F(x) = 1 - \exp\left(-\frac{x-a}{b}\right)}
#'
#' The support of the distribution is \eqn{x \geq a} if \eqn{s \geq 0}, and
#' \eqn{a \leq x \leq a -b/s} if \eqn{s < 0}.
#'
#' The Pickands–Balkema–De Haan theorem states that for a large class of
#' distributions, the tail (above some threshold) can be approximated by a GPD.
#'
#' @param location the location parameter \eqn{a} of the GPD distribution.
#' @param scale the scale parameter \eqn{b} of the GPD distribution.
#' @param shape the shape parameter \eqn{s} of the GPD distribution.
#' @seealso \code{\link[evd]{gpd}}
#' @examples
#' dist <- dist_gpd(location = 0, scale = 1, shape = 0)
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
