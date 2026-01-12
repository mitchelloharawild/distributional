#' The Tweedie distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Tweedie distribution is a family of exponential dispersion models that
#' includes several important distributions as special cases. When the power
#' parameter \eqn{p = 0}, the Tweedie is a Normal distribution. When \eqn{p = 1},
#' it is a Poisson distribution. When \eqn{p = 2}, it is a Gamma distribution.
#' When \eqn{p = 3}, it is an Inverse Gaussian distribution. For \eqn{1 < p < 2},
#' the Tweedie is a compound Poisson-Gamma distribution, which is particularly
#' useful for modeling data with exact zeros and continuous positive values
#' (e.g., insurance claims, rainfall data).
#'
#' @param mu mean parameter (positive for p >= 2)
#' @param phi dispersion parameter (positive)
#' @param power power parameter, typically in (1, 2) for compound Poisson-Gamma
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{Y} be a Tweedie random variable
#'   with parameters
#'   `mu` = \eqn{\mu},
#'   `phi` = \eqn{\phi}, and
#'   `power` = \eqn{p}.
#'
#'   **Support**: 
#'   - \eqn{p = 0}: \eqn{y \in (-\infty, \infty)} (Normal)
#'   - \eqn{p = 1}: \eqn{y \in \{0, 1, 2, ...\}} (Poisson)
#'   - \eqn{1 < p < 2}: \eqn{y \in \{0\} \cup (0, \infty)} (Compound Poisson-Gamma)
#'   - \eqn{p = 2}: \eqn{y \in (0, \infty)} (Gamma)
#'   - \eqn{p = 3}: \eqn{y \in (0, \infty)} (Inverse Gaussian)
#'
#'   **Mean**: \eqn{\mu}
#'
#'   **Variance**: \eqn{\phi \mu^p}
#'
#'   **Probability density function**:
#'
#'   The Tweedie density does not have a closed form for \eqn{1 < p < 2}, but
#'   can be expressed as an infinite series or evaluated numerically.
#'
#' @examples
#' dist <- dist_tweedie(mu = c(1, 2, 3), phi = c(1, 1.5, 2), power = c(1, 1.5, 2))
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
#' 
#' support(dist)
#'
#' @name dist_tweedie
#' @export
dist_tweedie <- function(mu, phi, power){
  mu <- vec_cast(mu, double())
  phi <- vec_cast(phi, double())
  power <- vec_cast(power, double())
  
  if(any(mu[!is.na(mu)] < 0)){
    abort("The mu parameter of a Tweedie distribution must be non-negative.")
  }
  if(any(phi[!is.na(phi)] <= 0)){
    abort("The phi parameter of a Tweedie distribution must be strictly positive (non-negative).")
  }
  if(any(power[!is.na(power)] < 0)){
    abort("The power parameter must be non-negative (p=0 for Normal).")
  }
  if(any(power[!is.na(power)] > 0 & power[!is.na(power)] < 1)){
    abort("The power parameter must be 0, 1, or greater than 1 (no distributions exist for 0 < p < 1).")
  }
  
  new_dist(mu = mu, phi = phi, power = power, class = "dist_tweedie")
}

#' @export
format.dist_tweedie <- function(x, digits = 2, ...){
  sprintf(
    "Tweedie(%s, %s, %s)",
    format(x[["mu"]], digits = digits, ...),
    format(x[["phi"]], digits = digits, ...),
    format(x[["power"]], digits = digits, ...)
  )
}

#' @export
density.dist_tweedie <- function(x, at, ...){
  require_package("tweedie")
  tweedie::dtweedie(at, power = x[["power"]], mu = x[["mu"]], phi = x[["phi"]])
}

#' @export
log_density.dist_tweedie <- function(x, at, ...){
  require_package("tweedie")
  log(tweedie::dtweedie(at, power = x[["power"]], mu = x[["mu"]], phi = x[["phi"]]))
}

#' @export
quantile.dist_tweedie <- function(x, p, ...){
  require_package("tweedie")
  tweedie::qtweedie(p, power = x[["power"]], mu = x[["mu"]], phi = x[["phi"]])
}

#' @export
cdf.dist_tweedie <- function(x, q, ...){
  require_package("tweedie")
  tweedie::ptweedie(q, power = x[["power"]], mu = x[["mu"]], phi = x[["phi"]])
}

#' @export
generate.dist_tweedie <- function(x, times, ...){
  require_package("tweedie")
  tweedie::rtweedie(times, power = x[["power"]], mu = x[["mu"]], phi = x[["phi"]])
}

#' @export
mean.dist_tweedie <- function(x, ...){
  x[["mu"]]
}

#' @export
covariance.dist_tweedie <- function(x, ...){
  x[["phi"]] * x[["mu"]]^x[["power"]]
}

#' @export
support.dist_tweedie <- function(x, ...) {
  power <- x[["power"]]
  
  # Determine limits and closedness based on power parameter
  lims <- lapply(power, function(p) {
    if (near(p, 0)) {
      # Normal distribution: (-Inf, Inf)
      c(-Inf, Inf)
    } else if (near(p, 1)) {
      # Poisson distribution: {0, 1, 2, ...}
      c(0L, Inf)
    } else if (p > 1 && p < 2) {
      # Compound Poisson-Gamma: {0} ∪ (0, Inf)
      c(0, Inf)
    } else if (p >= 2) {
      # Gamma, Inverse Gaussian, etc: (0, Inf)
      c(0, Inf)
    } else {
      # Should not reach here due to validation
      c(NA_real_, NA_real_)
    }
  })
  
  closed <- lapply(power, function(p) {
    if (near(p, 0)) {
      # Normal: open on both ends
      c(FALSE, FALSE)
    } else if (near(p, 1)) {
      # Poisson: closed at 0, open at infinity
      c(TRUE, FALSE)
    } else if (p > 1 && p < 2) {
      # Compound Poisson-Gamma: closed at 0, open at infinity
      c(TRUE, FALSE)
    } else if (p >= 2) {
      # Gamma, Inverse Gaussian: open at 0, open at infinity
      c(FALSE, FALSE)
    } else {
      c(FALSE, FALSE)
    }
  })
  
  # Use integer prototype for Poisson (p=1), numeric otherwise
  x_proto <- lapply(power, function(p) {
    if (near(p, 1)) {
      vctrs::vec_init(integer(), n = 0L)
    } else {
      vctrs::vec_init(numeric(), n = 0L)
    }
  })
  
  new_support_region(x_proto, lims, closed)
}
