#' The Logistic distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A continuous distribution on the real line. For binary outcomes
#' the model given by \eqn{P(Y = 1 | X) = F(X \beta)} where
#' \eqn{F} is the Logistic [cdf()] is called *logistic regression*.
#'
#' @inheritParams stats::dlogis
#'
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Logistic random variable with
#'   `location` = \eqn{\mu} and `scale` = \eqn{s}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers
#'
#'   **Mean**: \eqn{\mu}
#'
#'   **Variance**: \eqn{s^2 \pi^2 / 3}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{e^{-(\frac{x - \mu}{s})}}{s [1 + \exp(-(\frac{x - \mu}{s})) ]^2}
#'   }{
#'     f(x) = e^(-(t - \mu) / s) / (s (1 + e^(-(t - \mu) / s))^2)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(t) = \frac{1}{1 + e^{-(\frac{t - \mu}{s})}}
#'   }{
#'     F(t) = 1 / (1 +  e^(-(t - \mu) / s))
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = e^{\mu t} \beta(1 - st, 1 + st)
#'   }{
#'     E(e^(tX)) = = e^(\mu t) \beta(1 - st, 1 + st)
#'   }
#'
#'   where \eqn{\beta(x, y)} is the Beta function.
#'
#' @seealso [stats::Logistic]
#'
#' @examples
#' dist <- dist_logistic(location = c(5,9,9,6,2), scale = c(2,3,4,2,1))
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
#' @name dist_logistic
#' @export
dist_logistic <- function(location, scale){
  location <- vec_cast(location, double())
  scale <- vec_cast(scale, double())
  new_dist(l = location, s = scale, class = "dist_logistic")
}

#' @export
format.dist_logistic <- function(x, digits = 2, ...){
  sprintf(
    "Logistic(%s, %s)",
    format(x[["l"]], digits = digits, ...),
    format(x[["s"]], digits = digits, ...)
  )
}

#' @export
density.dist_logistic <- function(x, at, ...){
  stats::dlogis(at, x[["l"]], x[["s"]])
}

#' @export
log_density.dist_logistic <- function(x, at, ...){
  stats::dlogis(at, x[["l"]], x[["s"]], log = TRUE)
}

#' @export
quantile.dist_logistic <- function(x, p, ...){
  stats::qlogis(p, x[["l"]], x[["s"]])
}

#' @export
cdf.dist_logistic <- function(x, q, ...){
  stats::plogis(q, x[["l"]], x[["s"]])
}

#' @export
generate.dist_logistic <- function(x, times, ...){
  stats::rlogis(times, x[["l"]], x[["s"]])
}

#' @export
mean.dist_logistic <- function(x, ...){
  x[["l"]]
}

#' @export
covariance.dist_logistic <- function(x, ...){
  (x[["s"]]*pi)^2/3
}

#' @export
skewness.dist_logistic <- function(x, ...) 0

#' @export
kurtosis.dist_logistic <- function(x, ...) 6 / 5
