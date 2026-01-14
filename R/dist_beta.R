#' The Beta distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Beta distribution is a continuous probability distribution defined on the
#' interval \[0, 1\], commonly used to model probabilities and proportions.
#'
#' @param shape1,shape2 The non-negative shape parameters of the Beta distribution.
#'
#' @details
#'
#'   `r pkgdown_doc_link("dist_beta")`
#'
#'   In the following, let \eqn{X} be a Beta random variable with parameters
#'   `shape1` = \eqn{\alpha} and `shape2` = \eqn{\beta}.
#'
#'   **Support**: \eqn{x \in [0, 1]}
#'
#'   **Mean**: \eqn{\frac{\alpha}{\alpha + \beta}}
#'
#'   **Variance**: \eqn{\frac{\alpha\beta}{(\alpha + \beta)^2(\alpha + \beta + 1)}}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{x^{\alpha - 1}(1-x)^{\beta - 1}}{B(\alpha, \beta)} = 
#'     \frac{\Gamma(\alpha + \beta)}{\Gamma(\alpha)\Gamma(\beta)} x^{\alpha - 1}(1-x)^{\beta - 1}
#'   }{
#'     f(x) = x^(alpha - 1) * (1-x)^(beta - 1) / B(alpha, beta)
#'   }
#'
#'   where \eqn{B(\alpha, \beta) = \frac{\Gamma(\alpha)\Gamma(\beta)}{\Gamma(\alpha + \beta)}}
#'   is the Beta function.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = I_x(alpha, beta) = \frac{B(x; \alpha, \beta)}{B(\alpha, \beta)}
#'   }{
#'     F(x) = I_x(\alpha, \beta)
#'   }
#'
#'   where \eqn{I_x(\alpha, \beta)} is the regularized incomplete beta function and
#'   \eqn{B(x; \alpha, \beta) = \int_0^x t^{\alpha-1}(1-t)^{\beta-1} dt}.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   The moment generating function does not have a simple closed form, but the
#'   moments can be calculated as:
#'
#'   \deqn{
#'     E(X^k) = \prod_{r=0}^{k-1} \frac{\alpha + r}{\alpha + \beta + r}
#'   }{
#'     E(X^k) = prod((alpha + r) / (alpha + beta + r)) for r = 0, ..., k-1
#'   }
#'
#' @seealso [stats::Beta]
#'
#' @examples
#' dist <- dist_beta(shape1 = c(0.5, 5, 1, 2, 2), shape2 = c(0.5, 1, 3, 2, 5))
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
#' @name dist_beta
#' @export
dist_beta <- function(shape1, shape2){
  shape1 <- vec_cast(shape1, double())
  shape2 <- vec_cast(shape2, double())
  if(any((shape1 < 0) | shape2 < 0)){
    abort("Shape parameters of a Beta distribution must be non-negative.")
  }
  new_dist(shape1 = shape1, shape2 = shape2, class = "dist_beta")
}

#' @export
format.dist_beta <- function(x, digits = 2, ...){
  sprintf(
    "Beta(%s, %s)",
    format(x[["shape1"]], digits = digits, ...),
    format(x[["shape2"]], digits = digits, ...)
  )
}

#' @export
density.dist_beta <- function(x, at, ...){
  stats::dbeta(at, x[["shape1"]], x[["shape2"]])
}

#' @export
log_density.dist_beta <- function(x, at, ...){
  stats::dbeta(at, x[["shape1"]], x[["shape2"]], log = TRUE)
}

#' @export
quantile.dist_beta <- function(x, p, ...){
  stats::qbeta(p, x[["shape1"]], x[["shape2"]])
}

#' @export
cdf.dist_beta <- function(x, q, ...){
  stats::pbeta(q, x[["shape1"]], x[["shape2"]])
}

#' @export
generate.dist_beta <- function(x, times, ...){
  stats::rbeta(times, x[["shape1"]], x[["shape2"]])
}

#' @export
mean.dist_beta <- function(x, ...){
  x[["shape1"]]/(x[["shape1"]] + x[["shape2"]])
}

#' @export
covariance.dist_beta <- function(x, ...){
  a <- x[["shape1"]]
  b <- x[["shape2"]]
  a*b/((a+b)^2*(a+b+1))
}

#' @export
skewness.dist_beta <- function(x, ...) {
  a <- x[["shape1"]]
  b <- x[["shape2"]]
  2 * (b - a) * sqrt(a + b + 1) / (a + b + 2) * sqrt(a * b)
}

#' @export
kurtosis.dist_beta <- function(x, ...) {
  a <- x[["shape1"]]
  b <- x[["shape2"]]
  num <- 6 * ((a - b)^2 * (a + b + 1) - (a * b) * (a + b + 2))
  denom <- a * b * (a + b + 2) * (a + b + 3)
  num / denom
}

#' @export
has_symmetry.dist_beta <- function(x, ...) {
  a <- x[["shape1"]]
  b <- x[["shape2"]]
  a == b
}