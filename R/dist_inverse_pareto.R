#' The Inverse Pareto Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Inverse Pareto distribution is often used to model heavy-tailed data.
#'
#' @inheritParams actuar::dinvpareto
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be an Inverse Pareto random variable with parameters
#'   `shape` (\eqn{\alpha}) and `scale` (\eqn{\theta}).
#'
#'   **Support**: \eqn{(0, \infty)}
#'
#'   **Mean**: \eqn{\frac{\alpha \theta}{\alpha - 1}} for \eqn{\alpha > 1}
#'
#'   **Variance**: \eqn{\frac{\alpha \theta^2}{(\alpha - 1)^2 (\alpha - 2)}} for \eqn{\alpha > 2}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\alpha \theta^\alpha}{x^{\alpha + 1}}
#'   }{
#'     f(x) = \alpha \theta^\alpha / x^{\alpha + 1}
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = 1 - \left( \frac{\theta}{x} \right)^\alpha
#'   }{
#'     F(x) = 1 - (\theta / x)^\alpha
#'   }
#'
#' @seealso [actuar::dinvpareto]
#'
#' @examples
#' dist <- dist_inverse_pareto(shape = 2.5, scale = 3)
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_inverse_pareto
#' @export
dist_inverse_pareto <- function(shape, scale){
  shape <- vec_cast(shape, double())
  scale <- vec_cast(scale, double())
  if(any(shape <= 0)){
    abort("The shape parameter of an Inverse Pareto distribution must be strictly positive.")
  }
  if(any(scale <= 0)){
    abort("The scale parameter of an Inverse Pareto distribution must be strictly positive.")
  }
  new_dist(shape = shape, scale = scale, class = "dist_inverse_pareto")
}

#' @export
format.dist_inverse_pareto <- function(x, digits = 2, ...){
  sprintf(
    "InvPareto(shape = %s, scale = %s)",
    format(x[["shape"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...)
  )
}

#' @export
density.dist_inverse_pareto <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvpareto(at, x[["shape"]], x[["scale"]])
}

#' @export
log_density.dist_inverse_pareto <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvpareto(at, x[["shape"]], x[["scale"]], log = TRUE)
}

#' @export
quantile.dist_inverse_pareto <- function(x, p, ...){
  require_package("actuar")
  actuar::qinvpareto(p, x[["shape"]], x[["scale"]])
}

#' @export
cdf.dist_inverse_pareto <- function(x, q, ...){
  require_package("actuar")
  actuar::pinvpareto(q, x[["shape"]], x[["scale"]])
}

#' @export
generate.dist_inverse_pareto <- function(x, times, ...){
  require_package("actuar")
  actuar::rinvpareto(times, x[["shape"]], x[["scale"]])
}

#' @export
mean.dist_inverse_pareto <- function(x, ...){
  if (x[["shape"]] <= 1) return(NA_real_)
  x[["shape"]] * x[["scale"]] / (x[["shape"]] - 1)
}

#' @export
variance.dist_inverse_pareto <- function(x, ...){
  if (x[["shape"]] <= 2) return(NA_real_)
  x[["shape"]] * x[["scale"]]^2 / ((x[["shape"]] - 1)^2 * (x[["shape"]] - 2))
}
