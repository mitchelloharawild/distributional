#' The Pareto Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Pareto distribution is a power-law probability distribution commonly used
#' in actuarial science to model loss severity and in economics to model income
#' distributions and firm sizes.
#'
#' @inheritParams actuar::dpareto
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_pareto")`
#'
#'   In the following, let \eqn{X} be a Pareto random variable with parameters
#'   `shape` = \eqn{\alpha} and `scale` = \eqn{\theta}.
#'
#'   **Support**: \eqn{(0, \infty)}{(0, Inf)}
#'
#'   **Mean**: \eqn{\frac{\theta}{\alpha - 1}}{\theta / (\alpha - 1)} for \eqn{\alpha > 1}, 
#'   undefined otherwise
#'
#'   **Variance**: \eqn{\frac{\alpha\theta^2}{(\alpha - 1)^2(\alpha - 2)}}{\alpha\theta^2 / ((\alpha - 1)^2(\alpha - 2))} 
#'   for \eqn{\alpha > 2}, undefined otherwise
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\alpha\theta^\alpha}{(x + \theta)^{\alpha + 1}}
#'   }{
#'     f(x) = \alpha\theta^\alpha / (x + \theta)^(\alpha + 1)
#'   }
#'
#'   for \eqn{x > 0}, \eqn{\alpha > 0} and \eqn{\theta > 0}.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = 1 - \left(\frac{\theta}{x + \theta}\right)^\alpha
#'   }{
#'     F(x) = 1 - (\theta / (x + \theta))^\alpha
#'   }
#'
#'   for \eqn{x > 0}.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   Does not exist in closed form, but the \eqn{k}th raw moment \eqn{E[X^k]} exists
#'   for \eqn{-1 < k < \alpha}.
#'
#' @note
#' There are many different definitions of the Pareto distribution in the
#' literature; see Arnold (2015) or Kleiber and Kotz (2003). This implementation
#' uses the Pareto distribution without a location parameter as described in [actuar::Pareto].
#'
#' @references
#' Kleiber, C. and Kotz, S. (2003), *Statistical Size Distributions in Economics
#' and Actuarial Sciences*, Wiley.
#'
#' Klugman, S. A., Panjer, H. H. and Willmot, G. E. (2012), *Loss Models, From
#' Data to Decisions, Fourth Edition*, Wiley.
#'
#' @seealso [actuar::Pareto]
#'
#' @examples
#' dist <- dist_pareto(shape = c(10, 3, 2, 1), scale = rep(1, 4))
#' dist
#'
#' @examplesIf requireNamespace("actuar", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_pareto
#' @export
dist_pareto <- function(shape, scale){
  shape <- vec_cast(shape, double())
  scale <- vec_cast(scale, double())
  if(any(shape < 0)){
    abort("The shape parameter of a Pareto distribution must be non-negative.")
  }
  if(any(scale <= 0)){
    abort("The scale parameter of a Pareto distribution must be strictly positive.")
  }
  new_dist(shape = shape, scale = scale, class = "dist_pareto")
}

#' @export
format.dist_pareto <- function(x, digits = 2, ...){
  sprintf(
    "Pareto(%s, %s)",
    format(x[["shape"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...)
  )
}

#' @export
density.dist_pareto <- function(x, at, ...){
  require_package("actuar")
  actuar::dpareto(at, x[["shape"]], x[["scale"]])
}

#' @export
log_density.dist_pareto <- function(x, at, ...){
  require_package("actuar")
  actuar::dpareto(at, x[["shape"]], x[["scale"]], log = TRUE)
}

#' @export
quantile.dist_pareto <- function(x, p, ...){
  require_package("actuar")
  actuar::qpareto(p, x[["shape"]], x[["scale"]])
}

#' @export
cdf.dist_pareto <- function(x, q, ...){
  require_package("actuar")
  actuar::ppareto(q, x[["shape"]], x[["scale"]])
}

#' @export
generate.dist_pareto <- function(x, times, ...){
  require_package("actuar")
  actuar::rpareto(times, x[["shape"]], x[["scale"]])
}

#' @export
mean.dist_pareto <- function(x, ...){
  actuar::mpareto(1, x[["shape"]], x[["scale"]])
}

#' @export
covariance.dist_pareto <- function(x, ...){
  actuar::mpareto(2, x[["shape"]], x[["scale"]]) - actuar::mpareto(1, x[["shape"]], x[["scale"]])^2
}
