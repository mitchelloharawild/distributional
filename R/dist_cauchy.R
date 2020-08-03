#' The Cauchy distribution
#'
#' \lifecycle{maturing}
#'
#' The Cauchy distribution is the student's t distribution with one degree of
#' freedom. The Cauchy distribution does not have a well defined mean or
#' variance. Cauchy distributions often appear as priors in Bayesian contexts
#' due to their heavy tails.
#'
#' @inheritParams stats::dcauchy
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Cauchy variable with mean
#'   `location =` \eqn{x_0} and `scale` = \eqn{\gamma}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers
#'
#'   **Mean**: Undefined.
#'
#'   **Variance**: Undefined.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{1}{\pi \gamma \left[1 + \left(\frac{x - x_0}{\gamma} \right)^2 \right]}
#'   }{
#'     f(x) = 1 / (\pi \gamma (1 + ((x - x_0) / \gamma)^2)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(t) = \frac{1}{\pi} \arctan \left( \frac{t - x_0}{\gamma} \right) +
#'       \frac{1}{2}
#'   }{
#'     F(t) = arctan((t - x_0) / \gamma) / \pi + 1/2
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   Does not exist.
#'
#' @seealso [stats::Cauchy]
#'
#' @examples
#' dist <- dist_cauchy(location = c(0, 0, 0, -2), scale = c(0.5, 1, 2, 1))
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
#' @name dist_cauchy
#' @export
dist_cauchy <- function(location, scale){
  location <- vec_cast(location, double())
  scale <- vec_cast(scale, double())
  if(any(scale[!is.na(scale)] <= 0)){
    abort("The scale parameter of a Cauchy distribution must strictly positive.")
  }
  new_dist(location = location, scale = scale, class = "dist_cauchy")
}

#' @export
print.dist_cauchy <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_cauchy <- function(x, digits = 2, ...){
  sprintf(
    "Cauchy(%s, %s)",
    format(x[["location"]], digits = digits, ...),
    format(x[["scale"]], digits = digits, ...)
  )
}

#' @export
density.dist_cauchy <- function(x, at, ...){
  stats::dcauchy(at, x[["location"]], x[["scale"]])
}

#' @export
log_density.dist_cauchy <- function(x, at, ...){
  stats::dcauchy(at, x[["location"]], x[["scale"]], log = TRUE)
}

#' @export
quantile.dist_cauchy <- function(x, p, ...){
  stats::qcauchy(p, x[["location"]], x[["scale"]])
}

#' @export
cdf.dist_cauchy <- function(x, q, ...){
  stats::pcauchy(q, x[["location"]], x[["scale"]])
}

#' @export
generate.dist_cauchy <- function(x, times, ...){
  stats::rcauchy(times, x[["location"]], x[["scale"]])
}

#' @export
mean.dist_cauchy <- function(x, ...){
  NA_real_
}

#' @export
variance.dist_cauchy <- function(x, ...){
  NA_real_
}

#' @export
skewness.dist_cauchy <- function(x, ...){
  NA_real_
}

#' @export
kurtosis.dist_cauchy <- function(x, ...){
  NA_real_
}
