#' The generalised g-and-h Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The generalised g-and-h distribution is a flexible distribution used to model univariate data, similar to the g-k distribution.
#' It is known for its ability to handle skewness and heavy-tailed behavior.
#'
#' @inheritParams gk::dgh
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a g-and-h random variable with parameters
#'   `A`, `B`, `g`, `h`, and `c`.
#'
#'   **Support**: \eqn{(-\infty, \infty)}
#'
#'   **Mean**: Not available in closed form.
#'
#'   **Variance**: Not available in closed form.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   The g-and-h distribution does not have a closed-form expression for its density. Instead,
#'   it is defined through its quantile function:
#'
#'   \deqn{
#'     Q(u) = A + B \left( 1 + c \frac{1 - \exp(-gz(u))}{1 + \exp(-gz(u))} \right) \exp(h z(u)^2/2) z(u)
#'   }{
#'     Q(u) = A + B * (1 + c * ((1 - exp(-g * z(u))) / (1 + exp(-g * z(u))))) * exp(h * z(u)^2/2) * z(u)
#'   }
#'
#'   where \eqn{z(u) = \Phi^{-1}(u)}
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cumulative distribution function is typically evaluated numerically due to the lack
#'   of a closed-form expression.
#'
#' @seealso [gk::dgh], [distributional::dist_gk]
#'
#' @examples
#' dist <- dist_gh(A = 0, B = 1, g = 0, h = 0.5)
#' dist
#'
#' @examplesIf requireNamespace("gk", quietly = TRUE)
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
#' @name dist_gh
#' @export
dist_gh <- function(A, B, g, h, c = 0.8){
  A <- vec_cast(A, double())
  B <- vec_cast(B, double())
  g <- vec_cast(g, double())
  h <- vec_cast(h, double())
  c <- vec_cast(c, double())
  if(any(B <= 0)){
    abort("The B parameter (scale) of the g-and-h distribution must be strictly positive.")
  }
  new_dist(A = A, B = B, g = g, h = h, c = c, class = "dist_gh")
}

#' @export
format.dist_gh <- function(x, digits = 2, ...){
  sprintf(
    "gh(A = %s, B = %s, g = %s, h = %s%s)",
    format(x[["A"]], digits = digits, ...),
    format(x[["B"]], digits = digits, ...),
    format(x[["g"]], digits = digits, ...),
    format(x[["h"]], digits = digits, ...),
    if (x[["c"]]==0.8) "" else paste0(", c = ", format(x[["c"]], digits = digits, ...))
  )
}

#' @export
density.dist_gh <- function(x, at, ...){
  require_package("gk")
  gk::dgh(at, x[["A"]], x[["B"]], x[["g"]], x[["h"]], x[["c"]])
}

#' @export
log_density.dist_gh <- function(x, at, ...){
  require_package("gk")
  gk::dgh(at, x[["A"]], x[["B"]], x[["g"]], x[["h"]], x[["c"]], log = TRUE)
}

#' @export
quantile.dist_gh <- function(x, p, ...){
  require_package("gk")
  gk::qgh(p, x[["A"]], x[["B"]], x[["g"]], x[["h"]], x[["c"]])
}

#' @export
cdf.dist_gh <- function(x, q, ...){
  require_package("gk")
  gk::pgh(q, x[["A"]], x[["B"]], x[["g"]], x[["h"]], x[["c"]])
}

#' @export
generate.dist_gh <- function(x, times, ...){
  require_package("gk")
  gk::rgh(times, x[["A"]], x[["B"]], x[["g"]], x[["h"]], x[["c"]])
}
