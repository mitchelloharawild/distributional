#' The g-and-k Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The g-and-k distribution is a flexible distribution often used to model univariate data.
#' It is particularly known for its ability to handle skewness and heavy-tailed behavior.
#'
#' @inheritParams gk::dgk
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a g-k random variable with parameters
#'   `A`, `B`, `g`, `k`, and `c`.
#'
#'   **Support**: \eqn{(-\infty, \infty)}
#'
#'   **Mean**: Not available in closed form.
#'
#'   **Variance**: Not available in closed form.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   The g-k distribution does not have a closed-form expression for its density. Instead,
#'   it is defined through its quantile function:
#'
#'   \deqn{
#'     Q(u) = A + B \left( 1 + c \frac{1 - \exp(-gz(u))}{1 + \exp(-gz(u))} \right) (1 + z(u)^2)^k z(u)
#'   }{
#'     Q(u) = A + B * (1 + c * ((1 - exp(-g * z(u))) / (1 + exp(-g * z(u))))) * (1 + z(u)^2)^k * z(u)
#'   }
#'
#'   where \eqn{z(u) = \Phi^{-1}(u)}, the standard normal quantile of u.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cumulative distribution function is typically evaluated numerically due to the lack
#'   of a closed-form expression.
#'
#' @seealso [gk::dgk], [distributional::dist_gh]
#'
#' @examples
#' dist <- dist_gk(A = 0, B = 1, g = 0, k = 0.5)
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
#' @name dist_gk
#' @export
dist_gk <- function(A, B, g, k, c = 0.8){
  A <- vec_cast(A, double())
  B <- vec_cast(B, double())
  g <- vec_cast(g, double())
  k <- vec_cast(k, double())
  c <- vec_cast(c, double())
  if(any(B <= 0)){
    abort("The B parameter (scale) of the gk distribution must be strictly positive.")
  }
  new_dist(A = A, B = B, g = g, k = k, c = c, class = "dist_gk")
}

#' @export
format.dist_gk <- function(x, digits = 2, ...){
  sprintf(
    "gk(A = %s, B = %s, g = %s, k = %s%s)",
    format(x[["A"]], digits = digits, ...),
    format(x[["B"]], digits = digits, ...),
    format(x[["g"]], digits = digits, ...),
    format(x[["k"]], digits = digits, ...),
    if (x[["c"]]==0.8) "" else paste0(", c = ", format(x[["c"]], digits = digits, ...))
  )
}

#' @export
density.dist_gk <- function(x, at, ...){
  require_package("gk")
  gk::dgk(at, x[["A"]], x[["B"]], x[["g"]], x[["k"]], x[["c"]])
}

#' @export
log_density.dist_gk <- function(x, at, ...){
  require_package("gk")
  gk::dgk(at, x[["A"]], x[["B"]], x[["g"]], x[["k"]], x[["c"]], log = TRUE)
}

#' @export
quantile.dist_gk <- function(x, p, ...){
  require_package("gk")
  gk::qgk(p, x[["A"]], x[["B"]], x[["g"]], x[["k"]], x[["c"]])
}

#' @export
cdf.dist_gk <- function(x, q, ...){
  require_package("gk")
  gk::pgk(q, x[["A"]], x[["B"]], x[["g"]], x[["k"]], x[["c"]])
}

#' @export
generate.dist_gk <- function(x, times, ...){
  require_package("gk")
  gk::rgk(times, x[["A"]], x[["B"]], x[["g"]], x[["k"]], x[["c"]])
}
