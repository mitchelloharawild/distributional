#' The Burr distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Burr distribution (Type XII) is a flexible continuous probability
#' distribution often used for modeling income distributions, reliability
#' data, and failure times.
#'
#' @inheritParams actuar::dburr
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_burr")`
#'
#'   In the following, let \eqn{X} be a Burr random variable with parameters
#'   `shape1` = \eqn{\alpha}, `shape2` = \eqn{\gamma}, and `rate` = \eqn{\lambda}.
#'
#'   **Support**: \eqn{x \in (0, \infty)}{x > 0}
#'
#'   **Mean**: \eqn{\frac{\lambda^{-1/\alpha} \gamma B(\gamma - 1/\alpha, 1 + 1/\alpha)}{\gamma}} (for \eqn{\alpha \gamma > 1})
#'
#'   **Variance**: \eqn{\frac{\lambda^{-2/\alpha} \gamma B(\gamma - 2/\alpha, 1 + 2/\alpha)}{\gamma} - \mu^2} (for \eqn{\alpha \gamma > 2})
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \alpha \gamma \lambda x^{\alpha - 1} (1 + \lambda x^\alpha)^{-\gamma - 1}
#'   }{
#'     f(x) = \alpha \gamma \lambda x^(\alpha - 1) (1 + \lambda x^\alpha)^(-\gamma - 1)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = 1 - (1 + \lambda x^\alpha)^{-\gamma}
#'   }{
#'     F(x) = 1 - (1 + \lambda x^\alpha)^(-\gamma)
#'   }
#'
#'   **Quantile function**:
#'
#'   \deqn{
#'     F^{-1}(p) = \lambda^{-1/\alpha} ((1 - p)^{-1/\gamma} - 1)^{1/\alpha}
#'   }{
#'     F^(-1)(p) = \lambda^(-1/\alpha) ((1 - p)^(-1/\gamma) - 1)^(1/\alpha)
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   Does not exist in closed form.
#'
#' @seealso [actuar::Burr]
#'
#' @examples
#' dist <- dist_burr(shape1 = c(1,1,1,2,3,0.5), shape2 = c(1,2,3,1,1,2))
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
#' @name dist_burr
#' @export
dist_burr <- function(shape1, shape2, rate = 1, scale = 1/rate){
  shape1 <- vec_cast(shape1, double())
  shape2 <- vec_cast(shape2, double())
  if(any(shape1 <= 0)){
    abort("The shape1 parameter of a Burr distribution must be strictly positive.")
  }
  if(any(shape2 <= 0)){
    abort("The shape2 parameter of a Burr distribution must be strictly positive.")
  }
  if(any(rate <= 0)){
    abort("The rate parameter of a Burr distribution must be strictly positive.")
  }
  new_dist(s1 = shape1, s2 = shape2, r = 1/scale, class = "dist_burr")
}

#' @export
format.dist_burr <- function(x, digits = 2, ...){
  sprintf(
    "Burr12(%s, %s, %s)",
    format(x[["s1"]], digits = digits, ...),
    format(x[["s2"]], x[["r"]], digits = digits, ...),
    format(x[["r"]], digits = digits, ...)
  )
}

#' @export
density.dist_burr <- function(x, at, ...){
  require_package("actuar")
  actuar::dburr(at, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
log_density.dist_burr <- function(x, at, ...){
  require_package("actuar")
  actuar::dburr(at, x[["s1"]], x[["s2"]], x[["r"]], log = TRUE)
}

#' @export
quantile.dist_burr <- function(x, p, ...){
  require_package("actuar")
  actuar::qburr(p, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
cdf.dist_burr <- function(x, q, ...){
  require_package("actuar")
  actuar::pburr(q, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
generate.dist_burr <- function(x, times, ...){
  require_package("actuar")
  actuar::rburr(times, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
mean.dist_burr <- function(x, ...){
  require_package("actuar")
  actuar::mburr(1, x[["s1"]], x[["s2"]], x[["r"]])
}

#' @export
covariance.dist_burr <- function(x, ...){
  require_package("actuar")
  m1 <- actuar::mburr(1, x[["s1"]], x[["s2"]], x[["r"]])
  m2 <- actuar::mburr(2, x[["s1"]], x[["s2"]], x[["r"]])
  -m1^2 + m2
}
