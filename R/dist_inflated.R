#' Inflate a value of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Inflated distributions add extra probability mass at a specific value,
#' most commonly zero (zero-inflation). These distributions are useful for
#' modeling data with excess observations at a particular value compared to
#' what the base distribution would predict. Common applications include
#' zero-inflated Poisson or negative binomial models for count data with
#' many zeros.
#'
#' @param dist The distribution(s) to inflate.
#' @param prob The added probability of observing `x`.
#' @param x The value to inflate. The default of `x = 0` is for zero-inflation.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_inflated")`
#'
#'   In the following, let \eqn{Y} be an inflated random variable based on
#'   a base distribution \eqn{X}, with inflation value `x` = \eqn{c} and
#'   inflation probability `prob` = \eqn{p}.
#'
#'   **Support**: Same as the base distribution, but with additional
#'   probability mass at \eqn{c}
#'
#'   **Mean**: (when `x` is numeric)
#'
#'   \deqn{
#'     E(Y) = p \cdot c + (1-p) \cdot E(X)
#'   }{
#'     E(Y) = p * c + (1-p) * E(X)
#'   }
#'
#'   **Variance**: (when `x = 0`)
#'
#'   \deqn{
#'     \text{Var}(Y) = (1-p) \cdot \text{Var}(X) + p(1-p) \cdot [E(X)]^2
#'   }{
#'     Var(Y) = (1-p) * Var(X) + p(1-p) * [E(X)]^2
#'   }
#'
#'   For non-zero inflation values, the variance is not computed in closed form.
#'
#'   **Probability mass/density function (p.m.f/p.d.f)**:
#'
#'   For discrete distributions:
#'   \deqn{
#'     f_Y(y) = \begin{cases}
#'       p + (1-p) \cdot f_X(c) & \text{if } y = c \\
#'       (1-p) \cdot f_X(y) & \text{if } y \neq c
#'     \end{cases}
#'   }{
#'     f_Y(y) = p + (1-p) * f_X(c) if y = c, (1-p) * f_X(y) if y != c
#'   }
#'
#'   For continuous distributions:
#'   \deqn{
#'     f_Y(y) = \begin{cases}
#'       p & \text{if } y = c \\
#'       (1-p) \cdot f_X(y) & \text{if } y \neq c
#'     \end{cases}
#'   }{
#'     f_Y(y) = p if y = c, (1-p) * f_X(y) if y != c
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F_Y(q) = \begin{cases}
#'       (1-p) \cdot F_X(q) & \text{if } q < c \\
#'       p + (1-p) \cdot F_X(q) & \text{if } q \geq c
#'     \end{cases}
#'   }{
#'     F_Y(q) = (1-p) * F_X(q) if q < c, p + (1-p) * F_X(q) if q >= c
#'   }
#'
#'   **Quantile function**:
#'
#'   The quantile function is computed numerically by inverting the
#'   inflated CDF, accounting for the jump in probability at the
#'   inflation point.
#'
#' @examples
#' # Zero-inflated Poisson
#' dist <- dist_inflated(dist_poisson(lambda = 2), prob = 0.3, x = 0)
#'
#' dist
#' mean(dist)
#' variance(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 0)
#' density(dist, 1)
#'
#' cdf(dist, 2)
#'
#' quantile(dist, 0.5)
#'
#' @name dist_inflated
#' @export
dist_inflated <- function(dist, prob, x = 0){
  vec_is(dist, new_dist())
  if(prob < 0 || prob > 1){
    abort("The inflation probability must be between 0 and 1.")
  }
  new_dist(dist = dist, x = x, p = prob,
           dimnames = dimnames(dist), class = "dist_inflated")
}

#' @export
format.dist_inflated <- function(x, ...){
  sprintf(
    "%s+%s",
    format(x[["x"]]),
    format(x[["dist"]])
  )
}

#' @export
density.dist_inflated <- function(x, at, ...){
  x[["p"]]*(at==x[["x"]]) + (1-x[["p"]])*density(x[["dist"]], at, ...)
}

#' @export
quantile.dist_inflated <- function(x, p, ...){
  qt <- quantile(x[["dist"]], pmax(0, (p - x[["p"]]) / (1-x[["p"]])), ...)
  if(qt >= x[["x"]]) return(qt)
  qt <- quantile(x[["dist"]], p, ...)
  if(qt < x[["x"]]) qt else x[["x"]]
}

#' @export
cdf.dist_inflated <- function(x, q, ...){
  x[["p"]]*(q>=x[["x"]]) + (1-x[["p"]])*cdf(x[["dist"]], q, ...)
}

#' @export
generate.dist_inflated <- function(x, times, ...){
  p <- x[["p"]]
  inf <- stats::runif(times) < p
  r <- vec_init(x[["x"]], times)
  r[inf] <- x[["x"]]
  r[!inf] <- generate(x[["dist"]], sum(!inf))
  r
}

#' @export
mean.dist_inflated <- function(x, ...){
  # Can't compute if inflation value is not numeric
  if(!vec_is(x[["x"]], numeric())) return(NA_real_)

  p <- x[["p"]]
  p*x[["x"]] + (1-p)*mean(x[["dist"]])
}

#' @export
covariance.dist_inflated <- function(x, ...){
  # Can't compute if inflation value is not numeric
  if(!vec_is(x[["x"]], numeric())) return(NA_real_)
  # Can't (easily) compute if inflation value is not zero
  if(x[["x"]] != 0) return(NA_real_)

  m1 <- mean(x[["dist"]])
  v <- variance(x[["dist"]])
  m2 <- v + m1^2
  p <- x[["p"]]
  (1-p)*v + p*(1-p)*m1^2
}
