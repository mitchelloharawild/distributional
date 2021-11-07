#' The (non-central) Chi-Squared Distribution
#'
#' \lifecycle{stable}
#'
#' Chi-square distributions show up often in frequentist settings
#' as the sampling distribution of test statistics, especially
#' in maximum likelihood estimation settings.
#'
#' @inheritParams stats::dchisq
#'
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a \eqn{\chi^2} random variable with
#'   `df` = \eqn{k}.
#'
#'   **Support**: \eqn{R^+}, the set of positive real numbers
#'
#'   **Mean**: \eqn{k}
#'
#'   **Variance**: \eqn{2k}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{1}{\sqrt{2 \pi \sigma^2}} e^{-(x - \mu)^2 / 2 \sigma^2}
#'   }{
#'     f(x) = 1 / (2 \pi \sigma^2) exp(-(x - \mu)^2 / (2 \sigma^2))
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cumulative distribution function has the form
#'
#'   \deqn{
#'     F(t) = \int_{-\infty}^t \frac{1}{\sqrt{2 \pi \sigma^2}} e^{-(x - \mu)^2 / 2 \sigma^2} dx
#'   }{
#'     F(t) = integral_{-\infty}^t 1 / (2 \pi \sigma^2) exp(-(x - \mu)^2 / (2 \sigma^2)) dx
#'   }
#'
#'   but this integral does not have a closed form solution and must be
#'   approximated numerically. The c.d.f. of a standard normal is sometimes
#'   called the "error function". The notation \eqn{\Phi(t)} also stands
#'   for the c.d.f. of a standard normal evaluated at \eqn{t}. Z-tables
#'   list the value of \eqn{\Phi(t)} for various \eqn{t}.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = e^{\mu t + \sigma^2 t^2 / 2}
#'   }{
#'     E(e^(tX)) = e^(\mu t + \sigma^2 t^2 / 2)
#'   }
#'
#'
#' @seealso [stats::Chisquare]
#'
#' @examples
#' dist <- dist_chisq(df = c(1,2,3,4,6,9))
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
#' @name dist_chisq
#' @export
dist_chisq <- function(df, ncp = 0){
  df <- vec_cast(df, double())
  ncp <- vec_cast(ncp, double())
  if(any(df < 0)){
    abort("The degrees of freedom parameter of a Chi-Squared distribution must be non-negative.")
  }
  new_dist(df = df, ncp = ncp, class = "dist_chisq")
}

#' @export
format.dist_chisq <- function(x, digits = 2, ...){
  sprintf(
    if (is_utf8_output()) "\u1d6a\u00b2(%s)" else "x2(%s)",
    format(x[["df"]], digits = digits, ...)
  )
}

#' @export
density.dist_chisq <- function(x, at, ...){
  stats::dchisq(at, x[["df"]], x[["ncp"]])
}

#' @export
log_density.dist_chisq <- function(x, at, ...){
  stats::dchisq(at, x[["df"]], x[["ncp"]], log = TRUE)
}

#' @export
quantile.dist_chisq <- function(x, p, ...){
  stats::qchisq(p, x[["df"]], x[["ncp"]])
}

#' @export
cdf.dist_chisq <- function(x, q, ...){
  stats::pchisq(q, x[["df"]], x[["ncp"]])
}

#' @export
generate.dist_chisq <- function(x, times, ...){
  stats::rchisq(times, x[["df"]], x[["ncp"]])
}

#' @export
mean.dist_chisq <- function(x, ...){
  x[["df"]] + x[["ncp"]]
}

#' @export
covariance.dist_chisq <- function(x, ...){
  2*(x[["df"]] + 2*x[["ncp"]])
}

#' @export
skewness.dist_chisq <- function(x, ...) sqrt(8 / x[["df"]])

#' @export
kurtosis.dist_chisq <- function(x, ...) 12 / x[["df"]]
