#' The Inverse Exponential distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Inverse Exponential distribution is used to model the reciprocal of 
#' exponentially distributed variables.
#'
#' @inheritParams actuar::dinvexp
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_inverse_exponential")`
#'
#'   In the following, let \eqn{X} be an Inverse Exponential random variable 
#'   with parameter `rate` = \eqn{\lambda}.
#'
#'   **Support**: \eqn{x > 0}
#'
#'   **Mean**: Does not exist, returns NA
#'
#'   **Variance**: Does not exist, returns NA
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\lambda}{x^2} e^{-\lambda/x}
#'   }{
#'     f(x) = \lambda / x^2 * e^(-\lambda/x)
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = e^{-\lambda/x}
#'   }{
#'     F(x) = e^(-\lambda/x)
#'   }
#'
#'   **Quantile function (inverse c.d.f)**:
#'
#'   \deqn{
#'     F^{-1}(p) = -\frac{\lambda}{\log(p)}
#'   }{
#'     F^(-1)(p) = -\lambda / log(p)
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   Does not exist (divergent integral).
#'
#' @seealso [actuar::InverseExponential]
#'
#' @examples
#' dist <- dist_inverse_exponential(rate = 1:5)
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
#' @name dist_inverse_exponential
#' @export
dist_inverse_exponential <- function(rate){
  rate <- vec_cast(rate, double())
  if(any(rate <= 0)){
    abort("The rate parameter of a Inverse Exponential distribution must be strictly positive.")
  }
  new_dist(r = rate, class = "dist_inverse_exponential")
}

#' @export
format.dist_inverse_exponential <- function(x, digits = 2, ...){
  sprintf(
    "InvExp(%s)",
    format(x[["r"]], digits = digits, ...)
  )
}

#' @export
density.dist_inverse_exponential <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvexp(at, x[["r"]])
}

#' @export
log_density.dist_inverse_exponential <- function(x, at, ...){
  require_package("actuar")
  actuar::dinvexp(at, x[["r"]], log = TRUE)
}

#' @export
quantile.dist_inverse_exponential <- function(x, p, ..., tail = "lower"){
  require_package("actuar")
  actuar::qinvexp(p, x[["r"]], lower.tail = tail == "lower")
}

#' @export
cdf.dist_inverse_exponential <- function(x, q, ..., tail = "lower"){
  require_package("actuar")
  actuar::pinvexp(q, x[["r"]], lower.tail = tail == "lower")
}

#' @export
generate.dist_inverse_exponential <- function(x, times, ...){
  require_package("actuar")
  vctrs::vec_chop(
    actuar::rinvexp(sum(times), rep(x[["r"]], times)),
    sizes = times
  )
}

#' @export
point_mass.dist_inverse_exponential <- point_mass_continuous

#' @export
support.dist_inverse_exponential <- function(x, ...) {
  n <- vctrs::vec_size(x)
  new_support_region(
    rep_len(list(numeric()), n),
    rep_len(list(c(0, Inf)), n),
    rep_len(list(c(FALSE, FALSE)), n)
  )
}

#' @export
mean.dist_inverse_exponential <- function(x, ...){
  rep_len(NA_real_, vctrs::vec_size(x))
}

#' @export
covariance.dist_inverse_exponential <- function(x, ...){
  rep_len(NA_real_, vctrs::vec_size(x))
}
