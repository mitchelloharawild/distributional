#' The log-normal distribution
#'
#' \lifecycle{stable}
#'
#' The log-normal distribution is a commonly used transformation of the Normal
#' distribution. If \eqn{X} follows a log-normal distribution, then \eqn{\ln{X}}
#' would be characteristed by a Normal distribution.
#'
#' @param mu The mean (location parameter) of the distribution, which is the
#'   mean of the associated Normal distribution. Can be any real number.
#' @param sigma The standard deviation (scale parameter) of the distribution.
#'   Can be any positive number.
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{Y} be a Normal random variable with mean
#'   `mu` = \eqn{\mu} and standard deviation `sigma` = \eqn{\sigma}. The
#'   log-normal distribution \eqn{X = exp(Y)} is characterised by:
#'
#'   **Support**: \eqn{R+}, the set of all real numbers greater than or equal to 0.
#'
#'   **Mean**: \eqn{e^(\mu + \sigma^2/2}
#'
#'   **Variance**: \eqn{(e^(\sigma^2)-1) e^(2\mu + \sigma^2}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{1}{x\sqrt{2 \pi \sigma^2}} e^{-(\ln{x} - \mu)^2 / 2 \sigma^2}
#'   }{
#'     f(x) = 1 / (x * sqrt(2 \pi \sigma^2)) exp(-(log(x) - \mu)^2 / (2 \sigma^2))
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cumulative distribution function has the form
#'
#'   \deqn{
#'     F(x) = \Phi((\ln{x} - \mu)/\sigma)
#'   }{
#'     F(x) = Phi((log(x) - \mu)/\sigma)
#'   }
#'
#'   Where \eqn{Phi}{Phi} is the CDF of a standard Normal distribution, N(0,1).
#'
#' @seealso [stats::Lognormal]
#'
#' @examples
#' dist <- dist_lognormal(mu = 1:5, sigma = 0.1)
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
#' # A log-normal distribution X is exp(Y), where Y is a Normal distribution of
#' # the same parameters. So log(X) will produce the Normal distribution Y.
#' log(dist)
#' @export
dist_lognormal <- function(mu = 0, sigma = 1){
  mu <- vec_cast(mu, double())
  sigma <- vec_cast(sigma, double())
  if(any(sigma[!is.na(sigma)] < 0)){
    abort("Standard deviation of a log-normal distribution must be non-negative")
  }
  new_dist(mu = mu, sigma = sigma, class = "dist_lognormal")
}

#' @export
format.dist_lognormal <- function(x, digits = 2, ...){
  sprintf(
    "lN(%s, %s)",
    format(x[["mu"]], digits = digits, ...),
    format(x[["sigma"]]^2, digits = digits, ...)
  )
}

#' @export
density.dist_lognormal <- function(x, at, ...){
  stats::dlnorm(at, x[["mu"]], x[["sigma"]])
}

#' @export
log_density.dist_lognormal <- function(x, at, ...){
  stats::dlnorm(at, x[["mu"]], x[["sigma"]], log = TRUE)
}

#' @export
quantile.dist_lognormal <- function(x, p, ...){
  stats::qlnorm(p, x[["mu"]], x[["sigma"]])
}
#' @export
log_quantile.dist_lognormal <- function(x, p, ...){
  stats::qlnorm(p, x[["mu"]], x[["sigma"]], log.p = TRUE)
}

#' @export
cdf.dist_lognormal <- function(x, q, ...){
  stats::plnorm(q, x[["mu"]], x[["sigma"]])
}
#' @export
log_cdf.dist_lognormal <- function(x, q, ...){
  stats::plnorm(q, x[["mu"]], x[["sigma"]], log.p = TRUE)
}

#' @export
generate.dist_lognormal <- function(x, times, ...){
  stats::rlnorm(times, x[["mu"]], x[["sigma"]])
}

#' @export
mean.dist_lognormal <- function(x, ...){
  exp(x[["mu"]] + x[["sigma"]]^2/2)
}

#' @export
covariance.dist_lognormal <- function(x, ...){
  s2 <- x[["sigma"]]^2
  (exp(s2)-1)*exp(2*x[["mu"]] + s2)
}

#' @export
skewness.dist_lognormal <- function(x, ...) {
  es2 <- exp(x[["sigma"]]^2)
  (es2+2)*sqrt(es2-1)
}

#' @export
kurtosis.dist_lognormal <- function(x, ...) {
  s2 <- x[["sigma"]]^2
  exp(4*s2) + 2*exp(3*s2) + 3*exp(2*s2) - 6
}

# make a normal distribution from a lognormal distribution using the
# specified base
normal_dist_with_base <- function(x, base = exp(1)) {
  vec_data(dist_normal(x[["mu"]], x[["sigma"]]) / log(base))[[1]]
}

#' @method Math dist_lognormal
#' @export
Math.dist_lognormal <- function(x, ...) {
  switch(.Generic,
    # Shortcuts to get Normal distribution from log-normal.
    log = normal_dist_with_base(x, ...),
    log2 = normal_dist_with_base(x, 2),
    log10 = normal_dist_with_base(x, 10),

    NextMethod()
  )
}
