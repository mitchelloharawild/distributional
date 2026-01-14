#' The (non-central) Chi-Squared Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Chi-square distributions show up often in frequentist settings
#' as the sampling distribution of test statistics, especially
#' in maximum likelihood estimation settings.
#'
#' @param df Degrees of freedom (non-centrality parameter). Can be any
#'   positive real number.
#' @param ncp Non-centrality parameter. Can be any non-negative real number.
#'   Defaults to 0 (central chi-squared distribution).
#'
#' @details
#'
#'
#' `r pkgdown_doc_link("dist_chisq")`
#'
#'   In the following, let \eqn{X} be a \eqn{\chi^2} random variable with
#'   `df` = \eqn{k} and `ncp` = \eqn{\lambda}.
#'
#'   **Support**: \eqn{R^+}, the set of positive real numbers
#'
#'   **Mean**: \eqn{k + \lambda}
#'
#'   **Variance**: \eqn{2(k + 2\lambda)}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   For the central chi-squared distribution (\eqn{\lambda = 0}):
#'
#'   \deqn{
#'     f(x) = \frac{1}{2^{k/2} \Gamma(k/2)} x^{k/2 - 1} e^{-x/2}
#'   }{
#'     f(x) = 1 / (2^(k/2) \Gamma(k/2)) x^(k/2 - 1) exp(-x/2)
#'   }
#'
#'   For the non-central chi-squared distribution (\eqn{\lambda > 0}):
#'
#'   \deqn{
#'     f(x) = \frac{1}{2} e^{-(x+\lambda)/2} \left(\frac{x}{\lambda}\right)^{k/4-1/2} I_{k/2-1}\left(\sqrt{\lambda x}\right)
#'   }{
#'     f(x) = (1/2) exp(-(x+\lambda)/2) (x/\lambda)^(k/4-1/2) I_(k/2-1)(sqrt(\lambda x))
#'   }
#'
#'   where \eqn{I_\nu(z)} is the modified Bessel function of the first kind.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   For the central chi-squared distribution (\eqn{\lambda = 0}):
#'
#'   \deqn{
#'     F(x) = \frac{\gamma(k/2, x/2)}{\Gamma(k/2)} = P(k/2, x/2)
#'   }{
#'     F(x) = \gamma(k/2, x/2) / \Gamma(k/2) = P(k/2, x/2)
#'   }
#'
#'   where \eqn{\gamma(s, x)} is the lower incomplete gamma function and
#'   \eqn{P(s, x)} is the regularized gamma function.
#'
#'   For the non-central chi-squared distribution (\eqn{\lambda > 0}):
#'
#'   \deqn{
#'     F(x) = \sum_{j=0}^{\infty} \frac{e^{-\lambda/2} (\lambda/2)^j}{j!} P(k/2 + j, x/2)
#'   }{
#'     F(x) = sum_(j=0)^(\infty) exp(-\lambda/2) (\lambda/2)^j / j! P(k/2 + j, x/2)
#'   }
#'
#'   This is approximated numerically.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   For the central chi-squared distribution (\eqn{\lambda = 0}):
#'
#'   \deqn{
#'     E(e^{tX}) = (1 - 2t)^{-k/2}, \quad t < 1/2
#'   }{
#'     E(e^(tX)) = (1 - 2t)^(-k/2), t < 1/2
#'   }
#'
#'   For the non-central chi-squared distribution (\eqn{\lambda > 0}):
#'
#'   \deqn{
#'     E(e^{tX}) = \frac{e^{\lambda t / (1 - 2t)}}{(1 - 2t)^{k/2}}, \quad t < 1/2
#'   }{
#'     E(e^(tX)) = exp(\lambda t / (1 - 2t)) / (1 - 2t)^(k/2), t < 1/2
#'   }
#'
#'   **Skewness**:
#'
#'   \deqn{
#'     \gamma_1 = \frac{2^{3/2}(k + 3\lambda)}{(k + 2\lambda)^{3/2}}
#'   }{
#'     \gamma_1 = 2^(3/2) (k + 3\lambda) / (k + 2\lambda)^(3/2)
#'   }
#'
#'   For the central case (\eqn{\lambda = 0}), this simplifies to
#'   \eqn{\sqrt{8/k}}.
#'
#'   **Excess Kurtosis**:
#'
#'   \deqn{
#'     \gamma_2 = \frac{12(k + 4\lambda)}{(k + 2\lambda)^2}
#'   }{
#'     \gamma_2 = 12(k + 4\lambda) / (k + 2\lambda)^2
#'   }
#'
#'   For the central case (\eqn{\lambda = 0}), this simplifies to
#'   \eqn{12/k}.
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
