#' The F Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The F distribution is commonly used in statistical inference, particularly
#' in the analysis of variance (ANOVA), testing the equality of variances,
#' and in regression analysis. It arises as the ratio of two scaled
#' chi-squared distributions divided by their respective degrees of freedom.
#'
#' @param df1 Degrees of freedom for the numerator. Can be any positive number.
#' @param df2 Degrees of freedom for the denominator. Can be any positive number.
#' @param ncp Non-centrality parameter. If `NULL` (default), the central F
#'   distribution is used. If specified, must be non-negative.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_f")`
#'
#'   In the following, let \eqn{X} be an F random variable with numerator
#'   degrees of freedom `df1` = \eqn{d_1} and denominator degrees of freedom
#'   `df2` = \eqn{d_2}.
#'
#'   **Support**: \eqn{x \in (0, \infty)}
#'
#'   **Mean**: 
#'   
#'   For the central F distribution (\code{ncp = NULL}):
#'   
#'   \deqn{
#'     E(X) = \frac{d_2}{d_2 - 2}
#'   }{
#'     E(X) = d_2 / (d_2 - 2)
#'   }
#'   
#'   for \eqn{d_2 > 2}, otherwise undefined.
#'   
#'   For the non-central F distribution with non-centrality parameter 
#'   `ncp` = \eqn{\lambda}:
#'   
#'   \deqn{
#'     E(X) = \frac{d_2 (d_1 + \lambda)}{d_1 (d_2 - 2)}
#'   }{
#'     E(X) = d_2 (d_1 + \lambda) / (d_1 (d_2 - 2))
#'   }
#'   
#'   for \eqn{d_2 > 2}, otherwise undefined.
#'
#'   **Variance**: 
#'   
#'   For the central F distribution (\code{ncp = NULL}):
#'   
#'   \deqn{
#'     \text{Var}(X) = \frac{2 d_2^2 (d_1 + d_2 - 2)}{d_1 (d_2 - 2)^2 (d_2 - 4)}
#'   }{
#'     Var(X) = 2 d_2^2 (d_1 + d_2 - 2) / (d_1 (d_2 - 2)^2 (d_2 - 4))
#'   }
#'   
#'   for \eqn{d_2 > 4}, otherwise undefined.
#'   
#'   For the non-central F distribution with non-centrality parameter 
#'   `ncp` = \eqn{\lambda}:
#'   
#'   \deqn{
#'     \text{Var}(X) = \frac{2 d_2^2}{d_1^2} \cdot \frac{(d_1 + \lambda)^2 + (d_1 + 2\lambda)(d_2 - 2)}{(d_2 - 2)^2 (d_2 - 4)}
#'   }{
#'     Var(X) = 2 d_2^2 / d_1^2 * ((d_1 + lambda)^2 + (d_1 + 2*lambda)(d_2 - 2)) / ((d_2 - 2)^2 (d_2 - 4))
#'   }
#'   
#'   for \eqn{d_2 > 4}, otherwise undefined.
#'
#'   **Skewness**: 
#'   
#'   For the central F distribution (\code{ncp = NULL}):
#'   
#'   \deqn{
#'     \text{Skew}(X) = \frac{(2 d_1 + d_2 - 2) \sqrt{8 (d_2 - 4)}}{(d_2 - 6) \sqrt{d_1 (d_1 + d_2 - 2)}}
#'   }{
#'     Skew(X) = (2 d_1 + d_2 - 2) sqrt(8 (d_2 - 4)) / ((d_2 - 6) sqrt(d_1 (d_1 + d_2 - 2)))
#'   }
#'   
#'   for \eqn{d_2 > 6}, otherwise undefined.
#'   
#'   For the non-central F distribution, skewness has no simple closed form
#'   and is not computed.
#'
#'   **Excess Kurtosis**: 
#'   
#'   For the central F distribution (\code{ncp = NULL}):
#'   
#'   \deqn{
#'     \text{Kurt}(X) = \frac{12[d_1 (5 d_2 - 22)(d_1 + d_2 - 2) + (d_2 - 4)(d_2 - 2)^2]}{d_1 (d_2 - 6)(d_2 - 8)(d_1 + d_2 - 2)}
#'   }{
#'     Kurt(X) = 12[d_1 (5 d_2 - 22)(d_1 + d_2 - 2) + (d_2 - 4)(d_2 - 2)^2] / (d_1 (d_2 - 6)(d_2 - 8)(d_1 + d_2 - 2))
#'   }
#'   
#'   for \eqn{d_2 > 8}, otherwise undefined.
#'   
#'   For the non-central F distribution, kurtosis has no simple closed form
#'   and is not computed.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   For the central F distribution (\code{ncp = NULL}):
#'
#'   \deqn{
#'     f(x) = \frac{\sqrt{\frac{(d_1 x)^{d_1} d_2^{d_2}}{(d_1 x + d_2)^{d_1 + d_2}}}}{x \, B(d_1/2, d_2/2)}
#'   }{
#'     f(x) = sqrt((d_1 x)^d_1 d_2^d_2 / (d_1 x + d_2)^(d_1 + d_2)) / (x B(d_1/2, d_2/2))
#'   }
#'   
#'   where \eqn{B(\cdot, \cdot)} is the beta function.
#'   
#'   For the non-central F distribution, the density involves an infinite
#'   series and is approximated numerically.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The c.d.f. does not have a simple closed form expression and is
#'   approximated numerically using regularized incomplete beta functions
#'   and related special functions.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   The moment generating function for the F distribution does not exist
#'   in general (it diverges for \eqn{t > 0}).
#'
#' @seealso [stats::FDist]
#'
#' @examples
#' dist <- dist_f(df1 = c(1,2,5,10,100), df2 = c(1,1,2,1,100))
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
#' @name dist_f
#' @export
dist_f <- function(df1, df2, ncp = NULL){
  df1 <- vec_cast(df1, double())
  df2 <- vec_cast(df2, double())
  ncp <- vec_cast(ncp, double())
  if(any((df1 < 0) | (df2 < 0))){
    abort("The degrees of freedom parameters of the F distribution must be non-negative.")
  }
  if(is.null(ncp)){
    new_dist(df1 = df1, df2 = df2, class = "dist_f")
  } else {
    new_dist(df1 = df1, df2 = df2, ncp = ncp, class = "dist_f")
  }
}

#' @export
format.dist_f <- function(x, digits = 2, ...){
  sprintf(
    "F(%s, %s)",
    format(x[["df1"]], digits = digits, ...),
    format(x[["df2"]], digits = digits, ...)
  )
}

#' @export
density.dist_f <- function(x, at, ...){
  if(is.null(x[["ncp"]])) {
    stats::df(at, x[["df1"]], x[["df2"]])
  } else {
    stats::df(at, x[["df1"]], x[["df2"]], x[["ncp"]])
  }
}

#' @export
log_density.dist_f <- function(x, at, ...){
  if(is.null(x[["ncp"]])) {
    stats::df(at, x[["df1"]], x[["df2"]], log = TRUE)
  } else {
    stats::df(at, x[["df1"]], x[["df2"]], x[["ncp"]], log = TRUE)
  }
}

#' @export
quantile.dist_f <- function(x, p, ...){
  if(is.null(x[["ncp"]])) {
    stats::qf(p, x[["df1"]], x[["df2"]])
  } else {
    stats::qf(p, x[["df1"]], x[["df2"]], x[["ncp"]])
  }
}

#' @export
cdf.dist_f <- function(x, q, ...){
  if(is.null(x[["ncp"]])) {
    stats::pf(q, x[["df1"]], x[["df2"]])
  } else {
    stats::pf(q, x[["df1"]], x[["df2"]], x[["ncp"]])
  }
}

#' @export
generate.dist_f <- function(x, times, ...){
  if(is.null(x[["ncp"]])) {
    stats::rf(times, x[["df1"]], x[["df2"]])
  } else {
    stats::rf(times, x[["df1"]], x[["df2"]], x[["ncp"]])
  }
}

#' @export
mean.dist_f <- function(x, ...){
  df1 <- x[["df1"]]
  df2 <- x[["df2"]]
  if(df2 > 2) {
    if(is.null(x[["ncp"]])){
      df2 / (df2 - 2)
    } else {
      (df2 * (df1 + x[["ncp"]])) / (df1 * (df2 - 2))
    }
  } else {
    NA_real_
  }
}

#' @export
covariance.dist_f <- function(x, ...){
  df1 <- x[["df1"]]
  df2 <- x[["df2"]]
  if(df2 > 4) {
    if(is.null(x[["ncp"]])){
      (2 * df2^2 * (df1 + df2 - 2))/(df1*(df2-2)^2*(df2-4))
    } else {
      2*((df1 + x[["ncp"]])^2 + (df1 + 2*x[["ncp"]])*(df2 - 2))/((df2-2)^2*(df2-4)) * (df2^2/df1^2)
    }
  } else {
    NA_real_
  }
}

#' @export
skewness.dist_f <- function(x, ...) {
  df1 <- x[["df1"]]
  df2 <- x[["df2"]]
  if(!is.null(x[["ncp"]])) return(NA_real_)
  if (df2 > 6) {
    a <- (2 * df1 + df2 - 2) * sqrt(8 * (df2 - 4))
    b <- (df2 - 6) * sqrt(df1 * (df1 + df2 - 2))
    a / b
  } else {
    NA_real_
  }
}

#' @export
kurtosis.dist_f <- function(x, ...) {
  df1 <- x[["df1"]]
  df2 <- x[["df2"]]
  if(!is.null(x[["ncp"]])) return(NA_real_)
  if (df2 > 8) {
    a <- df1 * (5 * df2 - 22) * (df1 + df2 - 2) + (df2 - 4) * (df2 - 2)^2
    b <- df1 * (df2 - 6) * (df2 - 8) * (df1 + df2 - 2)
    12 * a / b
  } else {
    NA_real_
  }
}
