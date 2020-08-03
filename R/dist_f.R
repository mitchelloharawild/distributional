#' The F Distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::df
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Gamma random variable
#'   with parameters
#'   `shape` = \eqn{\alpha} and
#'   `rate` = \eqn{\beta}.
#'
#'   **Support**: \eqn{x \in (0, \infty)}
#'
#'   **Mean**: \eqn{\frac{\alpha}{\beta}}
#'
#'   **Variance**: \eqn{\frac{\alpha}{\beta^2}}
#'
#'   **Probability density function (p.m.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\beta^{\alpha}}{\Gamma(\alpha)} x^{\alpha - 1} e^{-\beta x}
#'   }{
#'     f(x) = \frac{\beta^{\alpha}}{\Gamma(\alpha)} x^{\alpha - 1} e^{-\beta x}
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{\Gamma(\alpha, \beta x)}{\Gamma{\alpha}}
#'   }{
#'     f(x) = \frac{\Gamma(\alpha, \beta x)}{\Gamma{\alpha}}
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \Big(\frac{\beta}{ \beta - t}\Big)^{\alpha}, \thinspace t < \beta
#'   }{
#'     E(e^(tX)) = \Big(\frac{\beta}{ \beta - t}\Big)^{\alpha}, \thinspace t < \beta
#'   }
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
print.dist_f <- function(x, ...){
  cat(format(x, ...))
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
variance.dist_f <- function(x, ...){
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
