#' The multivariate t-distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The multivariate t-distribution is a generalization of the univariate
#' Student's t-distribution to multiple dimensions. It is commonly used for
#' modeling heavy-tailed multivariate data and in robust statistics.
#'
#' @param df A numeric vector of degrees of freedom (must be positive).
#' @param mu A list of numeric vectors for the distribution location parameter.
#' @param sigma A list of matrices for the distribution scale matrix.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_multivariate_t")`
#'
#'   In the following, let \eqn{\mathbf{X}} be a multivariate t random vector
#'   with degrees of freedom `df` = \eqn{\nu}, location parameter
#'   `mu` = \eqn{\boldsymbol{\mu}}, and scale matrix
#'   `sigma` = \eqn{\boldsymbol{\Sigma}}.
#'
#'   **Support**: \eqn{\mathbf{x} \in \mathbb{R}^k}, where \eqn{k} is the
#'   dimension of the distribution
#'
#'   **Mean**: \eqn{\boldsymbol{\mu}} for \eqn{\nu > 1}, undefined otherwise
#'
#'   **Covariance matrix**: 
#'   
#'   \deqn{
#'     \text{Cov}(\mathbf{X}) = \frac{\nu}{\nu - 2} \boldsymbol{\Sigma}
#'   }{
#'     Cov(X) = \nu / (\nu - 2) \Sigma
#'   }
#'   
#'   for \eqn{\nu > 2}, undefined otherwise
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(\mathbf{x}) = \frac{\Gamma\left(\frac{\nu + k}{2}\right)}
#'     {\Gamma\left(\frac{\nu}{2}\right) \nu^{k/2} \pi^{k/2}
#'     |\boldsymbol{\Sigma}|^{1/2}}
#'     \left[1 + \frac{1}{\nu}(\mathbf{x} - \boldsymbol{\mu})^T
#'     \boldsymbol{\Sigma}^{-1} (\mathbf{x} - \boldsymbol{\mu})\right]^{-\frac{\nu + k}{2}}
#'   }{
#'     f(x) = \Gamma((\nu + k)/2) / (\Gamma(\nu/2) \nu^(k/2) \pi^(k/2) |\Sigma|^(1/2))
#'     [1 + 1/\nu (x - \mu)^T \Sigma^(-1) (x - \mu)]^(-(\nu + k)/2)
#'   }
#'
#'   where \eqn{k} is the dimension of the distribution and \eqn{\Gamma(\cdot)} is
#'   the gamma function.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(\mathbf{t}) = \int_{-\infty}^{t_1} \cdots \int_{-\infty}^{t_k} f(\mathbf{x}) \, d\mathbf{x}
#'   }{
#'     F(t) = integral_{-\infty}^{t_1} ... integral_{-\infty}^{t_k} f(x) dx
#'   }
#'
#'   This integral does not have a closed form solution and is approximated numerically.
#'
#'   **Quantile function**:
#'
#'   The equicoordinate quantile function finds \eqn{q} such that:
#'
#'   \deqn{
#'     P(X_1 \leq q, \ldots, X_k \leq q) = p
#'   }{
#'     P(X_1 <= q, ..., X_k <= q) = p
#'   }
#'
#'   This does not have a closed form solution and is approximated numerically.
#'
#'   The marginal quantile function for each dimension \eqn{i} is:
#'
#'   \deqn{
#'     Q_i(p) = \mu_i + \sqrt{\Sigma_{ii}} \cdot t_{\nu}^{-1}(p)
#'   }{
#'     Q_i(p) = \mu_i + sqrt(\Sigma_{ii}) * t_\nu^(-1)(p)
#'   }
#'
#'   where \eqn{t_{\nu}^{-1}(p)} is the quantile function of the univariate
#'   Student's t-distribution with \eqn{\nu} degrees of freedom, and
#'   \eqn{\Sigma_{ii}} is the \eqn{i}-th diagonal element of `sigma`.
#'
#' @seealso [mvtnorm::dmvt], [mvtnorm::pmvt], [mvtnorm::qmvt], [mvtnorm::rmvt]
#'
#' @examples
#' dist <- dist_multivariate_t(
#'   df = 5,
#'   mu = list(c(1, 2)),
#'   sigma = list(matrix(c(4, 2, 2, 3), ncol = 2))
#' )
#' dimnames(dist) <- c("x", "y")
#' dist
#'
#' @examplesIf requireNamespace("mvtnorm", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, cbind(2, 1))
#' density(dist, cbind(2, 1), log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#' quantile(dist, 0.7, kind = "marginal")
#'
#' @name dist_multivariate_t
#' @export
dist_multivariate_t <- function(df = 1, mu = 0, sigma = diag(1)) {
  df <- vec_cast(df, double())
  if (any(df <= 0, na.rm = TRUE)) {
    abort("Degrees of freedom must be positive.")
  }
  
  new_dist(
    df = df,
    mu = mu,
    sigma = sigma,
    dimnames = colnames(sigma[[1]]),
    class = "dist_mvt"
  )
}

#' @export
format.dist_mvt <- function(x, digits = 2, ...) {
  sprintf(
    "MVT[%i](%s)",
    length(x[["mu"]]),
    format(x[["df"]], digits = digits, ...)
  )
}

#' @export
density.dist_mvt <- function(x, at, ..., na.rm = FALSE) {
  require_package("mvtnorm")
  if (is.list(at)) return(vapply(at, density, numeric(1L), x = x, ...))
  mvtnorm::dmvt(at, delta = x[["mu"]], sigma = x[["sigma"]], df = x[["df"]], log = FALSE, ...)
}

#' @export
log_density.dist_mvt <- function(x, at, ..., na.rm = FALSE) {
  require_package("mvtnorm")
  if (is.list(at)) return(vapply(at, log_density, numeric(1L), x = x, ...))
  mvtnorm::dmvt(at, delta = x[["mu"]], sigma = x[["sigma"]], df = x[["df"]], log = TRUE, ...)
}

#' @export
quantile.dist_mvt <- function(x, p, kind = c("equicoordinate", "marginal"),
                               ..., na.rm = FALSE) {
  kind <- match.arg(kind)
  q <- if (kind == "marginal") {
    scale <- sqrt(diag(x[["sigma"]]))
    stats::qt(p, df = rep(x[["df"]], each = length(p))) * 
      rep(scale, each = length(p)) + 
      rep(x[["mu"]], each = length(p))
  } else {
    require_package("mvtnorm")
    vapply(p, function(p, ...) {
      if (p == 0) return(-Inf) else if (p == 1) return(Inf)
      mvtnorm::qmvt(p, ...)$quantile
    }, numeric(1L), delta = x[["mu"]], sigma = x[["sigma"]], df = x[["df"]], ...)
  }
  
  matrix(q, nrow = length(p), ncol = dim(x))
}

#' @export
cdf.dist_mvt <- function(x, q, ..., na.rm = FALSE) {
  if (is.list(q)) return(vapply(q, cdf, numeric(1L), x = x, ...))
  require_package("mvtnorm")
  mvtnorm::pmvt(upper = as.numeric(q), delta = x[["mu"]], sigma = x[["sigma"]], df = x[["df"]], ...)[1]
}

#' @export
generate.dist_mvt <- function(x, times, ..., na.rm = FALSE) {
  require_package("mvtnorm")
  mvtnorm::rmvt(times, delta = x[["mu"]], sigma = x[["sigma"]], df = x[["df"]], ...)
}

#' @export
mean.dist_mvt <- function(x, ...) {
  if (x[["df"]] <= 1) {
    warning("Mean is undefined for df <= 1")
    return(matrix(NA_real_, nrow = 1, ncol = length(x[["mu"]])))
  }
  matrix(x[["mu"]], nrow = 1)
}

#' @export
covariance.dist_mvt <- function(x, ...) {
  if (x[["df"]] <= 2) {
    warning("Covariance is undefined for df <= 2")
    return(list(matrix(NA_real_, nrow = length(x[["mu"]]), ncol = length(x[["mu"]]))))
  }
  list(x[["df"]] / (x[["df"]] - 2) * x[["sigma"]])
}

#' @export
dim.dist_mvt <- function(x) {
  length(x[["mu"]])
}
