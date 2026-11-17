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
#' cdf(dist, cbind(4, 4))
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
    lengths(mv_field(x, "mu")),
    format(x[["df"]], digits = digits, ...)
  )
}

#' @export
density.dist_mvt <- function(x, at, ..., na.rm = FALSE) {
  require_package("mvtnorm")
  mu <- mv_field(x, "mu")
  sigma <- mv_field(x, "sigma")
  df <- x[["df"]]
  at <- mv_arg_rows(at, length(mu))
  vapply(seq_along(mu), function(i) {
    mvtnorm::dmvt(at[[i]], delta = mu[[i]], sigma = sigma[[i]], df = df[i], log = FALSE, ...)
  }, numeric(1L))
}

#' @export
log_density.dist_mvt <- function(x, at, ..., na.rm = FALSE) {
  require_package("mvtnorm")
  mu <- mv_field(x, "mu")
  sigma <- mv_field(x, "sigma")
  df <- x[["df"]]
  at <- mv_arg_rows(at, length(mu))
  vapply(seq_along(mu), function(i) {
    mvtnorm::dmvt(at[[i]], delta = mu[[i]], sigma = sigma[[i]], df = df[i], log = TRUE, ...)
  }, numeric(1L))
}

#' @export
quantile.dist_mvt <- function(x, p, kind = c("equicoordinate", "marginal"),
                               ..., na.rm = FALSE, tail = "lower") {
  kind <- match.arg(kind)
  mu <- mv_field(x, "mu")
  sigma <- mv_field(x, "sigma")
  df <- x[["df"]]
  n <- length(mu)
  if (kind == "marginal") {
    scale <- sqrt(mv_stack(lapply(sigma, diag)))
    return(stats::qt(p, df = df, lower.tail = tail == "lower") * scale + mv_stack(mu))
  }
  require_package("mvtnorm")
  if (tail == "upper") p <- 1 - p
  p <- rep_len(p, n)
  q <- vapply(seq_len(n), function(i) {
    if (is.na(p[i])) return(NA_real_)
    if (p[i] == 0) return(-Inf) else if (p[i] == 1) return(Inf)
    mvtnorm::qmvt(p[i], delta = mu[[i]], sigma = sigma[[i]], df = df[i], ...)$quantile
  }, numeric(1L))
  mv_stack(lapply(seq_len(n), function(i) rep(q[i], length(mu[[i]]))))
}

#' @export
cdf.dist_mvt <- function(x, q, ..., na.rm = FALSE, tail = "lower") {
  require_package("mvtnorm")
  mu <- mv_field(x, "mu")
  sigma <- mv_field(x, "sigma")
  df <- x[["df"]]
  q <- mv_arg_rows(q, length(mu))
  p <- vapply(seq_along(mu), function(i) {
    mvtnorm::pmvt(upper = as.numeric(q[[i]]), delta = mu[[i]], sigma = sigma[[i]], df = df[i], ...)[1]
  }, numeric(1L))
  # The upper tail of a multivariate distribution is the complement 1 - F(q)
  if (tail == "lower") p else 1 - p
}

#' @export
generate.dist_mvt <- function(x, times, ..., na.rm = FALSE) {
  require_package("mvtnorm")
  mu <- mv_field(x, "mu")
  sigma <- mv_field(x, "sigma")
  df <- x[["df"]]
  times <- rep_len(times, length(mu))
  lapply(seq_along(mu), function(i) {
    mvtnorm::rmvt(times[i], delta = mu[[i]], sigma = sigma[[i]], df = df[i], ...)
  })
}

#' @export
support.dist_mvt <- function(x, ...) {
  mv_support(mv_field(x, "mu"))
}

#' @export
mean.dist_mvt <- function(x, ...) {
  out <- mv_stack(mv_field(x, "mu"))
  undefined <- x[["df"]] <= 1
  if (any(undefined, na.rm = TRUE)) {
    warning("Mean is undefined for df <= 1")
    out[which(undefined), ] <- NA_real_
  }
  out
}

#' @export
median.dist_mvt <- function(x, ...) {
  mv_stack(mv_field(x, "mu"))
}

#' @export
covariance.dist_mvt <- function(x, ...) {
  df <- x[["df"]]
  if (any(df <= 2, na.rm = TRUE)) {
    warning("Covariance is undefined for df <= 2")
  }
  lapply(seq_along(df), function(i) {
    sigma <- unclass(mv_field(x, "sigma")[[i]])
    if (is.na(df[i]) || df[i] <= 2) sigma[] <- NA_real_ else sigma <- df[i] / (df[i] - 2) * sigma
    sigma
  })
}

#' @export
variance.dist_mvt <- function(x, ...) {
  mv_stack(lapply(covariance(x, ...), diag))
}

#' @export
dim.dist_mvt <- function(x) {
  length(mv_field(x, "mu")[[1L]])
}
