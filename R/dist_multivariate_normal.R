#' The multivariate normal distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The multivariate normal distribution is a generalization of the univariate
#' normal distribution to higher dimensions. It is widely used in multivariate
#' statistics and describes the joint distribution of multiple correlated
#' continuous random variables.
#'
#' @param mu A list of numeric vectors for the distribution's mean.
#' @param sigma A list of matrices for the distribution's variance-covariance matrix.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_multivariate_normal")`
#'
#'   In the following, let \eqn{\mathbf{X}} be a \eqn{k}-dimensional multivariate
#'   normal random variable with mean vector `mu` = \eqn{\boldsymbol{\mu}} and
#'   variance-covariance matrix `sigma` = \eqn{\boldsymbol{\Sigma}}.
#'
#'   **Support**: \eqn{\mathbf{x} \in \mathbb{R}^k}
#'
#'   **Mean**: \eqn{\boldsymbol{\mu}}
#'
#'   **Variance-covariance matrix**: \eqn{\boldsymbol{\Sigma}}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(\mathbf{x}) = \frac{1}{(2\pi)^{k/2} |\boldsymbol{\Sigma}|^{1/2}}
#'     \exp\left(-\frac{1}{2}(\mathbf{x} - \boldsymbol{\mu})^T
#'     \boldsymbol{\Sigma}^{-1}(\mathbf{x} - \boldsymbol{\mu})\right)
#'   }{
#'     f(x) = 1 / ((2\pi)^(k/2) |\Sigma|^(1/2))
#'     exp(-1/2 (x - \mu)^T \Sigma^(-1) (x - \mu))
#'   }
#'
#'   where \eqn{|\boldsymbol{\Sigma}|}{|\Sigma|} is the determinant of
#'   \eqn{\boldsymbol{\Sigma}}{\Sigma}.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(\mathbf{X} \le \mathbf{q}) = P(X_1 \le q_1, \ldots, X_k \le q_k)
#'   }{
#'     P(X \le q) = P(X_1 \le q_1, ..., X_k \le q_k)
#'   }
#'
#'   The c.d.f. does not have a closed-form expression and is computed numerically.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     M(\mathbf{t}) = E(e^{\mathbf{t}^T \mathbf{X}}) =
#'     \exp\left(\mathbf{t}^T \boldsymbol{\mu} + \frac{1}{2}\mathbf{t}^T
#'     \boldsymbol{\Sigma} \mathbf{t}\right)
#'   }{
#'     M(t) = E(e^(t^T X)) = exp(t^T \mu + 1/2 t^T \Sigma t)
#'   }
#'
#' @seealso [mvtnorm::dmvnorm()], [mvtnorm::pmvnorm()], [mvtnorm::qmvnorm()],
#'   [mvtnorm::rmvnorm()]
#'
#' @examples
#' dist <- dist_multivariate_normal(mu = list(c(1,2)), sigma = list(matrix(c(4,2,2,3), ncol=2)))
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
#' quantile(dist, 0.7, kind = "equicoordinate")
#' quantile(dist, 0.7, kind = "marginal")
#'
#' @export
dist_multivariate_normal <- function(mu = 0, sigma = diag(1)){
  new_dist(mu = mu, sigma = sigma,
           dimnames = colnames(sigma[[1]]), class = "dist_mvnorm")
}

#' @export
format.dist_mvnorm <- function(x, digits = 2, ...){
  sprintf(
    "MVN[%i]",
    length(x[["mu"]])
  )
}

#' @export
density.dist_mvnorm <- function(x, at, ..., na.rm = FALSE){
  require_package("mvtnorm")
  if(is.list(at)) return(vapply(at, density, numeric(1L), x = x, ...))
  mvtnorm::dmvnorm(at, x[["mu"]], x[["sigma"]])
}

#' @export
log_density.dist_mvnorm <- function(x, at, ..., na.rm = FALSE){
  require_package("mvtnorm")
  if(is.list(at)) return(vapply(at, log_density, numeric(1L), x = x, ...))
  mvtnorm::dmvnorm(at, x[["mu"]], x[["sigma"]], log = TRUE)
}

#' @export
quantile.dist_mvnorm <- function(x, p, kind = c("marginal", "equicoordinate"),
                                 ..., type = lifecycle::deprecated(), na.rm = FALSE){
  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_warn(
      "0.6.0", "quantile.dist_mvnorm(type)", "quantile.dist_mvnorm(kind)",
      # Force removal of the 'contact author' footer since it doesn't work here.
      user_env = .GlobalEnv
    )
    kind <- type
  }
  kind <- match.arg(kind)
  q <- if (kind == "marginal") {
    stats::qnorm(p, mean = rep(x[["mu"]], each = length(p)),
                 sd = rep(sqrt(diag(x[["sigma"]])), each = length(p)), ...)
  } else {
    require_package("mvtnorm")
    vapply(p, function(p, ...) {
      if (p == 0) return(-Inf) else if (p == 1) return(Inf)
      mvtnorm::qmvnorm(p, ...)$quantile
    }, numeric(1L), mean = x[["mu"]], sigma = x[["sigma"]], ...)
  }

  matrix(q, nrow = length(p), ncol = dim(x))
}

#' @export
cdf.dist_mvnorm <- function(x, q, ..., na.rm = FALSE){
  if(is.list(q)) return(vapply(q, cdf, numeric(1L), x = x, ...))
  require_package("mvtnorm")
  mvtnorm::pmvnorm(upper = as.numeric(q), mean = x[["mu"]], sigma = x[["sigma"]], ...)[1]
}

#' @export
generate.dist_mvnorm <- function(x, times, ..., na.rm = FALSE){
  require_package("mvtnorm")
  mvtnorm::rmvnorm(times, x[["mu"]], x[["sigma"]], ...)
}

#' @export
mean.dist_mvnorm <- function(x, ...){
  matrix(x[["mu"]], nrow = 1)
}

#' @export
median.dist_mvnorm <- mean.dist_mvnorm

#' @export
covariance.dist_mvnorm <- function(x, ...){
  # Wrap in list to preserve matrix structure
  list(x[["sigma"]])
}

#' @export
dim.dist_mvnorm <- function(x){
  length(x[["mu"]])
}
