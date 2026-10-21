#' The Dirichlet distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Dirichlet distribution is a multivariate generalisation of the Beta
#' distribution. It is the conjugate prior of the Categorical and Multinomial
#' distributions, and describes a probability distribution over the
#' \eqn{(k-1)}-simplex — the set of \eqn{k}-dimensional vectors whose
#' components are non-negative and sum to one.
#'
#' @param alpha A list of positive numeric concentration vectors.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_dirichlet")`
#'
#'   In the following, let \eqn{\mathbf{X} = (X_1, \ldots, X_k)} be a
#'   Dirichlet random variable with concentration parameter
#'   `alpha` = \eqn{\boldsymbol{\alpha} = (\alpha_1, \ldots, \alpha_k)},
#'   where each \eqn{\alpha_i > 0}.
#'
#'   **Support**: \eqn{\mathbf{x}} on the \eqn{(k-1)}-simplex,
#'   i.e. \eqn{x_i \geq 0} and \eqn{\sum_{i=1}^k x_i = 1}.
#'
#'   **Mean**: \eqn{E(X_i) = \frac{\alpha_i}{\alpha_0}} where
#'   \eqn{\alpha_0 = \sum_{i=1}^k \alpha_i}.
#'
#'   **Variance**:
#'
#'   \deqn{
#'     \mathrm{Var}(X_i) = \frac{\alpha_i(\alpha_0 - \alpha_i)}{\alpha_0^2(\alpha_0 + 1)}
#'   }{
#'     Var(X_i) = alpha_i * (alpha_0 - alpha_i) / (alpha_0^2 * (alpha_0 + 1))
#'   }
#'
#'   **Covariance**:
#'
#'   \deqn{
#'     \mathrm{Cov}(X_i, X_j) = \frac{-\alpha_i \alpha_j}{\alpha_0^2(\alpha_0 + 1)}, \quad i \neq j
#'   }{
#'     Cov(X_i, X_j) = -alpha_i * alpha_j / (alpha_0^2 * (alpha_0 + 1)), i != j
#'   }
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(\mathbf{x}) = \frac{1}{B(\boldsymbol{\alpha})}
#'     \prod_{i=1}^k x_i^{\alpha_i - 1}
#'   }{
#'     f(x) = 1 / B(alpha) * prod(x_i^(alpha_i - 1))
#'   }
#'
#'   where \eqn{B(\boldsymbol{\alpha}) = \frac{\prod_{i=1}^k \Gamma(\alpha_i)}{\Gamma(\alpha_0)}}
#'   is the multivariate Beta function.
#'
#' @seealso [LaplacesDemon::ddirichlet()], [LaplacesDemon::rdirichlet()]
#'
#' @examples
#' dist <- dist_dirichlet(alpha = list(c(2, 5, 3)))
#' dist
#'
#' @examplesIf requireNamespace("LaplacesDemon", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, cbind(0.2, 0.5, 0.3))
#' density(dist, cbind(0.2, 0.5, 0.3), log = TRUE)
#'
#' @name dist_dirichlet
#' @export
dist_dirichlet <- function(alpha) {
  alpha <- as_list_of(as.list(alpha), .ptype = double())
  new_dist(alpha = alpha, class = "dist_dirichlet")
}

#' @export
format.dist_dirichlet <- function(x, digits = 2, ...) {
  sprintf(
    "Dirichlet[%i]",
    lengths(field(x, "alpha"))
  )
}

#' @export
density.dist_dirichlet <- function(x, at, ..., na.rm = FALSE) {
  require_package("LaplacesDemon")
  if (is.list(at)) return(vapply(at, density, numeric(1L), x = x, ...))
  vapply(seq_along(field(x, "alpha")), function(i) {
    LaplacesDemon::ddirichlet(as.numeric(multi_row(at, i)), field(x, "alpha")[[i]])
  }, numeric(1L))
}

#' @export
log_density.dist_dirichlet <- function(x, at, ..., na.rm = FALSE) {
  require_package("LaplacesDemon")
  if (is.list(at)) return(vapply(at, log_density, numeric(1L), x = x, ...))
  vapply(seq_along(field(x, "alpha")), function(i) {
    LaplacesDemon::ddirichlet(as.numeric(multi_row(at, i)), field(x, "alpha")[[i]], log = TRUE)
  }, numeric(1L))
}

#' @export
generate.dist_dirichlet <- function(x, times, ..., na.rm = FALSE) {
  require_package("LaplacesDemon")
  lapply(seq_along(field(x, "alpha")), function(i) {
    LaplacesDemon::rdirichlet(times[i], field(x, "alpha")[[i]])
  })
}

#' @export
mean.dist_dirichlet <- function(x, ...) {
  bind_ragged_rows(lapply(field(x, "alpha"), function(alpha) alpha / sum(alpha)))
}

#' @export
quantile.dist_dirichlet <- function(x, p, ..., tail = "lower") {
  # Marginal quantiles, as each component is Beta(alpha[i], alpha_0 - alpha[i])
  n <- vec_size(x)
  p <- vec_recycle(p, n)
  bind_ragged_rows(lapply(seq_len(n), function(i) {
    alpha <- field(x, "alpha")[[i]]
    stats::qbeta(p[i], alpha, sum(alpha) - alpha, lower.tail = tail == "lower")
  }))
}

#' @export
variance.dist_dirichlet <- function(x, ...) {
  bind_ragged_rows(lapply(field(x, "alpha"), function(alpha) {
    alpha_0 <- sum(alpha)
    alpha * (alpha_0 - alpha) / (alpha_0^2 * (alpha_0 + 1))
  }))
}

#' @export
covariance.dist_dirichlet <- function(x, ...) {
  lapply(field(x, "alpha"), function(alpha) {
    alpha_0 <- sum(alpha)
    sigma <- -tcrossprod(alpha) / (alpha_0^2 * (alpha_0 + 1))
    diag(sigma) <- alpha * (alpha_0 - alpha) / (alpha_0^2 * (alpha_0 + 1))
    sigma
  })
}

#' @export
support.dist_dirichlet <- function(x, ...) {
  alpha <- field(x, "alpha")
  new_support_region(
    lapply(alpha, function(a) matrix(rep(0, length(a)), nrow = 1)),
    rep_len(list(c(0, 1)), length(alpha)),
    rep_len(list(c(TRUE, TRUE)), length(alpha))
  )
}

#' @export
dim.dist_dirichlet <- function(x) {
  lengths(field(x, "alpha"))[1L]
}
