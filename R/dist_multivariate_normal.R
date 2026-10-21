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
dist_multivariate_normal <- function(mu = 0, sigma = list(diag(1))){
  new_dist(mu = mu, sigma = sigma,
           dimnames = colnames(sigma[[1]]), class = "dist_mvnorm")
}

#' @export
format.dist_mvnorm <- function(x, digits = 2, ...){
  sprintf("MVN[%i]", lengths(mv_field(x, "mu")))
}

# The list of `n` parameter vectors/matrices of a multivariate distribution.
# `x[["mu"]]` unwraps a size 1 list field, so the field is read directly.
mv_field <- function(x, name){
  as.list(vctrs::field(x, name))
}

# Rows of a recycled argument (`n` rows, or 1 row to be shared by all)
mv_arg_rows <- function(arg, n){
  arg <- as.matrix(arg)
  lapply(if (nrow(arg) == 1L) rep_len(1L, n) else seq_len(n), function(i) arg[i, ])
}

# Stack vectors as the rows of a matrix. Distributions of different
# dimensionality are padded with `NA` up to the largest dimension.
mv_stack <- function(rows){
  k <- max(lengths(rows), 0L)
  out <- matrix(NA_real_, nrow = length(rows), ncol = k)
  for (i in seq_along(rows)) out[i, seq_along(rows[[i]])] <- rows[[i]]
  out
}

mv_support <- function(mu){
  new_support_region(
    lapply(mu, function(m) {
      matrix(numeric(), nrow = 0L, ncol = length(m),
             dimnames = if (!is.null(names(m))) list(NULL, names(m)))
    }),
    lapply(mu, function(m) matrix(c(-Inf, Inf), nrow = 2L, ncol = length(m))),
    rep(list(c(FALSE, FALSE)), length(mu))
  )
}

#' @export
density.dist_mvnorm <- function(x, at, ..., na.rm = FALSE){
  require_package("mvtnorm")
  mu <- mv_field(x, "mu")
  sigma <- mv_field(x, "sigma")
  at <- mv_arg_rows(at, length(mu))
  vapply(seq_along(mu), function(i) {
    mvtnorm::dmvnorm(at[[i]], mu[[i]], sigma[[i]])
  }, numeric(1L))
}

#' @export
log_density.dist_mvnorm <- function(x, at, ..., na.rm = FALSE){
  require_package("mvtnorm")
  mu <- mv_field(x, "mu")
  sigma <- mv_field(x, "sigma")
  at <- mv_arg_rows(at, length(mu))
  vapply(seq_along(mu), function(i) {
    mvtnorm::dmvnorm(at[[i]], mu[[i]], sigma[[i]], log = TRUE)
  }, numeric(1L))
}

#' @export
quantile.dist_mvnorm <- function(x, p, kind = c("marginal", "equicoordinate"),
                                 ..., type = lifecycle::deprecated(), na.rm = FALSE,
                                 tail = "lower"){
  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_warn(
      "0.6.0", "quantile.dist_mvnorm(type)", "quantile.dist_mvnorm(kind)",
      # Force removal of the 'contact author' footer since it doesn't work here.
      user_env = .GlobalEnv
    )
    kind <- type
  }
  kind <- match.arg(kind)
  mu <- mv_field(x, "mu")
  sigma <- mv_field(x, "sigma")
  n <- length(mu)
  if (kind == "marginal") {
    return(stats::qnorm(
      p, mean = mv_stack(mu), sd = sqrt(mv_stack(lapply(sigma, diag))), ...,
      lower.tail = tail == "lower"
    ))
  }
  require_package("mvtnorm")
  if (tail == "upper") p <- 1 - p
  p <- rep_len(p, n)
  q <- vapply(seq_len(n), function(i) {
    if (is.na(p[i])) return(NA_real_)
    if (p[i] == 0) return(-Inf) else if (p[i] == 1) return(Inf)
    mvtnorm::qmvnorm(p[i], mean = mu[[i]], sigma = sigma[[i]], ...)$quantile
  }, numeric(1L))
  mv_stack(lapply(seq_len(n), function(i) rep(q[i], length(mu[[i]]))))
}

#' @export
cdf.dist_mvnorm <- function(x, q, ..., na.rm = FALSE, tail = "lower"){
  require_package("mvtnorm")
  mu <- mv_field(x, "mu")
  sigma <- mv_field(x, "sigma")
  q <- mv_arg_rows(q, length(mu))
  p <- vapply(seq_along(mu), function(i) {
    mvtnorm::pmvnorm(upper = as.numeric(q[[i]]), mean = mu[[i]], sigma = sigma[[i]], ...)[1]
  }, numeric(1L))
  # The upper tail of a multivariate distribution is the complement 1 - F(q)
  if (tail == "lower") p else 1 - p
}

#' @export
generate.dist_mvnorm <- function(x, times, ..., na.rm = FALSE){
  require_package("mvtnorm")
  mu <- mv_field(x, "mu")
  sigma <- mv_field(x, "sigma")
  times <- rep_len(times, length(mu))
  lapply(seq_along(mu), function(i) {
    mvtnorm::rmvnorm(times[i], mu[[i]], sigma[[i]], ...)
  })
}

#' @export
support.dist_mvnorm <- function(x, ...) {
  mv_support(mv_field(x, "mu"))
}

#' @export
mean.dist_mvnorm <- function(x, ...){
  mv_stack(mv_field(x, "mu"))
}

#' @export
median.dist_mvnorm <- mean.dist_mvnorm

#' @export
covariance.dist_mvnorm <- function(x, ...){
  lapply(mv_field(x, "sigma"), unclass)
}

#' @export
variance.dist_mvnorm <- function(x, ...){
  mv_stack(lapply(mv_field(x, "sigma"), diag))
}

#' @export
dim.dist_mvnorm <- function(x){
  length(mv_field(x, "mu")[[1L]])
}
