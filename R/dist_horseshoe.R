#' The Horseshoe distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The horseshoe distribution (Carvalho et al., 2008) is a heavy-tailed
#' continuous distribution defined as a scale mixture of normals. It is
#' primarily used as a shrinkage prior in sparse Bayesian regression, where
#' it concentrates mass near zero while retaining heavy tails that leave
#' large signals unshrunk.
#'
#' @param lambda A positive numeric vector of local scale parameters
#'   \eqn{\lambda > 0} (one per observation).
#' @param tau A positive scalar global scale parameter \eqn{\tau > 0}.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_horseshoe")`
#'
#'   In the following, let \eqn{X} be a horseshoe random variable with local
#'   scale parameter `lambda` = \eqn{\lambda > 0} and global scale parameter
#'   `tau` = \eqn{\tau > 0}.
#'
#'   **Support**: \eqn{x \in \mathbb{R}}, the set of all real numbers.
#'
#'   **Mean**: \eqn{E(X)} — not available in closed form.
#'
#'   **Variance**: \eqn{\mathrm{Var}(X)} — not available in closed form.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   The horseshoe density does not have a simple closed form but can be
#'   expressed as a scale mixture:
#'
#'   \deqn{
#'     X \mid \lambda, \tau \sim \mathcal{N}(0,\, \lambda^2 \tau^2)
#'   }{
#'     X | lambda, tau ~ N(0, lambda^2 * tau^2)
#'   }
#'
#'   where the half-Cauchy hyperprior \eqn{\lambda \sim C^+(0, 1)} induces the
#'   characteristic horseshoe shrinkage behaviour.
#'
#' @references
#'   Carvalho, C.M., Polson, N.G., and Scott, J.G. (2008).
#'   "The Horseshoe Estimator for Sparse Signals".
#'   *Discussion Paper 2008-31*. Duke University Department of Statistical
#'   Science.
#'
#'   Carvalho, C.M., Polson, N.G., and Scott, J.G. (2009).
#'   "Handling Sparsity via the Horseshoe".
#'   *Journal of Machine Learning Research*, 5, p. 73–80.
#'
#' @seealso [LaplacesDemon::dhs()], [LaplacesDemon::rhs()]
#'
#' @examples
#' dist <- dist_horseshoe(lambda = c(0.5, 1, 2), tau = 1)
#' dist
#'
#' @examplesIf requireNamespace("LaplacesDemon", quietly = TRUE)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, 0)
#' density(dist, 0, log = TRUE)
#'
#' @name dist_horseshoe
#' @export
dist_horseshoe <- function(lambda, tau) {
  lambda <- vec_cast(lambda, double())
  tau <- vec_cast(tau, double())
  if (any(lambda <= 0, na.rm = TRUE)) {
    abort("The local scale parameter `lambda` of a horseshoe distribution must be positive.")
  }
  if (any(tau <= 0, na.rm = TRUE)) {
    abort("The global scale parameter `tau` of a horseshoe distribution must be positive.")
  }
  new_dist(lambda = lambda, tau = tau, class = "dist_horseshoe")
}

#' @export
format.dist_horseshoe <- function(x, digits = 2, ...) {
  sprintf(
    "HS(%s, %s)",
    format(x[["lambda"]], digits = digits, ...),
    format(x[["tau"]], digits = digits, ...)
  )
}

#' @export
density.dist_horseshoe <- function(x, at, ..., na.rm = FALSE) {
  require_package("LaplacesDemon")
  LaplacesDemon::dhs(at, x[["lambda"]], x[["tau"]])
}

#' @export
log_density.dist_horseshoe <- function(x, at, ..., na.rm = FALSE) {
  require_package("LaplacesDemon")
  LaplacesDemon::dhs(at, x[["lambda"]], x[["tau"]], log = TRUE)
}

#' @export
generate.dist_horseshoe <- function(x, times, ..., na.rm = FALSE) {
  require_package("LaplacesDemon")
  vec_chop(
    LaplacesDemon::rhs(sum(times), lambda = rep(x[["lambda"]], times), tau = rep(x[["tau"]], times)),
    sizes = times
  )
}

#' @export
support.dist_horseshoe <- function(x, ...) {
  n <- vec_size(x)
  new_support_region(
    rep_len(list(numeric()), n),
    rep_len(list(c(-Inf, Inf)), n),
    rep_len(list(c(FALSE, FALSE)), n)
  )
}
