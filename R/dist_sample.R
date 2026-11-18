#' Sampling distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The sampling distribution represents an empirical distribution based on
#' observed samples. It is useful for bootstrapping, representing posterior
#' distributions from Markov Chain Monte Carlo (MCMC) algorithms, or working
#' with any empirical data where the parametric form is unknown. Unlike
#' parametric distributions, the sampling distribution makes no assumptions
#' about the underlying data-generating process and instead uses the sample
#' itself to estimate distributional properties. The distribution can handle
#' both univariate and multivariate samples.
#'
#' @param x A list of sampled values. For univariate distributions, each
#'   element should be a numeric vector. For multivariate distributions, each
#'   element should be a matrix where columns represent variables and rows
#'   represent observations.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_sample")`
#'
#'   In the following, let \eqn{X} be a random variable with sample
#'   \eqn{x_1, x_2, \ldots, x_n} of size \eqn{n}.
#'
#'   **Support**: The observed range of the sample
#'
#'   **Mean** (univariate):
#'
#'   \deqn{
#'     \bar{x} = \frac{1}{n} \sum_{i=1}^{n} x_i
#'   }{
#'     mean(x) = (1/n) sum(x_i)
#'   }
#'
#'   **Mean** (multivariate): Computed independently for each variable.
#'
#'   **Variance** (univariate):
#'
#'   \deqn{
#'     s^2 = \frac{1}{n-1} \sum_{i=1}^{n} (x_i - \bar{x})^2
#'   }{
#'     s^2 = (1/(n-1)) sum((x_i - mean(x))^2)
#'   }
#'
#'   **Covariance** (multivariate): The sample covariance matrix.
#'
#'   **Skewness** (univariate):
#'
#'   \deqn{
#'     g_1 = \frac{\sqrt{n} \sum_{i=1}^{n} (x_i - \bar{x})^3}{\left(\sum_{i=1}^{n} (x_i - \bar{x})^2\right)^{3/2}} \left(1 - \frac{1}{n}\right)^{3/2}
#'   }{
#'     g_1 = (sqrt(n) sum((x_i - mean(x))^3) / (sum((x_i - mean(x))^2)^(3/2))) * (1 - 1/n)^(3/2)
#'   }
#'
#'   **Probability density function**: Approximated numerically using
#'   kernel density estimation.
#'
#'   **Cumulative distribution function** (univariate):
#'
#'   \deqn{
#'     F(q) = \frac{1}{n} \sum_{i=1}^{n} I(x_i \leq q)
#'   }{
#'     F(q) = (1/n) sum(I(x_i <= q))
#'   }
#'
#'   where \eqn{I(\cdot)} is the indicator function.
#'
#'   **Cumulative distribution function** (multivariate):
#'
#'   \deqn{
#'     F(\mathbf{q}) = \frac{1}{n} \sum_{i=1}^{n} I(\mathbf{x}_i \leq \mathbf{q})
#'   }{
#'     F(q) = (1/n) sum(I(x_i <= q))
#'   }
#'
#'   where the inequality is applied element-wise.
#'
#'   **Quantile function** (univariate): The sample quantile, computed using
#'   the specified quantile type (see [stats::quantile()]).
#'
#'   **Quantile function** (multivariate): Marginal quantiles are computed
#'   independently for each variable.
#'
#'   **Random generation**: Bootstrap sampling with replacement from the
#'   empirical sample.
#'
#' @seealso [stats::density()], [stats::quantile()], [stats::cov()]
#'
#' @examples
#' # Univariate numeric samples
#' dist <- dist_sample(x = list(rnorm(100), rnorm(100, 10)))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' generate(dist, 10)
#'
#' density(dist, 1)
#'
#' # Multivariate numeric samples
#' dist <- dist_sample(x = list(cbind(rnorm(100), rnorm(100, 10))))
#' dimnames(dist) <- c("x", "y")
#'
#' dist
#' mean(dist)
#' variance(dist)
#' generate(dist, 10)
#' quantile(dist, 0.4) # Returns the marginal quantiles
#' cdf(dist, matrix(c(0.3,9), nrow = 1))
#'
#' @name dist_sample
#' @export
dist_sample <- function(x){
  vec_assert(x, list())
  x <- as_list_of(x, .ptype = vec_ptype(x[[1]]))
  new_dist(x = x, class = "dist_sample")
}

#' @export
format.dist_sample <- function(x, ...){
  # `vapply(x, ...)` over the record itself would iterate its *elements*
  # (distributions), not its "x" field's per-distribution sample vectors;
  # go via the field explicitly (bypassing the `$`/`[[` auto-unwrap shim,
  # which would instead flatten a length-1 `x` down to a bare vector).
  sprintf(
    "sample[%s]",
    vapply(x[["x"]], vctrs::vec_size, integer(1L))
  )
}

# `x[["x"]]` would auto-unwrap a length-1 `x` down to a bare sample vector

# Samples are ragged, so every method maps over the per-distribution samples
# in `x[["x"]]` (a `list_of`). Samples are multivariate when their elements are
# matrices (one column per variate).
sample_is_mv <- function(x) {
  is.matrix(x[["x"]][[1L]])
}

#' @export
density.dist_sample <- function(x, at, ..., na.rm = TRUE){
  if(sample_is_mv(x)) {
    abort("Multivariate sample density is not yet implemented.")
  }
  n <- vctrs::vec_size(x)
  at <- vctrs::vec_recycle(at, n)
  vapply(seq_len(n), function(i) {
    if (!is.finite(at[i])) return(0)
    stats::density(x[["x"]][[i]], from = at[i], to = at[i], n = 1, ..., na.rm = na.rm)$y
  }, numeric(1L))
}


#' @export
quantile.dist_sample <- function(x, p, type = 7, ..., na.rm = TRUE, tail = "lower"){
  n <- vctrs::vec_size(x)
  p <- vctrs::vec_recycle(p, n)
  if(tail == "upper") p <- 1 - p
  # Apply independently over sample variates
  if(sample_is_mv(x)) {
    # Marginal quantiles
    out <- lapply(seq_len(n), function(i) {
      apply(x[["x"]][[i]], 2, stats::quantile, probs = p[i], type = type, ..., na.rm = na.rm, names = FALSE)
    })
    return(do.call(rbind, out))
  }
  vapply(seq_len(n), function(i) {
    stats::quantile(x[["x"]][[i]], probs = p[i], type = type, ..., na.rm = na.rm, names = FALSE)
  }, numeric(1L))
}

#' @export
cdf.dist_sample <- function(x, q, ..., na.rm = TRUE, tail = "lower"){
  n <- vctrs::vec_size(x)
  mv <- sample_is_mv(x)
  q <- if (mv) q[if (nrow(q) == 1L) rep(1L, n) else seq_len(n), , drop = FALSE] else vctrs::vec_recycle(q, n)
  vapply(seq_len(n), function(i) {
    xi <- x[["x"]][[i]]
    if (mv) {
      p <- mean(sweep(xi, 2L, q[i, ], `<=`))
      if (tail == "lower") p else 1 - p
    } else if (tail == "upper") {
      mean(xi > q[i], ..., na.rm = na.rm)
    } else {
      mean(xi <= q[i], ..., na.rm = na.rm)
    }
  }, numeric(1L))
}

# The proportion of the sample equal to `at`. Equality is exact, as in `cdf()`
# (and `ecdf()`), so that `cdf()` and `point_mass()` agree.
#' @export
point_mass.dist_sample <- function(x, at, ..., na.rm = TRUE){
  at <- vctrs::vec_recycle(at, vctrs::vec_size(x))
  as.double(unlist(.mapply(function(s, a) {
    if (is.na(a)) NA_real_ else mean(s == a, na.rm = na.rm)
  }, list(x[["x"]], at), NULL)))
}

#' @export
generate.dist_sample <- function(x, times, ...){
  times <- vctrs::vec_recycle(times, vctrs::vec_size(x))
  mv <- sample_is_mv(x)
  lapply(seq_along(times), function(i) {
    xi <- x[["x"]][[i]]
    j <- sample.int(vctrs::vec_size(xi), size = times[i], replace = TRUE)
    if (mv) xi[j, , drop = FALSE] else xi[j]
  })
}

#' @export
mean.dist_sample <- function(x, ...){
  if(sample_is_mv(x)) {
    do.call(rbind, lapply(x[["x"]], function(xi) matrix(colMeans(xi, ...), nrow = 1L)))
  } else {
    vapply(x[["x"]], mean, numeric(1L), ...)
  }
}

#' @export
median.dist_sample <- function(x, na.rm = FALSE, ...){
  if(sample_is_mv(x))
    do.call(rbind, lapply(x[["x"]], function(xi) matrix(apply(xi, 2, stats::median, na.rm = na.rm, ...), nrow = 1L)))
  else
    vapply(x[["x"]], stats::median, numeric(1L), na.rm = na.rm, ...)
}

#' @export
covariance.dist_sample <- function(x, ...){
  if(sample_is_mv(x)) lapply(x[["x"]], stats::cov, ...) else vapply(x[["x"]], stats::var, numeric(1L), ...)
}

#' @export
skewness.dist_sample <- function(x, ..., na.rm = FALSE) {
  if(sample_is_mv(x)) {abort("Multivariate sample skewness is not yet implemented.")}
  vapply(x[["x"]], function(xi) {
    n <- length(xi)
    centered <- xi - mean(xi, na.rm = na.rm)
    sum_x2 <- sum(centered^2, na.rm = na.rm)
    sum_x3 <- sum(centered^3, na.rm = na.rm)
    sqrt(n) * sum_x3/(sum_x2^(3/2)) * ((1 - 1/n))^(3/2)
  }, numeric(1L))
}

#' @export
support.dist_sample <- function(x, ...) {
  xs <- as.list(x[["x"]])
  new_support_region(
    lapply(xs, vctrs::vec_init, n = 0L),
    lapply(xs, range),
    rep_len(list(rep(TRUE, 2)), length(xs))
  )
}

# Rebuild a `dist_sample` from a list of transformed samples
sample_from_list <- function(x) {
  vctrs::new_rcrd(
    list(x = vctrs::as_list_of(x, .ptype = vctrs::vec_ptype(x[[1L]]))),
    class = c("dist_sample", "dist_default")
  )
}

#' @method Math dist_sample
#' @export
Math.dist_sample <- function(x, ...) {
  f <- get(.Generic)
  sample_from_list(lapply(x[["x"]], f, ...))
}

#' @method Ops dist_sample
#' @export
Ops.dist_sample <- function(e1, e2) {
  n <- max(vctrs::vec_size(e1), vctrs::vec_size(e2))
  as_samples <- function(e) {
    if (inherits(e, "dist_sample")) as.list(e[["x"]])
    else vctrs::vec_chop(vctrs::vec_recycle(e, if (vctrs::vec_size(e) == 1L) 1L else n))[rep_len(seq_len(vctrs::vec_size(e)), n)]
  }
  e1 <- as_samples(e1)
  e2 <- as_samples(e2)
  f <- get(.Generic)
  sample_from_list(lapply(seq_len(n), function(i) f(e1[[i]], e2[[i]])))
}
