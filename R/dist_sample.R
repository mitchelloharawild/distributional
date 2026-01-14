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
  sprintf(
    "sample[%s]",
    vapply(x, vec_size, integer(1L))
  )
}

#' @export
density.dist_sample <- function(x, at, ..., na.rm = TRUE){
  # Apply independently over sample variates
  if(is.matrix(x$x)) {
    abort("Multivariate sample density is not yet implemented.")
  }
  z <- numeric(length(at))
  zi <- is.finite(at)
  at <- at[zi]
  zl <- vec_size(at)

  # Shortcut if only one point in density is needed
  if(zl == 1){
    z[zi] <- density(x[["x"]], from = at, to = at, n = 1)$y
  } else if (zl > 1) {
    d <- density(x[["x"]], from = min(at), to = max(at), ..., na.rm=na.rm)
    z[zi] <- stats::approx(d$x, d$y, xout = at)$y
  }
  z
}


#' @export
quantile.dist_sample <- function(x, p, type = "marginal", ..., na.rm = TRUE){
  type <- match.arg(type)
  # Apply independently over sample variates
  if(is.matrix(x$x)) {
    # Marginal quantiles
    return(
      matrix(apply(x$x, 2, quantile, p = p, ..., na.rm = na.rm), nrow = length(p))
    )
  }
  quantile(x$x, probs = p, ..., na.rm = na.rm, names = FALSE)
}

#' @export
cdf.dist_sample <- function(x, q, ..., na.rm = TRUE){
  if(vec_size(q) > 1) return(vapply(q, cdf, numeric(1L), x = x, ...))
  if(is.matrix(x$x)) {
    return(mean(x$x <= vec_recycle(q, vec_size(x$x))))
  }
  mean(x <= q, ..., na.rm = na.rm)
  # vapply(x, function(x, q) mean(x <= q, ..., na.rm = na.rm), numeric(1L), q = q)
}

#' @export
generate.dist_sample <- function(x, times, ...){
  i <- sample.int(vec_size(x[["x"]]), size = times, replace = TRUE)
  if(is.matrix(x$x)) x$x[i,,drop = FALSE] else x$x[i]
}

#' @export
mean.dist_sample <- function(x, ...){
  if(is.matrix(x$x)) {
    matrix(colMeans(x$x, ...), nrow = 1L)
  } else {
    mean(x$x, ...)
  }
}

#' @export
median.dist_sample <- function(x, na.rm = FALSE, ...){
  if(is.matrix(x$x))
    matrix(apply(x$x, 2, median, na.rm = na.rm, ...), nrow = 1L)
  else
    median(x$x, na.rm = na.rm, ...)
}

#' @export
covariance.dist_sample <- function(x, ...){
  if(is.matrix(x$x)) stats::cov(x$x, ...) else stats::var(x$x, ...)
}

#' @export
skewness.dist_sample <- function(x, ..., na.rm = FALSE) {
  if(is.matrix(x$x)) {abort("Multivariate sample skewness is not yet implemented.")}
  n <- lengths(x, use.names = FALSE)
  x <- lapply(x, function(.) . - mean(., na.rm = na.rm))
  sum_x2 <- vapply(x, function(.) sum(.^2, na.rm = na.rm), numeric(1L), USE.NAMES = FALSE)
  sum_x3 <- vapply(x, function(.) sum(.^3, na.rm = na.rm), numeric(1L), USE.NAMES = FALSE)
  y <- sqrt(n) * sum_x3/(sum_x2^(3/2))
  y * ((1 - 1/n))^(3/2)
}

#' @export
support.dist_sample <- function(x, ...) {
  new_support_region(
    list(vctrs::vec_init(x$x, n = 0L)),
    list(range(x$x)),
    list(rep(TRUE, 2))
  )
}

#' @method Math dist_sample
#' @export
Math.dist_sample <- function(x, ...) {
  x <- .mapply(.Generic, list(parameters(x)$x, ...), MoreArgs = NULL)
  names(x) <- "x"
  enclass_dist(x, "dist_sample")
}

#' @method Ops dist_sample
#' @export
Ops.dist_sample <- function(e1, e2) {
  is_dist <- c(inherits(e1, "dist_sample"), inherits(e2, "dist_sample"))
  if(is_dist[1]) {
    e1 <- parameters(e1)$x
  }
  if(is_dist[2]) {
    e2 <- parameters(e2)$x
  }

  x <- .mapply(.Generic, list(e1, e2), MoreArgs = NULL)
  names(x) <- "x"
  enclass_dist(x, "dist_sample")
}
