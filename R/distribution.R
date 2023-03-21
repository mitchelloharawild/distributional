#' Create a new distribution
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' Allows extension package developers to define a new distribution class
#' compatible with the distributional package.
#'
#' @param ... Parameters of the distribution (named).
#' @param class The class of the distribution for S3 dispatch.
#' @param dimnames The names of the variables in the distribution (optional).
#'
#' @export
new_dist <- function(..., class = NULL, dimnames = NULL){
  args <- transpose(vctrs::vec_recycle_common(...))
  wrap_dist(
    lapply(args, enclass_dist, class = class),
    dimnames = dimnames
  )
}

enclass_dist <- function(x, class) {
  structure(x, class = c(class, "dist_default"))
}


wrap_dist <- function(x, dimnames = NULL){
  vctrs::new_vctr(x, vars = dimnames, class = "distribution")
}

#' @export
vec_ptype_abbr.distribution <- function(x, ...){
  "dist"
}

#' @export
format.distribution <- function(x, ...){
  x <- vec_data(x)
  out <- vapply(x, format, character(1L), ...)
  out[vapply(x, is.null, logical(1L))] <- "NA"
  out
}

#' @export
`dimnames<-.distribution` <- function(x, value){
  attr(x, "vars") <- value
  x
}
#' @export
dimnames.distribution <- function(x){
  attr(x, "vars")
}

#' @export
`[[.distribution` <- `[`

#' The probability density/mass function
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Computes the probability density function for a continuous distribution, or
#' the probability mass function for a discrete distribution.
#'
#' @param x The distribution(s).
#' @param at The point at which to compute the density/mass.
#' @param ... Additional arguments passed to methods.
#' @param log If `TRUE`, probabilities will be given as log probabilities.
#'
#' @importFrom stats density
#' @export
density.distribution <- function(x, at, ..., log = FALSE){
  if(log) return(log_density(x, at, ...))
  at <- arg_listable(at, .ptype = NULL)
  dist_apply(x, density, at = at, ...)
}

log_density <- function(x, at, ...) {
  UseMethod("log_density")
}
#' @export
log_density.distribution <- function(x, at, ...){
  at <- arg_listable(at, .ptype = NULL)
  dist_apply(x, log_density, at = at, ...)
}

#' Distribution Quantiles
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Computes the quantiles of a distribution.
#'
#' @inheritParams density.distribution
#' @param p The probability of the quantile.
#' @param ... Additional arguments passed to methods.
#'
#' @importFrom stats quantile
#' @export
quantile.distribution <- function(x, p, ..., log = FALSE){
  if(log) return(log_quantile(x, p, ...))
  p <- arg_listable(p, .ptype = double())
  dist_apply(x, quantile, p = p, ...)
}
log_quantile <- function(x, p, ...) {
  UseMethod("log_quantile")
}
#' @export
log_quantile.distribution <- function(x, p, ...){
  vec_assert(q, double(), 1L)
  p <- arg_listable(p, .ptype = double())
  dist_apply(x, log_quantile, p = p, ...)
}

#' The cumulative distribution function
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams density.distribution
#' @param q The quantile at which the cdf is calculated.
#'
#' @name cdf
#' @export
cdf <- function (x, q, ..., log = FALSE){
  if(log) return(log_cdf(x, q, ...))
  UseMethod("cdf")
}
#' @rdname cdf
#' @export
cdf.distribution <- function(x, q, ...){
  q <- arg_listable(q, .ptype = NULL)
  dist_apply(x, cdf, q = q, ...)
}
log_cdf <- function(x, q, ...) {
  UseMethod("log_cdf")
}
#' @export
log_cdf.distribution <- function(x, q, ...){
  q <- arg_listable(q, .ptype = NULL)
  dist_apply(x, log_cdf, q = q, ...)
}

#' Randomly sample values from a distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Generate random samples from probability distributions.
#'
#' @param x The distribution(s).
#' @param times The number of samples.
#' @param ... Additional arguments used by methods.
#'
#' @export
generate.distribution <- function(x, times, ...){
  times <- vec_cast(times, integer())
  times <- vec_recycle(times, size = length(x))
  x <- vec_data(x)
  dist_is_na <- vapply(x, is.null, logical(1L))
  x[dist_is_na] <- list(structure(list(), class = c("dist_na", "dist_default")))
  mapply(generate, x, times = times, ..., SIMPLIFY = FALSE)
  # dist_apply(x, generate, times = times, ...)
  # Needs work to structure MV appropriately.
}

#' The (log) likelihood of a sample matching a distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @name likelihood
#' @export
likelihood <- function (x, ...){
  UseMethod("likelihood")
}

#' @rdname likelihood
#' @param sample A list of sampled values to compare to distribution(s).
#' @param log If `TRUE`, the log-likelihood will be computed.
#' @export
likelihood.distribution <- function(x, sample, ..., log = FALSE){
  if(vec_is(sample, numeric())) {
    warn("The `sample` argument of `likelihood()` should contain a list of numbers.
The same sample will be used for each distribution, i.e. `sample = list(sample)`.")
    sample <- list(sample)
  }
  if(log){
    dist_apply(x, log_likelihood, sample = sample, ...)
  } else {
    dist_apply(x, likelihood, sample = sample, ...)
  }
}

#' @rdname likelihood
#' @export
log_likelihood <- function(x, ...) {
  UseMethod("log_likelihood")
}
#' @export
log_likelihood.distribution <- function(x, sample, ...){
  dist_apply(x, log_likelihood, sample = sample, ...)
}

#' Extract the parameters of a distribution
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @name parameters
#' @examples
#' dist <- c(
#'   dist_normal(1:2),
#'   dist_poisson(3),
#'   dist_multinomial(size = c(4, 3),
#'   prob = list(c(0.3, 0.5, 0.2), c(0.1, 0.5, 0.4)))
#'   )
#' parameters(dist)
#' @export
parameters <- function(x, ...) {
  UseMethod("parameters")
}

#' @rdname parameters
#' @export
parameters.distribution <- function(x, ...) {
  x <- lapply(vec_data(x), parameters)
  x <- lapply(x, function(z) data_frame(!!!z, .name_repair = "minimal"))
  vec_rbind(!!!x)
}

#' Extract the name of the distribution family
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param object The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @examples
#' dist <- c(
#'   dist_normal(1:2),
#'   dist_poisson(3),
#'   dist_multinomial(size = c(4, 3),
#'   prob = list(c(0.3, 0.5, 0.2), c(0.1, 0.5, 0.4)))
#'   )
#' family(dist)
#'
#' @importFrom stats family
#' @export
family.distribution <- function(object, ...) {
  vapply(vec_data(object), family, character(1L))
}

#' Region of support of a distribution
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @name support
#' @export
support <- function(x, ...) {
  UseMethod("support")
}

#' @rdname support
#' @export
support.distribution <- function(x, ...) {
  dist_apply(x, support, ...)
}

#' Mean of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns the empirical mean of the probability distribution. If the method
#' does not exist, the mean of a random sample will be returned.
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
mean.distribution <- function(x, ...){
  dist_apply(x, mean, ...)
}

#' Variance
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A generic function for computing the variance of an object.
#'
#' @param x An object.
#' @param ... Additional arguments used by methods.
#'
#' @details
#'
#' The implementation of `variance()` for numeric variables coerces the input to
#' a vector then uses [`stats::var()`] to compute the variance. This means that,
#' unlike [`stats::var()`], if `variance()` is passed a matrix or a 2-dimensional
#' array, it will still return the variance ([`stats::var()`] returns the
#' covariance matrix in that case).
#'
#' @seealso [`variance.distribution()`], [`covariance()`]
#'
#' @export
variance <- function(x, ...){
  UseMethod("variance")
}
#' @export
variance.default <- function(x, ...){
  stop(
    "The variance() method is not supported for objects of type ",
    paste(deparse(class(x)), collapse = "")
  )
}
#' @rdname variance
#' @export
variance.numeric <- function(x, ...){
  stats::var(as.vector(x), ...)
}
#' @rdname variance
#' @export
variance.matrix <- function(x, ...){
  diag(stats::cov(x, ...))
}

#' Variance of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns the empirical variance of the probability distribution. If the method
#' does not exist, the variance of a random sample will be returned.
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
variance.distribution <- function(x, ...){
  dist_apply(x, variance, ...)
}

#' Covariance
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A generic function for computing the covariance of an object.
#'
#' @param x An object.
#' @param ... Additional arguments used by methods.
#'
#' @seealso [`covariance.distribution()`], [`variance()`]
#'
#' @export
covariance <- function(x, ...){
  UseMethod("covariance")
}
#' @export
covariance.default <- function(x, ...){
  stop(
    "The covariance() method is not supported for objects of type ",
    paste(deparse(class(x)), collapse = "")
  )
}
#' @rdname variance
#' @export
covariance.numeric <- function(x, ...){
  stats::cov(x, ...)
}
#' Covariance of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns the empirical covariance of the probability distribution. If the
#' method does not exist, the covariance of a random sample will be returned.
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
covariance.distribution <- function(x, ...){
  dist_apply(x, covariance, ...)
}

#' Skewness of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
skewness <- function(x, ...) {
  UseMethod("skewness")
}
#' @rdname skewness
#' @export
skewness.distribution <- function(x, ...){
  dist_apply(x, skewness, ...)
}

#' Kurtosis of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
kurtosis <- function(x, ...) {
  UseMethod("kurtosis")
}
#' @rdname kurtosis
#' @export
kurtosis.distribution <- function(x, ...){
  dist_apply(x, kurtosis, ...)
}

#' Median of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns the median (50th percentile) of a probability distribution. This is
#' equivalent to `quantile(x, p=0.5)`.
#'
#' @param x The distribution(s).
#' @param na.rm Unused, included for consistency with the generic function.
#' @param ... Additional arguments used by methods.
#'
#' @importFrom stats median
#' @export
median.distribution <- function(x, na.rm = FALSE, ...){
  dist_apply(x, median, na.rm = na.rm, ...)
}

#' Probability intervals of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns a `hilo` central probability interval with probability coverage of
#' `size`. By default, the distribution's [`quantile()`] will be used to compute
#' the lower and upper bound for a centered interval
#'
#' @param x The distribution(s).
#' @param size The size of the interval (between 0 and 100).
#' @param ... Additional arguments used by methods.
#'
#' @seealso [`hdr.distribution()`]
#'
#' @importFrom stats median
#' @export
hilo.distribution <- function(x, size = 95, ...){
  size <- arg_listable(size, .ptype = double())
  dist_apply(x, hilo, size = size, ...)
}

#' Highest density regions of probability distributions
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' This function is highly experimental and will change in the future. In
#' particular, improved functionality for object classes and visualisation tools
#' will be added in a future release.
#'
#' Computes minimally sized probability intervals highest density regions.
#'
#' @param x The distribution(s).
#' @param size The size of the interval (between 0 and 100).
#' @param n The resolution used to estimate the distribution's density.
#' @param ... Additional arguments used by methods.
#'
#' @export
hdr.distribution <- function(x, size = 95, n = 512, ...){
  size <- arg_listable(size, .ptype = double())
  dist_apply(x, hdr, size = size, n = n, ...)
}

#' @export
sum.distribution <- function(x, ...){
  vec_restore(list(Reduce("+", x)), x)
}

#' @method vec_arith distribution
#' @export
vec_arith.distribution <- function(op, x, y, ...){
  UseMethod("vec_arith.distribution", y)
}
#' @method vec_arith.distribution default
#' @export
vec_arith.distribution.default <- function(op, x, y, ...){
  dist_is_na <- vapply(x, is.null, logical(1L))
  x[dist_is_na] <- list(structure(list(), class = c("dist_na", "dist_default")))
  if(is_empty(y)){
    out <- lapply(vec_data(x), get(op))
  }
  else {
    x <- vec_recycle_common(x = x, y = y)
    y <- x[["y"]]
    if(is_distribution(y)) y <- vec_data(y)
    x <- x[["x"]]
    out <- mapply(get(op), x = vec_data(x), y = y, SIMPLIFY = FALSE)
  }
  vec_restore(out, x)
}

#' @method vec_arith.numeric distribution
#' @export
vec_arith.numeric.distribution <- function(op, x, y, ...){
  x <- vec_recycle_common(x = x, y = y)
  y <- x[["y"]]
  x <- x[["x"]]
  out <- mapply(get(op), x = x, y = vec_data(y), SIMPLIFY = FALSE)
  vec_restore(out, y)
}

#' @method vec_math distribution
#' @export
vec_math.distribution <- function(.fn, .x, ...) {
  if(.fn %in% c("is.nan", "is.infinite")) return(rep_len(FALSE, length(.x)))
  if(.fn == "is.finite") return(rep_len(TRUE, length(.x)))
  out <- lapply(vec_data(.x), get(.fn), ...)
  vec_restore(out, .x)
}

#' @export
vec_ptype2.distribution.distribution <- function(x, y, ...){
  if(!identical(dimnames(x), dimnames(y))){
    abort("Distributions must have the same `dimnames` to be combined.")
  }
  x
}
#' @export
vec_ptype2.double.distribution <- function(x, y, ...) new_dist()
#' @export
vec_ptype2.distribution.double <- function(x, y, ...) new_dist()
#' @export
vec_ptype2.integer.distribution <- function(x, y, ...) new_dist()
#' @export
vec_ptype2.distribution.integer <- function(x, y, ...) new_dist()

#' @export
vec_cast.distribution.distribution <- function(x, to, ...){
  dimnames(x) <- dimnames(to)
  x
}
#' @export
vec_cast.distribution.double <- function(x, to, ...){
  x <- dist_degenerate(x)
  dimnames(x) <- dimnames(to)
  x
}
#' @export
vec_cast.distribution.integer <- vec_cast.distribution.double
#' @export
vec_cast.character.distribution <- function(x, to, ...){
  format(x)
}

#' Test if the object is a distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function returns `TRUE` for distributions and `FALSE` for all other objects.
#'
#' @param x An object.
#'
#' @return TRUE if the object inherits from the distribution class.
#' @rdname is-distribution
#' @examples
#' dist <- dist_normal()
#' is_distribution(dist)
#' is_distribution("distributional")
#' @export
is_distribution <- function(x) {
    inherits(x, "distribution")
}
