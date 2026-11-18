#' Missing distribution
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' A placeholder distribution for handling missing values in a vector of
#' distributions.
#'
#' @param length The number of missing distributions
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_missing")`
#'
#'   The missing distribution represents the absence of distributional
#'   information. It is used as a placeholder when distribution values are
#'   not available or not applicable, similar to how `NA` is used for missing
#'   scalar values.
#'
#'   **Support**: Undefined
#'
#'   **Mean**: \eqn{\text{NA}}
#'
#'   **Variance**: \eqn{\text{NA}}
#'
#'   **Skewness**: \eqn{\text{NA}}
#'
#'   **Kurtosis**: \eqn{\text{NA}}
#'
#'   **Probability density function (p.d.f)**: Undefined
#'
#'   \deqn{
#'     f(x) = \text{NA}
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**: Undefined
#'
#'   \deqn{
#'     F(t) = \text{NA}
#'   }
#'
#'   **Quantile function**: Undefined
#'
#'   \deqn{
#'     Q(p) = \text{NA}
#'   }
#'
#'   **Moment generating function (m.g.f)**: Undefined
#'
#'   \deqn{
#'     E(e^{tX}) = \text{NA}
#'   }
#'
#'   All statistical operations on missing distributions return `NA` values
#'   of appropriate length, propagating the missingness through calculations.
#'
#' @seealso [base::NA]
#'
#' @name dist_missing
#'
#' @examples
#' dist <- dist_missing(3L)
#'
#' dist
#' mean(dist)
#' variance(dist)
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
#' @export
dist_missing <- function(length = 1) {
  new_dist(placeholder = rep(NA_real_, length), class = "dist_na")
}

#' @export
format.dist_na <- function(x, ...) {
  rep_len("NA", vctrs::vec_size(x))
}

#' @export
density.dist_na <- function(x, at, ...){
  rep_len(NA_real_, vctrs::vec_size(x))
}

#' @export
log_density.dist_na <- density.dist_na

#' @export
quantile.dist_na <- function(x, p, ...){
  rep_len(NA_real_, vctrs::vec_size(x))
}
#' @export
log_quantile.dist_na <- quantile.dist_na

#' @export
cdf.dist_na <- function(x, q, ...){
  rep_len(NA_real_, vctrs::vec_size(x))
}
#' @export
log_cdf.dist_na <- cdf.dist_na

#' @export
generate.dist_na <- function(x, times, ...){
  lapply(times, function(n) rep(NA_real_, n))
}

#' @export
support.dist_na <- function(x, ...) {
  vctrs::vec_init(new_support_region(), vctrs::vec_size(x))
}

#' @export
mean.dist_na <- function(x, ...) rep_len(NA_real_, vctrs::vec_size(x))

#' @export
covariance.dist_na <- function(x, ...) rep_len(NA_real_, vctrs::vec_size(x))

#' @export
skewness.dist_na <- function(x, ...) rep_len(NA_real_, vctrs::vec_size(x))

#' @export
kurtosis.dist_na <- function(x, ...) rep_len(NA_real_, vctrs::vec_size(x))

#' @export
Math.dist_na <- function(x, ...) {
  x
}

#' @export
Ops.dist_na <- function(e1, e2) {
  n <- if (missing(e2)) vctrs::vec_size(e1) else max(vctrs::vec_size(e1), vctrs::vec_size(e2))
  dist_record(dist_missing(n))
}
