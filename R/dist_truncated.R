#' Truncate a distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Note that the samples are generated using inverse transform sampling, and the
#' means and variances are estimated from samples.
#'
#' @param dist The distribution(s) to truncate.
#' @param lower,upper The range of values to keep from a distribution.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_truncated")`
#'
#'   In the following, let \eqn{X} be a truncated random variable with 
#'   underlying distribution \eqn{Y}, truncation bounds `lower` = \eqn{a} and 
#'   `upper` = \eqn{b}, where \eqn{F_Y(x)} is the c.d.f. of \eqn{Y} and 
#'   \eqn{f_Y(x)} is the p.d.f. of \eqn{Y}. Let \eqn{F_Y(a^-) = P(Y < a)} be
#'   the left limit of the c.d.f. at \eqn{a}, so that the mass at the lower
#'   bound is retained. For continuous \eqn{Y} this equals \eqn{F_Y(a)}, and
#'   for \eqn{Y} supported on the integers it is
#'   \eqn{F_Y(\lceil a \rceil - 1)}.
#'
#'   **Support**: \eqn{[a, b]}
#'
#'   **Mean**: For the general case, the mean is approximated numerically.
#'   For a truncated Normal distribution with underlying mean \eqn{\mu} and 
#'   standard deviation \eqn{\sigma}, the mean is:
#'
#'   \deqn{
#'     E(X) = \mu + \frac{\phi(\alpha) - \phi(\beta)}{\Phi(\beta) - \Phi(\alpha)} \sigma
#'   }{
#'     E(X) = mu + (phi(alpha) - phi(beta)) / (Phi(beta) - Phi(alpha)) * sigma
#'   }
#'
#'   where \eqn{\alpha = (a - \mu)/\sigma}, \eqn{\beta = (b - \mu)/\sigma},
#'   \eqn{\phi} is the standard Normal p.d.f., and \eqn{\Phi} is the 
#'   standard Normal c.d.f.
#'
#'   **Variance**: Approximated numerically for all distributions.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \begin{cases}
#'       \frac{f_Y(x)}{F_Y(b) - F_Y(a^-)} & \text{if } a \le x \le b \\
#'       0 & \text{otherwise}
#'     \end{cases}
#'   }{
#'     f(x) = f_Y(x) / (F_Y(b) - F_Y(a-)) if a <= x <= b, 0 otherwise
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F(x) = \begin{cases}
#'       0 & \text{if } x < a \\
#'       \frac{F_Y(x) - F_Y(a^-)}{F_Y(b) - F_Y(a^-)} & \text{if } a \le x \le b \\
#'       1 & \text{if } x > b
#'     \end{cases}
#'   }{
#'     F(x) = 0 if x < a, (F_Y(x) - F_Y(a-)) / (F_Y(b) - F_Y(a-)) if a <= x <= b, 1 if x > b
#'   }
#'
#'   **Quantile function**:
#'
#'   \deqn{
#'     Q(p) = F_Y^{-1}(F_Y(a^-) + p(F_Y(b) - F_Y(a^-)))
#'   }{
#'     Q(p) = F_Y^(-1)(F_Y(a-) + p(F_Y(b) - F_Y(a-)))
#'   }
#'
#'   clamped to the interval \eqn{[a, b]}.
#'
#' @name dist_truncated
#'
#' @examples
#' dist <- dist_truncated(dist_normal(2,1), lower = 0)
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
#' if(requireNamespace("ggdist")) {
#' library(ggplot2)
#' ggplot() +
#'   ggdist::stat_dist_halfeye(
#'     aes(y = c("Normal", "Truncated"),
#'         dist = c(dist_normal(2,1), dist_truncated(dist_normal(2,1), lower = 0)))
#'   )
#' }
#'
#' @export
dist_truncated <- function(dist, lower = -Inf, upper = Inf){
  vec_is(dist, new_dist())
  vec_is(lower, numeric())
  vec_is(upper, numeric())
  if(any(lower >= upper)){
    abort("The `lower` truncation bound must be lower than the `upper` bound.")
  }
  new_dist(dist = dist, lower = lower, upper = upper,
           dimnames = dimnames(dist), class = "dist_truncated")
}

#' @export
format.dist_truncated <- function(x, ...){
  lwr_bracket <- if (is.finite(x[["lower"]])) "[" else "("
  upr_bracket <- if (is.finite(x[["upper"]])) "]" else ")"
  sprintf(
    "%s%s%s,%s%s",
    format(x[["dist"]]),
    lwr_bracket,
    x[["lower"]],
    x[["upper"]],
    upr_bracket
  )
}


# Is the distribution supported on (a subset of) the integers? As in
# `convolve_support()`, the prototype stored by `support()` is integer (or
# logical) for lattice families.
truncated_is_lattice <- function(dist) {
  sup <- tryCatch(support(dist), error = function(e) NULL)
  if (is.null(sup)) return(FALSE)
  proto <- field(sup, "x")[[1L]]
  is.integer(proto) || is.logical(proto)
}

# The left limit of the c.d.f. at the lower truncation bound, P(Y < lower).
#
# The support of a truncated distribution is the closed interval [lower, upper],
# so the mass at `lower` must be kept when normalising. For continuous
# distributions P(Y < lower) = P(Y <= lower), but for distributions on the
# integers it is the c.d.f. at the largest integer below `lower`.
#
# With `tail = "upper"` this is the complement, P(Y >= lower).
cdf_truncated_lower <- function(dist, lower, tail = "lower") {
  if (is.finite(lower) && truncated_is_lattice(dist)) {
    cdf(dist, ceiling(lower) - 1, tail = tail)
  } else {
    cdf(dist, lower, tail = tail)
  }
}

#' @export
support.dist_truncated <- function(x, ...){
  sup <- support(x[["dist"]])
  proto <- field(sup, "x")[[1L]]
  lim <- field(sup, "lim")[[1L]]
  closed <- field(sup, "closed")[[1L]]
  lower <- x[["lower"]]
  upper <- x[["upper"]]
  # The smallest and largest attainable values of an integer supported distribution
  if (is.integer(proto) || is.logical(proto)) {
    lower <- ceiling(lower)
    upper <- floor(upper)
  }
  # Truncation bounds inside the base support are attained, as the support of
  # the truncated distribution is the closed interval [lower, upper]
  if (is.finite(lower) && (is.na(lim[1]) || lower > lim[1])) {
    lim[1] <- lower
    closed[1] <- TRUE
  }
  if (is.finite(upper) && (is.na(lim[2]) || upper < lim[2])) {
    lim[2] <- upper
    closed[2] <- TRUE
  }
  new_support_region(list(proto), list(lim), list(closed))
}

#' @export
density.dist_truncated <- function(x, at, ...){
  in_lim <- at >= x[["lower"]] & at <= x[["upper"]]
  cdf_upr <- cdf(x[["dist"]], x[["upper"]])
  cdf_lwr <- cdf_truncated_lower(x[["dist"]], x[["lower"]])
  out <- numeric(length(at))
  out[in_lim] <- density(x[["dist"]], at = at[in_lim], ...)/(cdf_upr - cdf_lwr)
  out
}

#' @export
quantile.dist_truncated <- function(x, p, ..., tail = "lower"){
  if (tail == "lower") {
    F_lwr <- cdf_truncated_lower(x[["dist"]], x[["lower"]])
    F_upr <- cdf(x[["dist"]], x[["upper"]])
    qt <- quantile(x[["dist"]], F_lwr + p * (F_upr - F_lwr), ...)
  } else {
    # Ratio of upper tails, to keep precision in the upper tail
    S_lwr <- cdf_truncated_lower(x[["dist"]], x[["lower"]], tail = "upper")
    S_upr <- cdf(x[["dist"]], x[["upper"]], tail = "upper")
    qt <- quantile(x[["dist"]], S_upr + p * (S_lwr - S_upr), ..., tail = "upper")
  }
  lwr <- x[["lower"]]
  # The smallest attainable value of an integer supported distribution
  if (is.finite(lwr) && truncated_is_lattice(x[["dist"]])) lwr <- ceiling(lwr)
  pmin(pmax(lwr, qt), x[["upper"]])
}

#' @export
cdf.dist_truncated <- function(x, q, ..., tail = "lower"){
  out <- numeric(length(q))
  q_lwr <- q < x[["lower"]]
  q_upr <- q > x[["upper"]]
  q_mid <- !(q_lwr|q_upr)
  if (tail == "lower") {
    cdf_upr <- cdf(x[["dist"]], x[["upper"]])
    cdf_lwr <- cdf_truncated_lower(x[["dist"]], x[["lower"]])
    out[q_upr] <- 1
    out[q_mid] <- (cdf(x[["dist"]], q = q[q_mid], ...) - cdf_lwr)/(cdf_upr - cdf_lwr)
  } else {
    # Ratio of upper tails, to keep precision in the upper tail
    S_upr <- cdf(x[["dist"]], x[["upper"]], tail = "upper")
    S_lwr <- cdf_truncated_lower(x[["dist"]], x[["lower"]], tail = "upper")
    out[q_lwr] <- 1
    out[q_mid] <- (cdf(x[["dist"]], q = q[q_mid], ..., tail = "upper") - S_upr)/(S_lwr - S_upr)
  }
  out
}

#' @export
mean.dist_truncated <- function(x, ...) {
  if(inherits(x$dist, "dist_sample")) {
    y <- x$dist[[1]]
    mean(y[y >= x$lower & y <= x$upper])
  } else if(inherits(x$dist, "dist_normal")) {
    mu <- x$dist$mu
    s <- x$dist$sigma
    a <- (x$lower - mu) / s
    b <- (x$upper - mu) / s
    mu + (stats::dnorm(a) - stats::dnorm(b))/(stats::pnorm(b) - stats::pnorm(a))*s
  } else {
    NextMethod()
  }
}
