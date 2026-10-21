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
           dimnames = variates(dist), class = "dist_truncated")
}

#' @export
format.dist_truncated <- function(x, ...){
  lwr_bracket <- ifelse(is.finite(x[["lower"]]), "[", "(")
  upr_bracket <- ifelse(is.finite(x[["upper"]]), "]", ")")
  sprintf(
    "%s%s%s,%s%s",
    format(x[["dist"]]),
    lwr_bracket,
    x[["lower"]],
    x[["upper"]],
    upr_bracket
  )
}


# Is each distribution supported on (a subset of) the integers? As in
# `convolve_support()`, the prototype stored by `support()` is integer (or
# logical) for lattice families.
truncated_is_lattice <- function(dist) {
  sup <- tryCatch(support(dist), error = function(e) NULL)
  if (is.null(sup)) return(rep(FALSE, vec_size(dist)))
  vapply(field(sup, "x"), function(proto) is.integer(proto) || is.logical(proto), logical(1L))
}

# The left limit of the c.d.f. at the lower truncation bound, P(Y < lower).
#
# The support of a truncated distribution is the closed interval [lower, upper],
# so the mass at `lower` must be kept when normalising. For continuous
# distributions P(Y < lower) = P(Y <= lower), but for distributions on the
# integers (`lattice`) it is the c.d.f. at the largest integer below `lower`.
#
# With `tail = "upper"` this is the complement, P(Y >= lower).
cdf_truncated_lower <- function(x, lattice, ..., tail = "lower") {
  lower <- x[["lower"]]
  lower[lattice] <- ceiling(lower[lattice]) - 1
  dist_paired(cdf, x[["dist"]], q = lower, ..., tail = tail)
}

#' @export
support.dist_truncated <- function(x, ...){
  sup <- support(x[["dist"]])
  proto <- field(sup, "x")
  lim <- field(sup, "lim")
  closed <- field(sup, "closed")
  lower <- x[["lower"]]
  upper <- x[["upper"]]
  for (i in vec_seq_along(x)) {
    # The smallest and largest attainable values of an integer supported distribution
    if (is.integer(proto[[i]]) || is.logical(proto[[i]])) {
      lower[i] <- ceiling(lower[i])
      upper[i] <- floor(upper[i])
    }
    # Truncation bounds inside the base support are attained, as the support of
    # the truncated distribution is the closed interval [lower, upper]
    if (is.finite(lower[i]) && (is.na(lim[[i]][1]) || lower[i] > lim[[i]][1])) {
      lim[[i]][1] <- lower[i]
      closed[[i]][1] <- TRUE
    }
    if (is.finite(upper[i]) && (is.na(lim[[i]][2]) || upper[i] < lim[[i]][2])) {
      lim[[i]][2] <- upper[i]
      closed[[i]][2] <- TRUE
    }
  }
  new_support_region(proto, lim, closed)
}

#' @export
density.dist_truncated <- function(x, at, ...){
  in_lim <- at >= x[["lower"]] & at <= x[["upper"]]
  cdf_upr <- dist_paired(cdf, x[["dist"]], q = x[["upper"]])
  cdf_lwr <- cdf_truncated_lower(x, truncated_is_lattice(x[["dist"]]))
  ifelse(in_lim, dist_paired(density, x[["dist"]], at = at, ...)/(cdf_upr - cdf_lwr), 0)
}

#' @export
quantile.dist_truncated <- function(x, p, ..., tail = "lower"){
  lattice <- truncated_is_lattice(x[["dist"]])
  if (tail == "lower") {
    F_lwr <- cdf_truncated_lower(x, lattice)
    F_upr <- dist_paired(cdf, x[["dist"]], q = x[["upper"]])
    qt <- dist_paired(quantile, x[["dist"]], p = F_lwr + p * (F_upr - F_lwr), ...)
  } else {
    # Ratio of upper tails, to keep precision in the upper tail
    S_lwr <- cdf_truncated_lower(x, lattice, tail = "upper")
    S_upr <- dist_paired(cdf, x[["dist"]], q = x[["upper"]], tail = "upper")
    qt <- dist_paired(quantile, x[["dist"]], p = S_upr + p * (S_lwr - S_upr), ..., tail = "upper")
  }
  # The smallest attainable value of an integer supported distribution
  lwr <- x[["lower"]]
  lwr[lattice] <- ceiling(lwr[lattice])
  pmin(pmax(lwr, qt), x[["upper"]])
}

#' @export
cdf.dist_truncated <- function(x, q, ..., tail = "lower"){
  q_lwr <- q < x[["lower"]]
  q_upr <- q > x[["upper"]]
  lattice <- truncated_is_lattice(x[["dist"]])
  if (tail == "lower") {
    cdf_upr <- dist_paired(cdf, x[["dist"]], q = x[["upper"]])
    cdf_lwr <- cdf_truncated_lower(x, lattice)
    cdf_q <- dist_paired(cdf, x[["dist"]], q = q, ...)
    ifelse(q_lwr, 0, ifelse(q_upr, 1, (cdf_q - cdf_lwr)/(cdf_upr - cdf_lwr)))
  } else {
    # Ratio of upper tails, to keep precision in the upper tail
    S_upr <- dist_paired(cdf, x[["dist"]], q = x[["upper"]], tail = "upper")
    S_lwr <- cdf_truncated_lower(x, lattice, tail = "upper")
    S_q <- dist_paired(cdf, x[["dist"]], q = q, ..., tail = "upper")
    ifelse(q_lwr, 1, ifelse(q_upr, 0, (S_q - S_upr)/(S_lwr - S_upr)))
  }
}

#' @export
generate.dist_truncated <- function(x, times, ...){
  # Inverse transform sampling, with each distribution repeated `times` times
  x_rep <- vec_slice(x, rep(vec_seq_along(x), times))
  vec_chop(quantile(x_rep, stats::runif(sum(times)), ...), sizes = times)
}

#' @export
mean.dist_truncated <- function(x, ...) {
  dist <- x[["dist"]]
  lower <- x[["lower"]]
  upper <- x[["upper"]]
  fam <- family(dist)
  is_sample <- fam %in% "sample"
  is_normal <- fam %in% "normal"
  is_other <- !(is_sample | is_normal)

  out <- numeric(vec_size(x))
  if (any(is_sample)) {
    out[is_sample] <- unlist(.mapply(
      function(y, lwr, upr) mean(y[y >= lwr & y <= upr]),
      list(parameters(vec_slice(dist, is_sample))$x, lower[is_sample], upper[is_sample]),
      NULL
    ))
  }
  if (any(is_normal)) {
    par <- parameters(vec_slice(dist, is_normal))
    a <- (lower[is_normal] - par$mu) / par$sigma
    b <- (upper[is_normal] - par$mu) / par$sigma
    out[is_normal] <- par$mu + (stats::dnorm(a) - stats::dnorm(b))/(stats::pnorm(b) - stats::pnorm(a))*par$sigma
  }
  if (any(is_other)) {
    out[is_other] <- mean.dist_default(vec_slice(x, is_other), ...)
  }
  out
}
