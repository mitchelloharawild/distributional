#' Inflate a value of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Inflated distributions add extra probability mass at a specific value,
#' most commonly zero (zero-inflation). These distributions are useful for
#' modeling data with excess observations at a particular value compared to
#' what the base distribution would predict. Common applications include
#' zero-inflated Poisson or negative binomial models for count data with
#' many zeros.
#'
#' @param dist The distribution(s) to inflate.
#' @param prob The added probability of observing `x`.
#' @param x The value to inflate. The default of `x = 0` is for zero-inflation.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_inflated")`
#'
#'   In the following, let \eqn{Y} be an inflated random variable based on
#'   a base distribution \eqn{X}, with inflation value `x` = \eqn{c} and
#'   inflation probability `prob` = \eqn{p}.
#'
#'   **Support**: Same as the base distribution, but with additional
#'   probability mass at \eqn{c}
#'
#'   **Mean**: (when `x` is numeric)
#'
#'   \deqn{
#'     E(Y) = p \cdot c + (1-p) \cdot E(X)
#'   }{
#'     E(Y) = p * c + (1-p) * E(X)
#'   }
#'
#'   **Variance**: (when `x = 0`)
#'
#'   \deqn{
#'     \text{Var}(Y) = (1-p) \cdot \text{Var}(X) + p(1-p) \cdot [E(X)]^2
#'   }{
#'     Var(Y) = (1-p) * Var(X) + p(1-p) * [E(X)]^2
#'   }
#'
#'   For non-zero inflation values, the variance is not computed in closed form.
#'
#'   **Probability mass/density function (p.m.f/p.d.f)**:
#'
#'   For discrete distributions:
#'   \deqn{
#'     f_Y(y) = \begin{cases}
#'       p + (1-p) \cdot f_X(c) & \text{if } y = c \\
#'       (1-p) \cdot f_X(y) & \text{if } y \neq c
#'     \end{cases}
#'   }{
#'     f_Y(y) = p + (1-p) * f_X(c) if y = c, (1-p) * f_X(y) if y != c
#'   }
#'
#'   For continuous distributions:
#'   \deqn{
#'     f_Y(y) = \begin{cases}
#'       p & \text{if } y = c \\
#'       (1-p) \cdot f_X(y) & \text{if } y \neq c
#'     \end{cases}
#'   }{
#'     f_Y(y) = p if y = c, (1-p) * f_X(y) if y != c
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     F_Y(q) = \begin{cases}
#'       (1-p) \cdot F_X(q) & \text{if } q < c \\
#'       p + (1-p) \cdot F_X(q) & \text{if } q \geq c
#'     \end{cases}
#'   }{
#'     F_Y(q) = (1-p) * F_X(q) if q < c, p + (1-p) * F_X(q) if q >= c
#'   }
#'
#'   **Quantile function**:
#'
#'   The quantile function is computed numerically by inverting the
#'   inflated CDF, accounting for the jump in probability at the
#'   inflation point.
#'
#' @examples
#' # Zero-inflated Poisson
#' dist <- dist_inflated(dist_poisson(lambda = 2), prob = 0.3, x = 0)
#'
#' dist
#' mean(dist)
#' variance(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 0)
#' density(dist, 1)
#'
#' cdf(dist, 2)
#'
#' quantile(dist, 0.5)
#'
#' @name dist_inflated
#' @export
dist_inflated <- function(dist, prob, x = 0){
  vec_is(dist, new_dist())
  if(any(prob < 0 | prob > 1)){
    abort("The inflation probability must be between 0 and 1.")
  }
  new_dist(dist = dist, x = x, p = prob,
           dimnames = variates(dist), class = "dist_inflated")
}

#' @export
format.dist_inflated <- function(x, ...){
  sprintf(
    "%s+%s",
    vapply(vec_chop(x[["x"]]), format, character(1L)),
    format(x[["dist"]])
  )
}

#' @export
density.dist_inflated <- function(x, at, ...){
  x[["p"]]*(at==x[["x"]]) + (1-x[["p"]])*dist_paired(density, x[["dist"]], at = at, ...)
}

#' @export
quantile.dist_inflated <- function(x, p, ..., tail = "lower") {
  # Probability in the requested tail of the base distribution at the
  # inflation point
  cdf_at <- dist_paired(cdf, x[["dist"]], q = x[["x"]], ..., tail = tail)
  par <- vec_recycle_common(p = p, p_inf = x[["p"]], x_inf = x[["x"]], cdf_at = cdf_at)
  dist <- vec_recycle(x[["dist"]], vec_size(par$p))

  # Range of tail probabilities covered by the point mass at the inflation point
  p_jump_lo <- (1 - par$p_inf) * par$cdf_at
  p_jump_hi <- par$p_inf + (1 - par$p_inf) * par$cdf_at

  # In order of increasing `p`, quantiles reached before (`pre`) and after
  # (`post`) the point mass
  if (tail == "lower") {
    pre <- par$p <= p_jump_lo
    post <- par$p > p_jump_hi
  } else {
    pre <- par$p < p_jump_lo
    post <- par$p >= p_jump_hi
  }
  # A fully inflated distribution is the point mass
  pre <- which(pre & par$p_inf < 1)
  post <- which(post & par$p_inf < 1)

  qt <- par$x_inf
  if (length(pre))
    qt[pre] <- dist_paired(
      quantile, vec_slice(dist, pre), p = par$p[pre] / (1 - par$p_inf[pre]), ..., tail = tail
    )
  if (length(post))
    qt[post] <- dist_paired(
      quantile, vec_slice(dist, post), p = (par$p[post] - par$p_inf[post]) / (1 - par$p_inf[post]), ..., tail = tail
    )
  qt
}

#' @export
cdf.dist_inflated <- function(x, q, ..., tail = "lower"){
  in_tail <- if (tail == "lower") q >= x[["x"]] else q < x[["x"]]
  x[["p"]]*in_tail + (1-x[["p"]])*dist_paired(cdf, x[["dist"]], q = q, ..., tail = tail)
}

#' @export
generate.dist_inflated <- function(x, times, ...){
  n <- vec_size(x)
  # Draws for each distribution are contiguous, as are the inflated points
  # and the draws from the base distribution within them
  id <- rep(seq_len(n), times)
  inf <- stats::runif(sum(times)) < x[["p"]][id]
  r_dist <- generate(x[["dist"]], times = list(tabulate(id[!inf], nbins = n)))
  r_dist <- vec_c(!!!r_dist)
  x_inf <- vec_slice(x[["x"]], id[inf])

  r <- vec_init(vec_ptype2(x_inf, r_dist), sum(times))
  r <- vec_assign(r, which(inf), x_inf)
  r <- vec_assign(r, which(!inf), r_dist)
  vec_chop(r, sizes = times)
}

#' @export
support.dist_inflated <- function(x, ...){
  sup <- support(x[["dist"]])
  proto <- field(sup, "x")
  lim <- field(sup, "lim")
  closed <- field(sup, "closed")
  x_inf <- x[["x"]]
  p <- x[["p"]]
  # The support is extended for univariate numeric inflation
  extend <- is.numeric(x_inf) &
    vapply(proto, function(z) !is.matrix(z) && (is.numeric(z) || is.logical(z)), logical(1L))
  for (i in which(!extend)) {
    # Otherwise the support is found from the quantiles
    sup_i <- support.dist_default(vec_slice(x, i))
    proto[[i]] <- field(sup_i, "x")[[1L]]
    lim[[i]] <- field(sup_i, "lim")[[1L]]
    closed[[i]] <- field(sup_i, "closed")[[1L]]
  }
  for (i in which(extend)) {
    # Integer support is kept when inflating at an integer
    if (is.integer(proto[[i]]) || is.logical(proto[[i]])) {
      proto[[i]] <- if (isTRUE(x_inf[i] == round(x_inf[i]))) integer() else numeric()
    }
    if (p[i] == 1) {
      lim[[i]] <- c(x_inf[i], x_inf[i])
      closed[[i]] <- c(TRUE, TRUE)
    } else if (p[i] > 0) {
      # The inflation point is attained, widening the base support if needed
      if (isTRUE(x_inf[i] <= lim[[i]][1])) {
        lim[[i]][1] <- x_inf[i]
        closed[[i]][1] <- TRUE
      }
      if (isTRUE(x_inf[i] >= lim[[i]][2])) {
        lim[[i]][2] <- x_inf[i]
        closed[[i]][2] <- TRUE
      }
    }
  }
  new_support_region(proto, lim, closed)
}

#' @export
mean.dist_inflated <- function(x, ...){
  # Can't compute if inflation value is not numeric
  if(!vec_is(x[["x"]], numeric())) return(rep(NA_real_, vec_size(x)))

  p <- x[["p"]]
  p*x[["x"]] + (1-p)*mean(x[["dist"]])
}

#' @export
covariance.dist_inflated <- function(x, ...){
  out <- rep(NA_real_, vec_size(x))
  # Can't compute if inflation value is not numeric
  if(!vec_is(x[["x"]], numeric())) return(out)
  # Can't (easily) compute if inflation value is not zero
  zero <- which(x[["x"]] == 0)
  if (!length(zero)) return(out)

  dist <- vec_slice(x[["dist"]], zero)
  m1 <- mean(dist)
  v <- variance(dist)
  m2 <- v + m1^2
  p <- x[["p"]][zero]
  out[zero] <- (1-p)*v + p*(1-p)*m1^2
  out
}
