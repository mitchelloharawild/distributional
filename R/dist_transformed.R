#' Modify a distribution with a transformation
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' A transformed distribution applies a monotonic transformation to an existing
#' distribution. This is useful for creating derived distributions such as 
#' log-normal (exponential transformation of normal), or other custom 
#' transformations of base distributions.
#'
#' The [`density()`], [`mean()`], and [`variance()`] methods are approximate as
#' they are based on numerical derivatives.
#'
#' @param dist A univariate distribution vector.
#' @param transform A function used to transform the distribution. This
#' transformation should be monotonic over the region of the base distribution's
#' mass (see Details).
#' @param inverse The inverse of the `transform` function.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_transformed")`
#'
#'   Let \eqn{Y = g(X)} where \eqn{X} is the base distribution with
#'   transformation function `transform` = \eqn{g} and `inverse` = \eqn{g^{-1}}.
#'   The transformation \eqn{g} must be monotonic so that the mapping between
#'   \eqn{X} and \eqn{Y} is invertible.
#'
#'   The direction of the transformation (increasing or decreasing) is detected
#'   by probing \eqn{g} at quantiles spanning the region of mass of \eqn{X},
#'   rather than at the (frequently infinite) support limits. A transformation
#'   that is monotonic where \eqn{X} has mass but not in its far tails (for
#'   example \eqn{g(x) = x^2} applied to a Normal whose mass is positive) is
#'   therefore treated as monotonic. When \eqn{g} is *not* monotonic over the
#'   region of mass, `cdf()` and `quantile()` are not well-defined from the
#'   single stored `inverse` and return `NA` with a warning.
#'
#'   **Support**: \eqn{g(S_X)} where \eqn{S_X} is the support of \eqn{X}. When
#'   \eqn{g} cannot be verified to be monotonic over the whole support, the
#'   support limits are reported as unknown (`NA`).
#'
#'   **Mean**: Approximated numerically using a second-order Taylor expansion:
#'
#'   \deqn{
#'     E(Y) \approx g(\mu_X) + \frac{1}{2}g''(\mu_X)\sigma_X^2
#'   }{
#'     E(Y) ~= g(mu_X) + (1/2) g''(mu_X) sigma_X^2
#'   }
#'
#'   where \eqn{\mu_X} and \eqn{\sigma_X^2} are the mean and variance of the 
#'   base distribution \eqn{X}, and \eqn{g''} is the second derivative of the 
#'   transformation. The derivative is computed numerically using 
#'   [`numDeriv::hessian()`].
#'
#'   **Variance**: Approximated numerically using the delta method:
#'
#'   \deqn{
#'     \mathrm{Var}(Y) \approx [g'(\mu_X)]^2\sigma_X^2 + \frac{1}{2}[g''(\mu_X)\sigma_X^2]^2
#'   }{
#'     Var(Y) ~= [g'(mu_X)]^2 sigma_X^2 + (1/2) [g''(mu_X) sigma_X^2]^2
#'   }
#'
#'   where \eqn{g'} is the first derivative (Jacobian) computed numerically 
#'   using [`numDeriv::jacobian()`].
#'
#'   **Probability density function (p.d.f)**: Using the change of variables 
#'   formula:
#'
#'   \deqn{
#'     f_Y(y) = f_X(g^{-1}(y)) \left|\frac{d}{dy}g^{-1}(y)\right|
#'   }{
#'     f_Y(y) = f_X(g^(-1)(y)) |d/dy g^(-1)(y)|
#'   }
#'
#'   where \eqn{f_X} is the p.d.f. of the base distribution and the Jacobian
#'   \eqn{|d/dy \, g^{-1}(y)|} is computed numerically using 
#'   [`numDeriv::jacobian()`].
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   For monotonically increasing \eqn{g}:
#'   \deqn{
#'     F_Y(y) = F_X(g^{-1}(y))
#'   }{
#'     F_Y(y) = F_X(g^(-1)(y))
#'   }
#'
#'   For monotonically decreasing \eqn{g}:
#'   \deqn{
#'     F_Y(y) = 1 - F_X(g^{-1}(y))
#'   }{
#'     F_Y(y) = 1 - F_X(g^(-1)(y))
#'   }
#'
#'   where \eqn{F_X} is the c.d.f. of the base distribution.
#'
#'   **Quantile function**: The inverse of the c.d.f.
#'
#'   For monotonically increasing \eqn{g}:
#'   \deqn{
#'     Q_Y(p) = g(Q_X(p))
#'   }{
#'     Q_Y(p) = g(Q_X(p))
#'   }
#'
#'   For monotonically decreasing \eqn{g}:
#'   \deqn{
#'     Q_Y(p) = g(Q_X(1-p))
#'   }{
#'     Q_Y(p) = g(Q_X(1-p))
#'   }
#'
#'   where \eqn{Q_X} is the quantile function of the base distribution.
#'
#' @seealso [numDeriv::jacobian()], [numDeriv::hessian()]
#'
#' @examples
#' # Create a log normal distribution
#' dist <- dist_transformed(dist_normal(0, 0.5), exp, log)
#' density(dist, 1) # dlnorm(1, 0, 0.5)
#' cdf(dist, 4) # plnorm(4, 0, 0.5)
#' quantile(dist, 0.1) # qlnorm(0.1, 0, 0.5)
#' generate(dist, 10) # rlnorm(10, 0, 0.5)
#'
#' @export
dist_transformed <- function(dist, transform, inverse){
  vec_is(dist, new_dist())
  if(is.function(transform)) transform <- list(transform)
  if(is.function(inverse)) inverse <- list(inverse)
  new_dist(dist = dist,
           transform = transform, inverse = inverse,
           dimnames = variates(dist), class = "dist_transformed")
}

#' @export
format.dist_transformed <- function(x, ...){
  sprintf(
    "t(%s)",
    format(x[["dist"]])
  )
}

# The transformations are functions, so distributions can only be handled
# together when they share the same `transform` and `inverse`. This splits the
# positions of `x` into such groups (usually a single group, as in
# `exp(dist_normal(1:3))`), calls `.f(dist, transform, inverse, idx)` once for
# each group with its inner distributions, `idx` being their positions in `x`,
# and returns the results (each of size `length(idx)`) in the order of `x`.
transformed_by_group <- function(x, .f) {
  tf <- x[["transform"]]
  inv <- x[["inverse"]]
  dist <- x[["dist"]]
  n <- vec_size(x)

  # Group positions by identical functions
  group <- integer(n)
  reps <- list()
  for (i in seq_len(n)) {
    k <- 0L
    for (j in seq_along(reps)) {
      if (identical(tf[[i]], tf[[reps[[j]]]]) && identical(inv[[i]], inv[[reps[[j]]]])) {
        k <- j
        break
      }
    }
    if (k == 0L) {
      reps[[length(reps) + 1L]] <- i
      k <- length(reps)
    }
    group[i] <- k
  }

  if (length(reps) == 1L) {
    return(.f(dist, tf[[1L]], inv[[1L]], seq_len(n)))
  }
  idx <- unname(split(seq_len(n), group))
  res <- lapply(idx, function(i) {
    .f(dist[i], tf[[i[1L]]], inv[[i[1L]]], i)
  })
  vec_slice(vec_c(!!!res), order(unlist(idx)))
}

# Positions `idx` of an argument paired with the distributions (size `n`), or
# the argument itself when it is recycled (size 1)
transformed_arg <- function(a, idx, n) {
  if (vec_size(a) == n) vec_slice(a, idx) else vec_recycle(a, length(idx))
}

# Direction of the transformation `f` for each of the distributions in `dist`
# (see `monotonic_dir()`)
transformed_dir <- function(f, dist, p = seq(0, 1, length.out = 23L)[2:22]) {
  n <- vec_size(dist)
  # Shortcut for identity function (used widely in ggdist)
  if(!is.primitive(f) && identical(body(f), as.name(names(formals(f))))) {
    return(rep(1L, n))
  }
  q <- quantile(dist, p)
  if (!is.list(q)) q <- list(q)
  vapply(q, function(qi) monotonic_dir_q(f, qi), integer(1L))
}

# The support of the distributions `dist` transformed by `tf`
transformed_support <- function(dist, tf, inv) {
  support <- support(dist)
  lim <- field(support, "lim")
  closed <- field(support, "closed")

  # Mapping the support limits through the transform and sorting them only
  # gives the correct image when the transform is monotonic over the *whole*
  # support. Probe monotonicity across the support (reaching toward the limits
  # via extreme base quantiles); if it cannot be verified, or a limit maps to
  # NA, return unknown (NA) limits rather than a wrong image. The clamping in
  # `cdf()` and `density()` is guarded on `!is.na(lim)` and degrades to no
  # clamping. Computing the exact image for non-monotonic transforms (via
  # interior critical points) is left as a follow-up (see #9).
  tail <- 10^(-8:-2)
  p <- sort(unique(c(tail, seq(0, 1, length.out = 23L)[2:22], 1 - tail)))
  dir <- transformed_dir(tf, dist, p)
  for (i in vec_seq_along(dist)) {
    lim_i <- suppressWarnings(tf(lim[[i]]))
    if (all(!is.na(lim_i)) && dir[i] != 0L) {
      # A decreasing transform swaps the limits, and with them their closedness.
      if (is.unsorted(lim_i)) {
        lim_i <- rev(lim_i)
        closed[[i]] <- rev(closed[[i]])
      }
    } else {
      lim_i <- c(NA_real_, NA_real_)
    }
    lim[[i]] <- lim_i
  }
  new_support_region(field(support, "x"), lim, closed)
}

#' @export
support.dist_transformed <- function(x, ...) {
  transformed_by_group(x, function(dist, tf, inv, idx) {
    transformed_support(dist, tf, inv)
  })
}

#' @export
density.dist_transformed <- function(x, at, ...){
  n <- vec_size(x)
  transformed_by_group(x, function(dist, tf, inv, idx) {
    at <- transformed_arg(at, idx, n)
    inv_f <- function(v) suppressWarnings(inv(v))
    jacobian <- vapply(at, numDeriv::jacobian, numeric(1L), func = inv_f)
    d <- dist_paired(density, dist, at = inv_f(at), ...) * abs(jacobian)
    sup <- transformed_support(dist, tf, inv)
    lim <- vec_c(!!!field(sup, "lim"))
    lo <- lim[c(TRUE, FALSE)]
    hi <- lim[c(FALSE, TRUE)]
    closed <- vec_c(!!!field(sup, "closed"))
    below <- ifelse(closed[c(TRUE, FALSE)], at < lo, at <= lo)
    above <- ifelse(closed[c(FALSE, TRUE)], at > hi, at >= hi)
    valid <- !is.na(lo) & !is.na(hi)
    d[which(valid & (below | above))] <- 0
    d
  })
}

# Call the inner generic `.f` (`cdf` or `quantile`) with the argument `a`
# (paired with the distributions), accounting for the direction of the
# transformation. A decreasing transformation swaps the tails, and a
# non-monotonic transformation gives `NA`.
transformed_inner <- function(.f, dist, tf, a, name, ..., tail = "lower") {
  dir <- transformed_dir(tf, dist)
  if (any(dir == 0L)) {
    warning(sprintf(
      "Cannot compute %s of a non-monotonic transformation over the distribution's region of mass, returning `NA`.",
      name
    ))
  }
  swap <- c("lower" = "upper", "upper" = "lower")[[tail]]
  out <- rep(NA_real_, vec_size(dist))
  for (d in c(1L, -1L)) {
    i <- which(dir == d)
    if (!length(i)) next
    out[i] <- dist_paired(.f, dist[i], a[i], ..., tail = if (d == 1L) tail else swap)
  }
  out
}

#' @export
cdf.dist_transformed <- function(x, q, ..., tail = "lower"){
  n <- vec_size(x)
  transformed_by_group(x, function(dist, tf, inv, idx) {
    q <- transformed_arg(q, idx, n)
    p <- transformed_inner(
      cdf, dist, tf, suppressWarnings(inv(q)), "the cdf", ..., tail = tail
    )

    # TODO: Rework for support of closed limits and prevent computation
    sup <- transformed_support(dist, tf, inv)
    lim <- vec_c(!!!field(sup, "lim"))
    lo <- lim[c(TRUE, FALSE)]
    hi <- lim[c(FALSE, TRUE)]
    closed <- vec_c(!!!field(sup, "closed"))
    valid <- !is.na(lo) & !is.na(hi)
    p[which(valid & q <= lo & !closed[c(TRUE, FALSE)])] <- if (tail == "lower") 0 else 1
    p[which(valid & q >= hi & !closed[c(FALSE, TRUE)])] <- if (tail == "lower") 1 else 0
    p
  })
}

#' @export
quantile.dist_transformed <- function(x, p, ..., tail = "lower"){
  n <- vec_size(x)
  transformed_by_group(x, function(dist, tf, inv, idx) {
    p <- transformed_arg(p, idx, n)
    # Quantiles of the base distribution (`NA` for non-monotonic transformations)
    tf(transformed_inner(quantile, dist, tf, p, "quantiles", ..., tail = tail))
  })
}

#' @export
generate.dist_transformed <- function(x, times, ...){
  n <- vec_size(x)
  times <- vec_recycle(times, n)
  res <- transformed_by_group(x, function(dist, tf, inv, idx) {
    r <- generate(dist, times = list(times[idx]), ...)
    # Transform all draws at once, keeping those of each distribution together
    vec_chop(tf(vec_c(!!!r)), sizes = times[idx])
  })
  res
}

#' @export
mean.dist_transformed <- function(x, ...){
  transformed_by_group(x, function(dist, tf, inv, idx) {
    mu <- mean(dist)
    sigma2 <- variance(dist)
    vapply(seq_along(mu), function(i) {
      if (is.na(sigma2[i])) {
        # warning("Could not compute the transformed distribution's mean as the base distribution's variance is unknown. The transformed distribution's median has been returned instead.")
        return(tf(mu[i]))
      }
      drop(
        tf(mu[i]) + numDeriv::hessian(tf, mu[i], method.args=list(d = 0.01))/2*sigma2[i]
      )
    }, numeric(1L))
  })
}

#' @export
covariance.dist_transformed <- function(x, ...){
  transformed_by_group(x, function(dist, tf, inv, idx) {
    mu <- mean(dist)
    sigma2 <- variance(dist)
    vapply(seq_along(mu), function(i) {
      if (is.na(sigma2[i])) return(NA_real_)
      drop(
        numDeriv::jacobian(tf, mu[i])^2*sigma2[i] + (numDeriv::hessian(tf, mu[i], method.args=list(d = 0.01))*sigma2[i])^2/2
      )
    }, numeric(1L))
  })
}

#' @export
variance.dist_transformed <- function(x, ...){
  covariance.dist_transformed(x, ...)
}

#' @method Math dist_transformed
#' @export
Math.dist_transformed <- function(x, ...) {
  trans <- new_function(exprs(x = ), body = expr((!!sym(.Generic))((!!x$transform[[1L]])(x), !!!dots_list(...))))

  inverse_fun <- get_unary_inverse(.Generic)
  inverse <- new_function(exprs(x = ), body = expr((!!x$inverse[[1L]])((!!inverse_fun)(x, !!!dots_list(...)))))

  dist_first_record(dist_transformed(x[["dist"]], trans, inverse))
}

#' @method Ops dist_transformed
#' @export
Ops.dist_transformed <- function(e1, e2) {
  if(.Generic %in% c("-", "+") && missing(e2)){
    e2 <- e1
    e1 <- if(.Generic == "+") 1 else -1
    .Generic <- "*"
  }
  is_dist <- c(inherits(e1, "dist_default"), inherits(e2, "dist_default"))

  trans <- if(all(is_dist)) {
    # Fall back to default distribution Ops
    return(NextMethod())
  } else if(is_dist[1]){
    new_function(exprs(x = ), body = expr((!!sym(.Generic))((!!e1$transform[[1L]])(x), !!e2)))
  } else {
    new_function(exprs(x = ), body = expr((!!sym(.Generic))(!!e1, (!!e2$transform[[1L]])(x))))
  }

  inverse <- if(all(is_dist)) {
    invert_fail
  } else if(is_dist[1]){
    inverse_fun <- get_binary_inverse_1(.Generic, e2)
    new_function(exprs(x = ), body = expr((!!e1$inverse[[1L]])((!!inverse_fun)(x))))
  } else {
    inverse_fun <- get_binary_inverse_2(.Generic, e1)
    new_function(exprs(x = ), body = expr((!!e2$inverse[[1L]])((!!inverse_fun)(x))))
  }

  dist_first_record(dist_transformed(list(e1,e2)[[which(is_dist)[1]]][["dist"]], trans, inverse))
}

# Direction of a transformation `f`, judged by probing it at quantiles of the
# base distribution `dist`:
#    1L = increasing, -1L = decreasing, 0L = non-monotonic over the probed region.
# Probing base quantiles judges monotonicity over the region of *mass*. The
# default `p` covers the interior of the distribution; pass more extreme
# probabilities to probe further toward the support limits (used by `support()`).
monotonic_dir <- function(f, dist, p = seq(0, 1, length.out = 23L)[2:22]) {
  # Shortcut for identity function (used widely in ggdist)
  if(!is.primitive(f) && identical(body(f), as.name(names(formals(f))))) {
    return(1L)
  }
  monotonic_dir_q(f, quantile(dist, p))
}

# The direction of `f` over the (ordered) base quantiles `q`
monotonic_dir_q <- function(f, q) {
  d <- diff(suppressWarnings(f(q)))
  d <- d[is.finite(d) & d != 0]
  if (!length(d)) return(1L)
  if (all(d > 0)) return(1L)
  if (all(d < 0)) return(-1L)
  0L
}
