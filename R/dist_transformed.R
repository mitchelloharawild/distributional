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
#'   support limits are reported as unknown (`NA`). The support of a
#'   transformed discrete distribution is on the integers only when \eqn{g}
#'   maps the integers of its support to integers (such as
#'   `-dist_poisson(3)`), and is otherwise reported as an interval of the
#'   real numbers (such as `0.1 * dist_poisson(3)`).
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
#'   For a discrete base distribution, the probability mass function is the
#'   mass at the preimage, \eqn{P(Y = y) = P(X = g^{-1}(y))}, without the
#'   Jacobian. It is 0 when \eqn{y} is not the image of a point in the
#'   support (such as odd numbers for `2 * dist_poisson(3)`).
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
#'     F_Y(y) = 1 - F_X(g^{-1}(y)) + P(X = g^{-1}(y))
#'   }{
#'     F_Y(y) = 1 - F_X(g^(-1)(y)) + P(X = g^(-1)(y))
#'   }
#'
#'   where \eqn{F_X} is the c.d.f. of the base distribution. The probability
#'   mass \eqn{P(X = g^{-1}(y))} is 0 for continuous distributions.
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
  group <- vec_group_id(vctrs::new_data_frame(list(
    tf = fn_group_id(tf), inv = fn_group_id(inv)
  )))

  if (attr(group, "n") == 1L) {
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

# The support of the distributions `dist` transformed by `tf`, from the
# support of `dist` (`base_sup`)
transformed_support <- function(dist, tf, inv, base_sup = support(dist)) {
  lim <- field(base_sup, "lim")
  closed <- field(base_sup, "closed")

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
  proto <- field(base_sup, "x")
  lattice <- support_is_lattice(base_sup)
  if (any(lattice)) {
    pts <- quantile(dist[lattice], p)
    if (!is.list(pts)) pts <- list(pts)
    proto[lattice] <- .mapply(transformed_lattice_proto, list(
      proto[lattice], pts, field(base_sup, "lim")[lattice]
    ), list(tf = tf))
  }
  new_support_region(proto, lim, closed)
}

# The prototype of the support of a transformed distribution on the integers
# (or logicals), from points `pts` of its base support (its quantiles) and its
# limits `lim`. The image is still on the integers when the transformation
# maps these points (and their neighbours) to integers, up to the rounding
# error tolerated by R's discrete p-functions, as for `2 * dist_poisson(3)` or
# `-dist_poisson(3)`. Otherwise (as for `0.1 * dist_poisson(3)`) the image is
# not on the integers, and is described as a subset of the real numbers.
# Logical support is kept only when the transformation maps 0 and 1 to
# themselves.
transformed_lattice_proto <- function(proto, pts, lim, tf) {
  k <- c(pts, lim)
  k <- unique(k[is.finite(k)])
  k <- unique(c(k, k + 1))
  if (is.logical(proto)) k <- c(0, 1)
  v <- suppressWarnings(tf(k))
  v <- v[!is.na(v)]
  if (!length(v)) return(proto)
  finite <- is.finite(v)
  int <- all(abs(v[finite] - round(v[finite])) < 1e-7 * pmax(1, abs(v[finite])))
  if (!int) return(double())
  if (is.logical(proto) && !identical(sort(as.double(v)), c(0, 1))) return(integer())
  proto
}

#' @export
support.dist_transformed <- function(x, ...) {
  transformed_by_group(x, function(dist, tf, inv, idx) {
    transformed_support(dist, tf, inv)
  })
}

# The preimage `inv(at)` of `at` in the base distributions, for finding their
# point masses. The inverse is often inexact in floating point (`0.3 / 0.1` is
# `2.9999999999999996`), so it is replaced by the nearest integer when the
# transformation maps that integer back to `at`, up to rounding error. This
# doesn't rely on the inverse being exact, and leaves a value that isn't the
# image of an integer (such as 1.5 for `2 * dist_poisson(3)` at 3) as it is.
# The tolerance is a few thousand ulps (relative): wide enough for rounding
# error in the transformation, but narrow enough not to mistake neighbouring
# points of a shifted lattice (such as `dist_poisson(3) + 1e9`) for `at`.
transformed_preimage <- function(tf, inv, at) {
  y <- suppressWarnings(inv(at))
  k <- round(y)
  fwd <- suppressWarnings(tf(k))
  snap <- which(
    is.finite(fwd) & is.finite(at) & abs(fwd - at) <= 1e-12 * pmax(abs(at), abs(fwd))
  )
  y[snap] <- k[snap]
  y
}

# The mass of the base distributions `dist` at the preimage of `at`. A value
# outside the image of the support has no preimage with mass, so its mass is 0.
# Without an inverse the mass is unknown (`NA`) for discrete distributions, and
# is taken to be 0 otherwise.
transformed_mass <- function(dist, tf, inv, at, ...) {
  y <- tryCatch(transformed_preimage(tf, inv, at), error = function(e) NULL)
  if (is.null(y)) return(ifelse(is_lattice(dist), NA_real_, 0))
  dist_paired(point_mass, dist, at = y, ...)
}

#' @export
point_mass.dist_transformed <- function(x, at, ...){
  n <- vec_size(x)
  transformed_by_group(x, function(dist, tf, inv, idx) {
    transformed_mass(dist, tf, inv, transformed_arg(at, idx, n), ...)
  })
}

# The density of a continuous base distribution is transformed with the
# Jacobian of the inverse. A discrete base distribution has a probability mass
# rather than a density, which is the mass at the preimage of `at` (and 0 when
# `at` isn't in the image of the support), without a Jacobian.
#' @export
density.dist_transformed <- function(x, at, ...){
  n <- vec_size(x)
  transformed_by_group(x, function(dist, tf, inv, idx) {
    at <- transformed_arg(at, idx, n)
    # The support of the base distribution can be slow to compute (such as
    # for `dist_wrap()`), so it is computed once
    base_sup <- support(dist)
    discrete <- support_is_lattice(base_sup)
    # Usually every distribution in the group is discrete or none is, and
    # they are then computed without subsetting
    sub <- function(v, i) if (all(i)) v else v[i]
    d <- numeric(length(at))
    if (any(discrete)) {
      d[discrete] <- transformed_mass(sub(dist, discrete), tf, inv, sub(at, discrete), ...)
    }
    if (!all(discrete)) {
      cont <- !discrete
      at_c <- sub(at, cont)
      inv_f <- function(v) suppressWarnings(inv(v))
      jacobian <- vapply(at_c, numDeriv::jacobian, numeric(1L), func = inv_f)
      d[cont] <- dist_paired(density, sub(dist, cont), at = inv_f(at_c), ...) * abs(jacobian)
    }
    sup <- transformed_support(dist, tf, inv, base_sup)
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
#
# Swapping the tails is only exact for continuous distributions, so the
# results of a decreasing transformation are corrected by
# `decreasing(dist, a, out, base_tail)`, where `out` is the result of `.f` in
# the swapped tail `base_tail` (see `transformed_cdf_mass()` and
# `transformed_quantile_atom()`).
transformed_inner <- function(.f, dist, tf, a, name, ..., tail = "lower", decreasing = NULL) {
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
    if (d == -1L && !is.null(decreasing)) {
      out[i] <- decreasing(dist[i], a[i], out[i], swap)
    }
  }
  out
}

# For a decreasing transformation `g`, the swapped tail gives P(g(X) <= q) as
# P(X > g^-1(q)), but it is P(X >= g^-1(q)). They differ by the mass of the
# base distribution at `a = g^-1(q)`, which is added to the lower tail (when
# the base tail is "upper") and subtracted from the upper tail.
transformed_cdf_mass <- function(dist, a, p, base_tail) {
  m <- dist_paired(point_mass, dist, at = a)
  pmin(pmax(if (base_tail == "upper") p + m else p - m, 0), 1)
}

# For a decreasing transformation `g`, the quantile at `p` is g(k) for the
# largest `k` with P(X >= k) >= p, while the base quantile in the swapped tail
# is the smallest `k` with P(X > k) <= p. They differ when P(X > k) is `p`
# (up to rounding error): `p` is then the cdf at an atom, and the quantile is
# at the next atom, k + 1 for distributions on the integers. The upper tail is
# the same, with P(X <= k) in place of P(X > k).
transformed_quantile_atom <- function(dist, p, k, base_tail) {
  lattice <- is_lattice(dist)
  if (!any(lattice)) return(k)
  s <- dist_paired(cdf, dist, q = k, tail = base_tail)
  next_atom <- which(lattice & is.finite(k) & abs(s - p) <= 1e-12 * p)
  k[next_atom] <- k[next_atom] + 1
  k
}

#' @export
cdf.dist_transformed <- function(x, q, ..., tail = "lower"){
  n <- vec_size(x)
  transformed_by_group(x, function(dist, tf, inv, idx) {
    q <- transformed_arg(q, idx, n)
    p <- transformed_inner(
      cdf, dist, tf, transformed_preimage(tf, inv, q), "the cdf", ...,
      tail = tail, decreasing = transformed_cdf_mass
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
    tf(transformed_inner(
      quantile, dist, tf, p, "quantiles", ..., tail = tail,
      decreasing = transformed_quantile_atom
    ))
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
  tf <- x[["transform"]]
  inv <- x[["inverse"]]
  generic <- .Generic
  dots <- dots_list(...)
  inverse_fun <- get_unary_inverse(.Generic)

  # Compose the transformation with each distinct transformation (and inverse)
  key_tf <- fn_group_id(tf)
  key_inv <- fn_group_id(inv)
  trans <- fns_by_group(function(i) {
    new_function(exprs(x = ), body = expr((!!sym(generic))((!!tf[[i]])(x), !!!dots)))
  }, key_tf, key_inv)
  inverse <- fns_by_group(function(i) {
    new_function(exprs(x = ), body = expr((!!inv[[i]])((!!inverse_fun)(x, !!!dots))))
  }, key_tf, key_inv)

  dist_record(dist_transformed(x[["dist"]], trans, inverse))
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

  if(all(is_dist)) {
    # Fall back to default distribution Ops
    return(NextMethod())
  }

  # The operands are paired by position (a size 1 operand is recycled)
  n <- max(vec_size(e1), vec_size(e2))
  e1 <- vec_recycle(e1, n)
  e2 <- vec_recycle(e2, n)

  generic <- .Generic
  if (is_dist[1]) {
    x <- e1
    constant <- unname(e2)
  } else {
    x <- e2
    constant <- unname(e1)
  }
  tf <- x[["transform"]]
  inv <- x[["inverse"]]

  # Compose the operation with each distinct transformation (and inverse) and
  # constant
  key_tf <- fn_group_id(tf)
  key_inv <- fn_group_id(inv)
  trans <- fns_by_group(function(i) {
    if (is_dist[1]) {
      new_function(exprs(x = ), body = expr((!!sym(generic))((!!tf[[i]])(x), !!constant[[i]])))
    } else {
      new_function(exprs(x = ), body = expr((!!sym(generic))(!!constant[[i]], (!!tf[[i]])(x))))
    }
  }, key_tf, key_inv, constant)
  inverse <- fns_by_group(function(i) {
    inverse_fun <- if (is_dist[1]) {
      get_binary_inverse_1(generic, constant[[i]])
    } else {
      get_binary_inverse_2(generic, constant[[i]])
    }
    new_function(exprs(x = ), body = expr((!!inv[[i]])((!!inverse_fun)(x))))
  }, key_tf, key_inv, constant)

  dist_record(dist_transformed(x[["dist"]], trans, inverse))
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
