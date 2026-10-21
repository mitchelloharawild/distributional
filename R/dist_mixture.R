#' Create a mixture of distributions
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' A mixture distribution combines multiple component distributions with
#' specified weights. Two types of mixture are supported:
#'
#' - **Probability mixture** (`type = "probability"`, the default): the CDF is
#'   a weighted average of the component CDFs. Also known as *linear pooling*.
#'
#' - **Quantile mixture** (`type = "quantile"`): the quantile function is a
#'   weighted average of the component quantile functions. Also known as
#'   *Vincentization*.
#'
#' @param ... Distributions to be used in the mixture. Can be any
#'   distributional objects.
#' @param weights A numeric vector of non-negative weights that sum to 1.
#'   The length must match the number of distributions passed to `...`.
#'   Each weight \eqn{w_i} represents the probability that a random draw
#'   comes from the \eqn{i}-th component distribution.
#' @param type `"probability"` (default) for a probability mixture or
#'   `"quantile"` for a quantile mixture (Vincentization). Quantile mixtures
#'   only support univariate component distributions.
#'
#' @details
#'
#'   In the following, let \eqn{X} be a mixture random variable composed
#'   of \eqn{K} component distributions \eqn{F_1, F_2, \ldots, F_K} with
#'   corresponding weights \eqn{w_1, w_2, \ldots, w_K} where
#'   \eqn{\sum_{i=1}^K w_i = 1} and \eqn{w_i \geq 0} for all \eqn{i}.
#'
#'   ## Probability mixture (`type = "probability"`)
#'
#'   **Support**: The union of the supports of all component distributions
#'
#'   **Mean**:
#'
#'   For univariate mixtures:
#'   \deqn{
#'     E(X) = \sum_{i=1}^K w_i \mu_i
#'   }{
#'     E(X) = sum_{i=1}^K w_i * mu_i
#'   }
#'
#'   where \eqn{\mu_i} is the mean of the \eqn{i}-th component distribution.
#'
#'   For multivariate mixtures:
#'   \deqn{
#'     E(\mathbf{X}) = \sum_{i=1}^K w_i \boldsymbol{\mu}_i
#'   }{
#'     E(X) = sum_{i=1}^K w_i * mu_i
#'   }
#'
#'   where \eqn{\boldsymbol{\mu}_i} is the mean vector of the \eqn{i}-th
#'   component distribution.
#'
#'   **Variance**:
#'
#'   For univariate mixtures:
#'   \deqn{
#'     \text{Var}(X) = \sum_{i=1}^K w_i (\mu_i^2 + \sigma_i^2) - \left(\sum_{i=1}^K w_i \mu_i\right)^2
#'   }{
#'     Var(X) = sum_{i=1}^K w_i * (mu_i^2 + sigma_i^2) - (sum_{i=1}^K w_i * mu_i)^2
#'   }
#'
#'   where \eqn{\sigma_i^2} is the variance of the \eqn{i}-th component
#'   distribution.
#'
#'   **Covariance**:
#'
#'   For multivariate mixtures:
#'   \deqn{
#'     \text{Cov}(\mathbf{X}) = \sum_{i=1}^K w_i \left[ (\boldsymbol{\mu}_i - \bar{\boldsymbol{\mu}})(\boldsymbol{\mu}_i - \bar{\boldsymbol{\mu}})^T + \boldsymbol{\Sigma}_i \right]
#'   }{
#'     Cov(X) = sum_{i=1}^K w_i * [ (mu_i - mu_bar)(mu_i - mu_bar)^T + Sigma_i ]
#'   }
#'
#'   where \eqn{\bar{\boldsymbol{\mu}} = \sum_{i=1}^K w_i \boldsymbol{\mu}_i}
#'   is the overall mean vector and \eqn{\boldsymbol{\Sigma}_i} is the
#'   covariance matrix of the \eqn{i}-th component distribution.
#'
#'   **Probability density/mass function (p.d.f/p.m.f)**:
#'
#'   \deqn{
#'     f(x) = \sum_{i=1}^K w_i f_i(x)
#'   }{
#'     f(x) = sum_{i=1}^K w_i * f_i(x)
#'   }
#'
#'   where \eqn{f_i(x)} is the density or mass function of the \eqn{i}-th
#'   component distribution.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   For univariate mixtures:
#'   \deqn{
#'     F(x) = \sum_{i=1}^K w_i F_i(x)
#'   }{
#'     F(x) = sum_{i=1}^K w_i * F_i(x)
#'   }
#'
#'   where \eqn{F_i(x)} is the c.d.f. of the \eqn{i}-th component
#'   distribution.
#'
#'   For multivariate mixtures, the c.d.f. is approximated numerically.
#'
#'   **Quantile function**:
#'
#'   For univariate probability mixtures, the quantile function has no closed
#'   form and is computed numerically by inverting the c.d.f. using root-finding
#'   ([stats::uniroot()]).
#'
#'   For multivariate mixtures, quantiles are not yet implemented.
#'
#'   ## Quantile mixture (`type = "quantile"`)
#'
#'   Also known as a *Vincent average* or *Vincentization*, only univariate
#'   component distributions are supported.
#'
#'   **Quantile function** (closed form):
#'   \deqn{
#'     Q(p) = \sum_{i=1}^K w_i Q_i(p)
#'   }{
#'     Q(p) = sum_{i=1}^K w_i * Q_i(p)
#'   }
#'
#'   where \eqn{Q_i(p)} is the quantile function of the \eqn{i}-th component
#'   distribution.
#'
#'   **Cumulative distribution function**: computed numerically by inverting
#'   \eqn{Q(p)} via [stats::uniroot()].
#'
#'   **Probability density function**: derived analytically from the quantile
#'   function. For \eqn{p = F(x)}:
#'   \deqn{
#'     f(x) = \frac{1}{Q'(p)} = \frac{1}{\sum_{i=1}^K w_i / f_i(Q_i(p))}
#'   }{
#'     f(x) = 1 / sum_{i=1}^K w_i / f_i(Q_i(p))
#'   }
#'
#'   **Mean**: \eqn{E(X) = \sum_{i=1}^K w_i \mu_i} (identical to the
#'   probability mixture mean).
#'
#'   **Variance**: computed numerically as
#'   \eqn{\int_0^1 Q(p)^2 \, dp - \left(E(X)\right)^2}.
#'
#' @seealso [stats::uniroot()], [vctrs::vec_unique_count()]
#'
#' @examples
#' # Probability mixture of two normal distributions (default)
#' dist <- dist_mixture(dist_normal(0, 1), dist_normal(5, 2), weights = c(0.3, 0.7))
#' dist
#'
#' mean(dist)
#' variance(dist)
#'
#' density(dist, 2)
#' cdf(dist, 2)
#' quantile(dist, 0.5)
#'
#' generate(dist, 10)
#'
#' # Quantile mixture (Vincentization) of two normal distributions
#' vdist <- dist_mixture(
#'   dist_normal(0, 1), dist_normal(5, 2),
#'   weights = c(0.3, 0.7), type = "quantile"
#' )
#' vdist
#'
#' mean(vdist)
#' variance(vdist)
#'
#' density(vdist, 2)
#' cdf(vdist, 2)
#' quantile(vdist, 0.5)
#'
#' @name dist_mixture
#' @export
dist_mixture <- function(..., weights = numeric(), type = c("probability", "quantile")) {
  type <- match.arg(type)
  dist <- dots_list(...)

  vec_is(weights, numeric(), length(dist))

  check_mixture_weights(weights)

  # Recycle the components to a common length, then build the `distribution`
  # vector of components for each output position.
  dist <- do.call(vec_recycle_common, dist)
  lapply(dist, vec_assert, ptype = new_dist())
  n <- vec_size(dist[[1L]])
  parts <- lapply(dist, vec_chop)
  comps <- lapply(seq_len(n), function(i) vec_c(!!!lapply(parts, `[[`, i)))
  w <- rep(list(weights), n)

  if (type == "quantile") {
    # Quantile mixtures require univariate components
    dims <- vapply(dist, function(d) dim(dist_elem(d, 1)), integer(1L))
    if (any(dims > 1L)) {
      abort("Quantile mixtures only support univariate distributions.")
    }
    return(new_dist(dist = comps, w = w, class = "dist_q_mixture"))
  }

  dn <- unique(lapply(dist, variates))
  dn <- if (length(dn) == 1) dn[[1]] else NULL

  new_dist(dist = comps, w = w, class = "dist_mixture", dimnames = dn)
}

check_mixture_weights <- function(weights) {
  if (!abs(sum(weights) - 1) < sqrt(.Machine$double.eps)) {
    abort("Weights of a mixture model must sum to 1.")
  }
  if (any(weights < 0)) {
    abort("All weights in a mixture model must be non-negative.")
  }
  invisible(weights)
}

# ---- Helpers -----------------------------------------------------------------

# A mixture record holds a list of `distribution` vectors (the K components of
# each mixture) and a list of weights, both ragged. All of the components are
# flattened into a single `distribution` vector, so that each family's methods
# are called once with every component of every mixture. `idx` maps each
# component back to its mixture. The fields are read with `field()`, as `[[`
# unwraps a size 1 list field.
mixture_parts <- function(x) {
  comps <- field(x, "dist")
  w <- field(x, "w")
  list(
    flat = vec_c(!!!comps),
    w = unlist(w, use.names = FALSE),
    idx = rep(seq_along(w), lengths(w)),
    n = length(w),
    comps = comps,
    w_list = w
  )
}

# Repeat the broadcast argument (size `n` or 1) for each of the components
mixture_arg <- function(arg, idx) {
  vec_slice(arg, if (vec_size(arg) == 1L) rep_len(1L, length(idx)) else idx)
}

# Weighted sum of the components' values (a vector or a matrix with a row for
# each component) in each mixture
mixture_sum <- function(v, parts) {
  out <- rowsum(parts$w * v, parts$idx, reorder = FALSE)
  if (is.matrix(v)) {
    dimnames(out) <- NULL
    out
  } else {
    as.vector(out)
  }
}

# Call the generic `.f` on the flattened components without a broadcast argument
mixture_apply <- function(parts, .f, ...) {
  apply_slots(parts$flat, .f, list2(...))
}

mixture_dim <- function(x) {
  dim(dist_elem(field(x, "dist")[[1L]], 1L))
}

mixture_family <- function(object, fam, recursive = FALSE, ...) {
  if (!recursive) return(rep_len(fam, vec_size(object)))
  comps <- field(object, "dist")
  if (vec_size(object) != 1L) {
    return(lapply(vec_seq_along(object), function(i) {
      family(vec_slice(object, i), recursive = TRUE)
    }))
  }
  list(family = fam, dist = unname(family(comps[[1L]], recursive = TRUE)))
}

mixture_format <- function(x, fam, width, ...) {
  comps <- field(x, "dist")
  w <- field(x, "w")
  vapply(seq_along(w), function(i) {
    # Each component is formatted alone, so that they are not padded to a common width
    dists <- vapply(vec_chop(comps[[i]]), format, character(1L), ...)
    long_dist <- paste0(fam, "(", paste0(w[[i]], "*", dists, collapse = ", "), ")")
    short_dist <- paste0(fam, "(n=", length(dists), ")")
    if (nchar(long_dist) <= width) long_dist else short_dist
  }, character(1L))
}

# ---- Probability mixture -----------------------------------------------------

#' @export
format.dist_mixture <- function(x, width = getOption("width"), ...) {
  mixture_format(x, "mixture", width, ...)
}

#' @export
family.dist_mixture <- function(object, recursive = FALSE, ...) {
  mixture_family(object, "mixture", recursive)
}

#' @export
density.dist_mixture <- function(x, at, ...) {
  parts <- mixture_parts(x)
  mixture_sum(dist_paired(density, parts$flat, at = mixture_arg(at, parts$idx), ...), parts)
}

#' @export
quantile.dist_mixture <- function(x, p, ..., tail = "lower") {
  if (mixture_dim(x) > 1) {
    stop("quantile is not implemented for multivariate mixtures.")
  }
  parts <- mixture_parts(x)
  p <- vec_recycle(p, parts$n)

  # Find bounds for optimisation based on range of each quantile
  dist_q <- dist_paired(quantile, parts$flat, p = mixture_arg(p, parts$idx), ..., tail = tail)
  dist_q <- split(dist_q, factor(parts$idx, seq_len(parts$n)))

  vapply(seq_len(parts$n), function(i) {
    dq <- unname(dist_q[[i]])
    pi <- p[i]
    if (is.na(pi)) return(NA_real_)
    if (vctrs::vec_unique_count(dq) == 1) return(dq[1])
    if (pi == 0) return(if (tail == "lower") min(dq) else max(dq))
    if (pi == 1) return(if (tail == "lower") max(dq) else min(dq))

    # Search the cdf() for appropriate quantile
    comps <- parts$comps[[i]]
    w <- parts$w_list[[i]]
    stats::uniroot(
      function(pos) pi - sum(w * dist_paired(cdf, comps, q = pos, ..., tail = tail)),
      interval = c(min(dq), max(dq)),
      extendInt = "yes"
    )$root
  }, numeric(1L))
}

#' @export
cdf.dist_mixture <- function(x, q, ..., tail = "lower") {
  if (mixture_dim(x) == 1L) {
    parts <- mixture_parts(x)
    mixture_sum(dist_paired(cdf, parts$flat, q = mixture_arg(q, parts$idx), ..., tail = tail), parts)
  } else {
    NextMethod()
  }
}

#' @export
generate.dist_mixture <- function(x, times, ...) {
  parts <- mixture_parts(x)
  n <- parts$n
  d <- mixture_dim(x)
  times <- vec_recycle(times, n)

  # Choose a component for each draw. Offsetting the cumulative weights and
  # the uniform draws by the mixture number finds all of the components at once.
  cw <- unlist(lapply(parts$w_list, function(w) {
    cw <- cumsum(w)
    cw[length(cw)] <- 1
    cw
  }), use.names = FALSE) + (parts$idx - 1)
  e <- rep(seq_len(n), times)
  comp <- .bincode(stats::runif(sum(times)) + (e - 1), c(0, cw), TRUE, TRUE)

  # Generate from each component the number of times that it was chosen
  m <- tabulate(comp, nbins = length(cw))
  keep <- which(m > 0L)
  draws <- dist_paired(generate, vec_slice(parts$flat, keep), times = m[keep], ...)
  draws <- if (d == 1L) unlist(draws, use.names = FALSE) else do.call(rbind, draws)

  r <- if (d == 1L) numeric(length(comp)) else matrix(NA_real_, length(comp), d)
  ord <- order(comp)
  if (d == 1L) r[ord] <- draws else r[ord, ] <- draws
  vec_chop(r, sizes = times)
}

#' @export
mean.dist_mixture <- function(x, ...) {
  parts <- mixture_parts(x)
  mixture_sum(mixture_apply(parts, mean, ...), parts)
}

#' @export
covariance.dist_mixture <- function(x, ...) {
  parts <- mixture_parts(x)
  m <- mixture_apply(parts, mean, ...)
  if (mixture_dim(x) == 1L) {
    v <- mixture_apply(parts, variance, ...)
    m1 <- mixture_sum(m, parts)
    m2 <- mixture_sum(m^2 + v, parts)
    m2 - m1^2
  } else {
    v <- mixture_apply(parts, covariance, ...)
    mbar <- mixture_sum(m, parts)
    lapply(seq_len(parts$n), function(i) {
      k <- which(parts$idx == i)
      dev <- m[k, , drop = FALSE] - rep(mbar[i, ], each = length(k))
      w <- parts$w[k]
      # Weighted spread of the component means plus weighted covariances
      Reduce(`+`, Map(`*`, w, v[k]), crossprod(dev * sqrt(w)))
    })
  }
}

#' @export
support.dist_mixture <- function(x, ...) {
  parts <- mixture_parts(x)
  sup <- support(parts$flat)
  proto <- split(field(sup, "x"), parts$idx)
  lim <- split(field(sup, "lim"), parts$idx)
  closed <- split(field(sup, "closed"), parts$idx)

  # The support is the union of the supports of the components
  union_lim <- lapply(seq_len(parts$n), function(i) {
    l <- lim[[i]]
    if (is.matrix(l[[1L]])) {
      lo <- do.call(pmin, lapply(l, function(z) z[1L, ]))
      hi <- do.call(pmax, lapply(l, function(z) z[2L, ]))
      return(rbind(lo, hi, deparse.level = 0))
    }
    c(min(vapply(l, `[`, numeric(1L), 1L)), max(vapply(l, `[`, numeric(1L), 2L)))
  })
  union_closed <- lapply(seq_len(parts$n), function(i) {
    l <- lim[[i]]
    cl <- closed[[i]]
    at_bound <- function(j, bound) {
      vapply(seq_along(l), function(k) {
        z <- if (is.matrix(l[[k]])) l[[k]][j, ] else l[[k]][j]
        isTRUE(cl[[k]][j]) && all(z == bound, na.rm = TRUE)
      }, logical(1L))
    }
    ul <- union_lim[[i]]
    if (is.matrix(ul)) {
      return(c(any(at_bound(1L, ul[1L, ])), any(at_bound(2L, ul[2L, ]))))
    }
    c(any(at_bound(1L, ul[1L])), any(at_bound(2L, ul[2L])))
  })
  union_proto <- lapply(proto, function(p) {
    if (all(vapply(p, function(z) is.integer(z) || is.logical(z), logical(1L)))) {
      p[[1L]]
    } else {
      p[[which(!vapply(p, function(z) is.integer(z) || is.logical(z), logical(1L)))[1L]]]
    }
  })
  new_support_region(unname(union_proto), union_lim, union_closed)
}

#' @export
dim.dist_mixture <- function(x) {
  mixture_dim(x)
}

# ---- Quantile mixture --------------------------------------------------------

#' @export
format.dist_q_mixture <- function(x, width = getOption("width"), ...) {
  mixture_format(x, "mixture<q>", width, ...)
}

#' @export
family.dist_q_mixture <- function(object, recursive = FALSE, ...) {
  mixture_family(object, "q_mixture", recursive)
}

# The quantile function of the mixture of the components of each mixture,
# where `p` is paired with the mixtures (size `n` or 1)
q_mixture_quantile <- function(parts, p, ..., tail = "lower") {
  mixture_sum(
    dist_paired(quantile, parts$flat, p = mixture_arg(p, parts$idx), ..., tail = tail),
    parts
  )
}

#' @export
quantile.dist_q_mixture <- function(x, p, ..., tail = "lower") {
  q_mixture_quantile(mixture_parts(x), p, ..., tail = tail)
}

#' @export
cdf.dist_q_mixture <- function(x, q, ..., tail = "lower") {
  parts <- mixture_parts(x)
  n <- parts$n
  q <- vec_recycle(q, n)

  # Compute support bounds directly from the quantile function to avoid
  # calling support(x), which would trigger density() -> cdf() recursion.
  lims_lo <- q_mixture_quantile(parts, 0, ...)
  lims_hi <- q_mixture_quantile(parts, 1, ...)

  vapply(seq_len(n), function(i) {
    qi <- q[i]
    if (is.na(qi)) return(NA_real_)

    # q is at or below the lower bound of the mixture support
    if (qi <= lims_lo[i]) return(if (tail == "lower") 0 else 1)
    # q is at or above the upper bound of the mixture support
    if (qi >= lims_hi[i]) return(if (tail == "lower") 1 else 0)

    # Find p in (0, 1) such that Q(p) = q by inverting the quantile function.
    # For the upper tail this inverts the upper tail quantile function, which
    # keeps precision for small upper tail probabilities.
    comps <- parts$comps[[i]]
    w <- parts$w_list[[i]]
    fn <- function(p) {
      sum(w * dist_paired(quantile, comps, p = p, ..., tail = tail)) - qi
    }

    # Use the smallest representable eps so the search interval covers
    # far tails (e.g., normal distributions). For q beyond Q(eps) or Q(1-eps),
    # return the boundary probability rather than erroring in uniroot.
    # The upper tail quantile function is decreasing, so its sign is flipped.
    eps <- .Machine$double.eps
    s <- if (tail == "lower") 1 else -1
    lo_val <- s * fn(eps)
    hi_val <- s * fn(1 - eps)

    if (lo_val >= 0) return(eps)
    if (hi_val <= 0) return(1 - eps)

    stats::uniroot(fn, interval = c(eps, 1 - eps), tol = 1e-8)$root
  }, numeric(1L))
}

#' @export
support.dist_q_mixture <- function(x, ...) {
  parts <- mixture_parts(x)
  lims <- cbind(q_mixture_quantile(parts, 0, ...), q_mixture_quantile(parts, 1, ...))
  new_support_region(
    rep(list(numeric(0)), parts$n),
    lapply(seq_len(parts$n), function(i) lims[i, ]),
    lapply(seq_len(parts$n), function(i) is.finite(lims[i, ]))
  )
}

#' @export
density.dist_q_mixture <- function(x, at, ...) {
  parts <- mixture_parts(x)
  at <- vec_recycle(at, parts$n)

  # p = F(at) via CDF inversion
  p <- cdf(x, at, ...)

  # Outside (or at boundary of) the support the density is 0
  out <- ifelse(is.na(p), NA_real_, 0)
  valid <- which(!is.na(p) & p > 0 & p < 1)
  if (!length(valid)) return(out)

  # Q'(p) = sum(w_i * Q_i'(p)) = sum(w_i / f_i(Q_i(p)))
  # => f(at) = 1 / Q'(p)
  use <- parts$idx %in% valid
  flat <- vec_slice(parts$flat, use)
  qi_vals <- dist_paired(quantile, flat, p = p[parts$idx[use]], ...)
  fi_vals <- dist_paired(density, flat, at = qi_vals, ...)
  w <- parts$w[use]
  idx <- parts$idx[use]
  zero <- unique(idx[fi_vals == 0])
  out[valid] <- as.vector(1 / rowsum(w / fi_vals, idx, reorder = FALSE))
  out[zero] <- 0
  out
}

#' @export
generate.dist_q_mixture <- function(x, times, ...) {
  # Inverse transform sampling, with each distribution repeated `times` times
  x_rep <- vec_slice(x, rep(vec_seq_along(x), times))
  vec_chop(quantile(x_rep, stats::runif(sum(times)), ...), sizes = times)
}

#' @export
mean.dist_q_mixture <- function(x, ...) {
  # E[X] = integral_0^1 Q(p) dp = sum(w_i * mu_i)
  parts <- mixture_parts(x)
  mixture_sum(mixture_apply(parts, mean, ...), parts)
}

#' @export
covariance.dist_q_mixture <- function(x, ...) {
  # Var(X) = E[X^2] - (E[X])^2
  # E[X^2] = integral_0^1 Q(p)^2 dp  (numerical)
  parts <- mixture_parts(x)
  mu <- mean(x, ...)
  vapply(seq_len(parts$n), function(i) {
    comps <- parts$comps[[i]]
    w <- parts$w_list[[i]]
    q_fn <- function(p) {
      colSums(w * do.call(rbind, quantile(comps, p, ...)))
    }
    e2 <- stats::integrate(
      function(p) q_fn(p)^2,
      lower = 0, upper = 1,
      rel.tol = 1e-5
    )$value
    e2 - mu[i]^2
  }, numeric(1L))
}

#' @export
dim.dist_q_mixture <- function(x) {
  1L
}
