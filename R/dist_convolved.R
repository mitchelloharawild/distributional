#' A convolved distribution
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Creates the distribution of the sum of two or more independent random
#' variables using numerical convolution of their distributions with a Fast
#' Fourier Transform (FFT).
#'
#' The convolution used depends on the support of the distributions being
#' summed. A sum of distributions with lattice (integer) support is convolved
#' exactly on the integer lattice, giving the probability mass function of the
#' result. Any other sum is convolved on a common grid of cell probabilities,
#' from which the density is obtained and interpolated with
#' [stats::approxfun()], and the cumulative distribution and quantile functions
#' are derived by accumulation and inversion respectively.
#'
#' This is primarily intended to be used via arithmetic on distributions:
#' `dist1 + dist2` or `dist1 + dist2 + dist3 + ...`. Distributions with known
#' closed-form sums (e.g. two [dist_normal()]) will use the exact result rather
#' than this approximation. Chaining `+` automatically performs a single k-way
#' FFT convolution rather than nested binary convolutions, avoiding compounding
#' approximation errors.
#'
#' @param ... Two or more distribution vectors to sum. All will be recycled to
#'   a common length.
#'
#' @details
#'
#'   Let \eqn{Z = X_1 + X_2 + \cdots + X_k} where the \eqn{X_i} are
#'   independent random variables.
#'
#'   **Mean**:
#'   \deqn{E(Z) = \sum_{i=1}^{k} E(X_i)}
#'
#'   **Variance**:
#'   \deqn{\mathrm{Var}(Z) = \sum_{i=1}^{k} \mathrm{Var}(X_i)}
#'
#'   **Probability density function (p.d.f)**:
#'   \deqn{f_Z(z) = (f_{X_1} * f_{X_2} * \cdots * f_{X_k})(z)}
#'
#'   **Support**: \eqn{\sum_i \inf S_{X_i}} to \eqn{\sum_i \sup S_{X_i}}, which
#'   is discrete if (and only if) all of the \eqn{X_i} are discrete.
#'
#'   The k-way convolution is computed in a single FFT pass: each component is
#'   discretised onto a common grid, transformed via FFT, all transforms are
#'   multiplied element-wise, and a single inverse FFT yields the result. This
#'   avoids compounding approximation errors from nested binary convolutions.
#'
#'   Components are discretised into probabilities rather than densities, which
#'   preserves their mass regardless of how the scales of the distributions
#'   being summed compare. When all of the distributions are supported on the
#'   integers this is done exactly on the integer lattice, and the result is a
#'   discrete distribution with an exact probability mass function. Otherwise
#'   the distributions are discretised into the probabilities of the cells of a
#'   common grid (which is aligned with the integers if any of the
#'   distributions are discrete, so that their atoms are also exact).
#'
#'   The accuracy of the FFT approximation can be controlled by passing `n`
#'   (number of grid cells, default `2^12`) and `tail_p` (tail probability used
#'   to find finite grid bounds for distributions with infinite support, default
#'   `1e-6`) to `density()`, `cdf()`, `quantile()`, or `generate()`. A warning
#'   is given if the grid is too coarse to resolve any of the distributions
#'   being summed.
#'
#' @examples
#' # Sum of a lognormal and an exponential (no closed-form result)
#' d <- dist_convolved(dist_lognormal(0, 1), dist_exponential(1))
#' d
#'
#' density(d, 2)
#' cdf(d, 2)
#' quantile(d, 0.5)
#' generate(d, 5)
#'
#' # Three distributions from different families
#' d3 <- dist_convolved(dist_lognormal(0, 0.5), dist_gamma(2, 1), dist_exponential(2))
#' d3
#'
#' # Via arithmetic
#' d2 <- dist_lognormal(0, 1) + dist_exponential(1)
#' density(d2, 2)
#'
#' # Sums of discrete distributions are computed exactly on the integer lattice
#' d_pois <- dist_poisson(2) + dist_poisson(3)
#' density(d_pois, 5) # dpois(5, 5)
#' support(d_pois)
#'
#' # Mean and variance are computed exactly from components
#' mean(d)
#' variance(d)
#'
#' @seealso [stats::convolve()], [stats::approxfun()]
#'
#' @name dist_convolved
#' @importFrom stats fft approxfun
#' @export
dist_convolved <- function(...) {
  dists <- dots_list(...)

  if (length(dists) < 2L) {
    abort("`dist_convolved()` requires at least two distributions.")
  }
  lapply(dists, vec_assert, ptype = new_dist())

  # Recycle inputs to a common length, then build a flat component list for
  # each output position, unpacking any nested dist_convolved inputs.
  recycled <- do.call(vctrs::vec_recycle_common, dists)
  n <- vctrs::vec_size(recycled[[1L]])

  components_by_pos <- lapply(seq_len(n), function(i) {
    unlist(
      lapply(recycled, function(d) {
        r <- vec_data(d)[[i]]
        if (inherits(r, "dist_convolved")) r[["dist"]] else list(r)
      }),
      recursive = FALSE
    )
  })

  new_dist(dist = components_by_pos, class = "dist_convolved")
}


#' @export
format.dist_convolved <- function(x, ...) {
  parts <- vapply(x[["dist"]], format, character(1L))
  paste(parts, collapse = " + ")
}

#' @export
density.dist_convolved <- function(x, at, ..., n = 2^12, tail_p = 1e-6) {
  convolve_approx(x, n = n, tail_p = tail_p)$dens_fn(at)
}

#' @export
cdf.dist_convolved <- function(x, q, ..., n = 2^12, tail_p = 1e-6) {
  convolve_approx(x, n = n, tail_p = tail_p)$cdf_fn(q)
}

#' @export
quantile.dist_convolved <- function(x, p, ..., n = 2^12, tail_p = 1e-6) {
  convolve_approx(x, n = n, tail_p = tail_p)$qtl_fn(p)
}

#' @export
generate.dist_convolved <- function(x, times, ..., n = 2^12, tail_p = 1e-6) {
  convolve_approx(x, n = n, tail_p = tail_p)$qtl_fn(stats::runif(times))
}

#' @export
mean.dist_convolved <- function(x, ...) {
  Reduce(`+`, lapply(x[["dist"]], mean))
}

#' @export
covariance.dist_convolved <- function(x, ...) {
  Reduce(`+`, lapply(x[["dist"]], covariance))
}

#' @export
support.dist_convolved <- function(x, ...) {
  info <- convolve_support(x[["dist"]])

  # The support of a sum spans the sum of the component limits.
  lims <- c(sum(info$lims[1L, ]), sum(info$lims[2L, ]))

  # The sum is discrete only if all of the components are discrete.
  proto <- if (all(info$lattice)) integer() else numeric()

  closed <- if (any(is.na(lims))) {
    c(FALSE, FALSE)
  } else {
    lim_dens <- tryCatch(
      suppressWarnings(density(x, lims)),
      error = function(e) c(0, 0)
    )
    !near(lim_dens, 0)
  }

  new_support_region(list(proto), list(lims), list(closed))
}

# Describe the support of each component of a convolution:
#
# * `lattice`: is the component supported on (a subset of) the integers? The
#   prototype stored by `support()` is integer (or logical) for lattice
#   families, and is preserved through transformations of those families.
# * `lims`: the (possibly infinite) limits of the region of mass.
# * `bounds`: `lims`, with infinite tails truncated at `tail_p`. Only computed
#   when `tail_p` is given.
convolve_support <- function(dists, tail_p = NULL) {
  sup <- lapply(dists, function(d) tryCatch(support(d), error = function(e) NULL))

  lims <- vapply(seq_along(dists), function(i) {
    lim <- if (is.null(sup[[i]])) {
      as.numeric(quantile(dists[[i]], c(0, 1)))
    } else {
      as.numeric(field(sup[[i]], "lim")[[1L]])
    }
    if (length(lim) == 2L) lim else c(NA_real_, NA_real_)
  }, numeric(2L))

  lattice <- vapply(seq_along(dists), function(i) {
    d <- dists[[i]]
    if (inherits(d, "dist_degenerate")) {
      # A point mass is on the lattice if it sits on an integer.
      return(is.finite(d[["x"]]) && d[["x"]] == trunc(d[["x"]]))
    }
    if (is.null(sup[[i]])) return(FALSE)
    proto <- field(sup[[i]], "x")[[1L]]
    is.integer(proto) || is.logical(proto)
  }, logical(1L))

  bounds <- if (is.null(tail_p)) NULL else vapply(seq_along(dists), function(i) {
    c(
      if (is.finite(lims[1L, i])) lims[1L, i] else as.numeric(quantile(dists[[i]], tail_p)),
      if (is.finite(lims[2L, i])) lims[2L, i] else as.numeric(quantile(dists[[i]], 1 - tail_p))
    )
  }, numeric(2L))

  list(lattice = lattice, lims = lims, bounds = bounds)
}

# Compute the k-way convolution approximation for a single dist_convolved
# object. Components are already flat (no nested dist_convolved).
#
# The method used depends on the support of the components: a sum of lattice
# (integer supported) distributions is convolved exactly on the integer
# lattice, while any sum involving a continuous component is convolved on a
# common continuous grid.
convolve_approx <- function(x, n, tail_p) {
  dists <- x[["dist"]]
  info <- convolve_support(dists, tail_p = tail_p)

  if (all(info$lattice)) {
    approx <- convolve_lattice(dists, info, tail_p = tail_p)
    # `NULL` if the components are not resolvable on the integer lattice,
    # in which case the continuous grid is used (with a warning).
    if (!is.null(approx)) return(approx)
  }

  convolve_grid(dists, info, n = n)
}

# Exact convolution of lattice distributions, computed on the integer lattice
# spanned by the components. Returns `NULL` if the components cannot be
# represented on that lattice.
convolve_lattice <- function(dists, info, tail_p) {
  k <- length(dists)

  lo <- floor(info$bounds[1L, ])
  hi <- ceiling(info$bounds[2L, ])
  if (anyNA(lo) || anyNA(hi)) return(NULL)

  # Guard against unreasonably large lattices (the grid approximation is used
  # instead, trading exactness for a bounded amount of work).
  if (sum(hi - lo + 1) > 2^22) {
    warn(
      "The support of the convolved distributions is too large to convolve exactly, approximating the result on a grid instead."
    )
    return(NULL)
  }

  atoms <- Map(seq.int, lo, hi)
  pmf <- lapply(seq_len(k), function(i) {
    pmax(suppressWarnings(density(dists[[i]], atoms[[i]])), 0)
  })

  # A component supported on a coarser or shifted lattice (for example a
  # scaled lattice distribution) will not carry its full mass on the integer
  # lattice, and so must be approximated on a continuous grid instead.
  resolved <- vapply(
    pmf, function(p) isTRUE(sum(p) >= 1 - 1e-3 - 4 * tail_p), logical(1L)
  )
  if (!all(resolved)) {
    warn(
      "The atoms of some convolved distributions do not lie on the integer lattice, approximating the result on a grid instead."
    )
    return(NULL)
  }

  # ---- k-way FFT convolution on the lattice ---------------------------------
  L <- sum(hi - lo) + 1L
  L_fft <- 2L^ceiling(log2(L))
  ffts <- lapply(pmf, function(p) fft(c(p, rep(0, L_fft - length(p)))))
  p_conv <- Re(fft(Reduce(`*`, ffts), inverse = TRUE)) / L_fft
  p_conv <- pmax(p_conv[seq_len(L)], 0)

  # The convolution is exact (up to floating point) over the retained atoms,
  # so the probabilities are deliberately not standardised: any mass lost to
  # `tail_p` truncation is beyond the largest retained atom.
  z <- sum(lo) + seq_len(L) - 1
  cdf_vals <- cumsum(p_conv)

  # True (untruncated) upper limit of the support of the sum.
  sup_hi <- sum(info$lims[2L, ])

  dens_fn <- function(at) {
    out <- rep_len(0, length(at))
    i <- match(at, z)
    out[!is.na(i)] <- p_conv[i[!is.na(i)]]
    out[is.na(at)] <- NA_real_
    out
  }
  cdf_fn <- function(q) {
    out <- c(0, cdf_vals)[findInterval(q, z) + 1L]
    pmin(pmax(out, 0), 1)
  }
  qtl_fn <- function(p) {
    # The smallest atom `z` for which P(Z <= z) >= p.
    i <- findInterval(p, cdf_vals, left.open = TRUE) + 1L
    out <- ifelse(i > L, sup_hi, z[pmin(i, L)])
    out[!is.na(p) & (p < 0 | p > 1)] <- NaN
    out
  }

  list(dens_fn = dens_fn, cdf_fn = cdf_fn, qtl_fn = qtl_fn)
}

# Approximate convolution on a common continuous grid. Components are
# discretised into cell probabilities (rather than sampled densities), which
# preserves mass and captures the jumps of any discrete component.
convolve_grid <- function(dists, info, n) {
  k <- length(dists)
  lattice <- info$lattice

  # ---- build evaluation grid ------------------------------------------------
  b_lo <- info$bounds[1L, ]
  b_hi <- info$bounds[2L, ]

  # Common input grid spanning the union of all component supports
  x_lo <- min(b_lo)
  x_hi <- max(b_hi)
  if (!isTRUE(x_hi > x_lo)) {
    # All of the mass is at a single point, use a nominal grid width
    eps <- max(1, abs(x_lo)) * 1e-8
    x_lo <- x_lo - eps
    x_hi <- x_hi + eps
  }
  dx <- (x_hi - x_lo) / n

  aligned <- any(lattice) && dx <= 1
  if (aligned) {
    # Align the grid so that each integer is the midpoint of a cell, allowing
    # the atoms of lattice components to be represented exactly.
    dx <- 1 / ceiling(1 / dx)
    x_lo <- (floor(x_lo / dx) - 0.5) * dx
    n <- as.integer(ceiling((x_hi - x_lo) / dx))
    x_hi <- x_lo + n * dx
  }

  # ---- resolution diagnostics -----------------------------------------------
  # The atoms of aligned lattice components are exact, all other components
  # need enough cells across their support to be resolved by the grid.
  if (any(lattice) && !aligned) {
    warn(sprintf(
      "The convolution grid (of width %s) cannot resolve the atoms of the discrete distributions, increase `n` for a more accurate result.",
      format(dx, digits = 3)
    ))
  }
  width <- b_hi - b_lo
  if (any(!(aligned & lattice) & width > 0 & width < 40 * dx)) {
    warn(sprintf(
      "The convolution grid (of width %s) is too coarse to resolve some of the distributions, increase `n` for a more accurate result.",
      format(dx, digits = 3)
    ))
  }

  # ---- discretise each component into cell probabilities --------------------
  # Cell `i` spans [x_lo + (i-1)*dx, x_lo + i*dx) and is represented by its
  # midpoint. Using probabilities rather than densities is mass preserving,
  # and picks up the atoms of discrete components via the jumps in the cdf.
  x_edges <- x_lo + seq.int(0, n) * dx
  mass_vecs <- lapply(dists, function(d) {
    pmax(diff(suppressWarnings(cdf(d, x_edges))), 0)
  })

  # ---- k-way FFT convolution ------------------------------------------------
  # Linear convolution of k vectors each of length n has length k*(n-1)+1.
  L <- k * (n - 1L) + 1L

  # Pad to next power of 2 for efficient FFT
  L_fft <- 2L^ceiling(log2(L))

  # FFT each zero-padded component, then multiply all element-wise
  ffts <- lapply(mass_vecs, function(mv) {
    fft(c(mv, rep(0, L_fft - n)))
  })
  combined_fft <- Reduce(`*`, ffts)

  # Inverse FFT; normalization convention: divide by L_fft
  p_conv <- Re(fft(combined_fft, inverse = TRUE)) / L_fft

  # Discard circular-wrap artefacts; keep only the L valid output points
  # and clamp numerical artefacts (tiny negative values)
  p_conv <- pmax(p_conv[seq_len(L)], 0)

  # Standardise, recovering the mass lost to `tail_p` truncation
  total <- sum(p_conv)
  if (total > 0) p_conv <- p_conv / total

  # ---- output grid ----------------------------------------------------------
  # Summing k cell midpoints offsets the output grid by k*dx/2.
  x_conv <- k * x_lo + k * dx / 2 + (seq_len(L) - 1) * dx

  # ---- density approxfun ----------------------------------------------------
  dens_fn <- approxfun(x_conv, p_conv / dx, rule = 2, yleft = 0, yright = 0)

  # ---- CDF from the cumulative cell probabilities ---------------------------
  # The cell probabilities accumulate at the upper edge of each output cell,
  # so linear interpolation of the cdf is the integral of the density above.
  x_cdf <- c(x_conv[1L] - dx / 2, x_conv + dx / 2)
  cdf_vals <- pmin(pmax(c(0, cumsum(p_conv)), 0), 1)

  cdf_fn <- approxfun(x_cdf, cdf_vals, rule = 2, yleft = 0, yright = 1)

  # ---- quantile function (inverse CDF) --------------------------------------
  unique_idx <- !duplicated(cdf_vals)
  qtl_fn <- approxfun(
    cdf_vals[unique_idx], x_cdf[unique_idx],
    rule = 2
  )

  list(dens_fn = dens_fn, cdf_fn = cdf_fn, qtl_fn = qtl_fn)
}
