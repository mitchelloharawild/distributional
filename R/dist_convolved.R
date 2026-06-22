#' A convolved distribution
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Creates the distribution of the sum of two or more independent random
#' variables using numerical convolution of their densities. The density of
#' the result is approximated on a grid using a Fast Fourier Transform (FFT),
#' and then interpolated with [stats::approxfun()].
#'
#' The cumulative distribution function and quantile function are derived from
#' the approximated density via numerical integration and inversion respectively.
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
#'   The k-way convolution is computed in a single FFT pass: each component
#'   density is evaluated on a common grid, transformed via FFT, all transforms
#'   are multiplied element-wise, and a single inverse FFT yields the result.
#'   This avoids compounding approximation errors from nested binary convolutions.
#'
#'   The accuracy of the FFT approximation can be controlled by passing `n`
#'   (number of grid points, default `2^12`) and `tail_p` (tail probability used
#'   to find finite grid bounds for distributions with infinite support, default
#'   `1e-6`) to `density()`, `cdf()`, `quantile()`, or `generate()`.
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
family.dist_convolved <- function(object, ...) {
  "convolved"
}

# Compute the FFT-based k-way convolution approximation for a single
# dist_convolved object. Components are already flat (no nested dist_convolved).
convolve_approx <- function(x, n, tail_p) {
  dists <- x[["dist"]]
  k <- length(dists)

  # ---- build evaluation grid ------------------------------------------------

  # Support bounds per component; use tail_p quantiles to bound infinite tails
  bounds <- lapply(dists, function(d) {
    lo <- quantile(d, 0)
    if (is.infinite(lo)) lo <- quantile(d, tail_p)
    hi <- quantile(d, 1)
    if (is.infinite(hi)) hi <- quantile(d, 1 - tail_p)
    c(lo, hi)
  })

  # Common input grid spanning the union of all component supports
  x_lo <- min(vapply(bounds, `[`, numeric(1L), 1L))
  x_hi <- max(vapply(bounds, `[`, numeric(1L), 2L))

  x_grid <- seq(x_lo, x_hi, length.out = n)
  dx     <- (x_hi - x_lo) / n   # step size for integral scaling

  # ---- evaluate densities on common grid ------------------------------------
  dens_vecs <- lapply(dists, function(d) {
    pmax(density(d, x_grid), 0)
  })

  # ---- k-way FFT convolution ------------------------------------------------
  # Linear convolution of k vectors each of length n has length k*(n-1)+1.
  L <- k * (n - 1L) + 1L

  # Pad to next power of 2 for efficient FFT
  L_fft <- 2L ^ ceiling(log2(L))

  # FFT each zero-padded component, then multiply all element-wise
  ffts <- lapply(dens_vecs, function(dv) {
    fft(c(dv, rep(0, L_fft - n)))
  })
  combined_fft <- Reduce(`*`, ffts)

  # Inverse FFT; normalization convention: divide by L_fft
  d_conv <- Re(fft(combined_fft, inverse = TRUE)) / L_fft

  # Discard circular-wrap artefacts; keep only the L valid output points
  d_conv <- d_conv[seq_len(L)]

  # Scale: each of the (k-1) pairwise convolution steps contributes one dx
  d_conv <- d_conv * dx ^ (k - 1L)

  # Clamp numerical artefacts (tiny negative values)
  d_conv <- pmax(d_conv, 0)

  # ---- output grid ----------------------------------------------------------
  x_conv <- seq(k * x_lo, k * x_hi, length.out = L)

  # ---- density approxfun ----------------------------------------------------
  dens_fn <- approxfun(x_conv, d_conv, rule = 2, yleft = 0, yright = 0)

  # ---- CDF via cumulative trapezoidal integration ---------------------------
  dx_conv  <- x_conv[2L] - x_conv[1L]
  trap_mid <- (d_conv[-L] + d_conv[-1L]) / 2 * dx_conv
  cdf_vals <- c(0, cumsum(trap_mid))

  mx <- max(cdf_vals)
  if (mx > 0) cdf_vals <- cdf_vals / mx
  cdf_vals <- pmin(pmax(cdf_vals, 0), 1)

  cdf_fn <- approxfun(x_conv, cdf_vals, rule = 2, yleft = 0, yright = 1)

  # ---- quantile function (inverse CDF) --------------------------------------
  unique_idx <- !duplicated(cdf_vals)
  qtl_fn <- approxfun(
    cdf_vals[unique_idx], x_conv[unique_idx],
    rule = 2
  )

  list(dens_fn = dens_fn, cdf_fn = cdf_fn, qtl_fn = qtl_fn)
}
