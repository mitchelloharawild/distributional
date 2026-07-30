#' Density distribution
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The density distribution is a non-parametric distribution defined by the
#' values of its probability density function at a set of points. This is
#' useful for representing a distribution which is known only through its
#' density, such as a kernel density estimate, a posterior evaluated over a
#' grid, or a density obtained numerically.
#'
#' The density is linearly interpolated between the given values, and is zero
#' beyond them. All other properties of the distribution are computed exactly
#' from that interpolation.
#'
#' @param x A list of values
#' @param density A list of density values at `x`
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_density")`
#'
#'   In the following, let \eqn{X} be a random variable with density
#'   \eqn{f_1, f_2, \ldots, f_n} given at the values
#'   \eqn{x_1 < x_2 < \cdots < x_n}. Write \eqn{w_i = x_{i+1} - x_i} for the
#'   width of the \eqn{i}th interval, and
#'   \eqn{s_i = (f_{i+1} - f_i) / w_i} for the slope of the density over it.
#'
#'   **Support**: \eqn{[x_1, x_n]}
#'
#'   **Probability density function (p.d.f)**: Linear interpolation of the
#'   given values, standardised to integrate to one:
#'
#'   \deqn{
#'     f(t) = f_i + (t - x_i) s_i \quad \text{for } x_i \le t \le x_{i+1}
#'   }{
#'     f(t) = f_i + (t - x_i) s_i for x_i <= t <= x_{i+1}
#'   }
#'
#'   and \eqn{f(t) = 0} for \eqn{t < x_1} or \eqn{t > x_n}.
#'
#'   **Cumulative distribution function (c.d.f)**: The exact integral of the
#'   interpolated density, which is piecewise quadratic. With
#'   \eqn{h = t - x_i} and \eqn{F_i = F(x_i)},
#'
#'   \deqn{
#'     F(t) = F_i + f_i h + \frac{s_i h^2}{2}
#'   }{
#'     F(t) = F_i + f_i h + s_i h^2 / 2
#'   }
#'
#'   **Quantile function**: The inverse of the above, obtained by solving the
#'   quadratic within the interval containing the requested probability.
#'
#'   **Mean**: Computed exactly from the interpolated density,
#'
#'   \deqn{
#'     E(X) = \sum_{i=1}^{n-1} \left[ x_i m_i + w_i^2 \left( \frac{f_i}{6} + \frac{f_{i+1}}{3} \right) \right]
#'   }{
#'     E(X) = sum(x_i m_i + w_i^2 (f_i/6 + f_{i+1}/3))
#'   }
#'
#'   where \eqn{m_i = w_i (f_i + f_{i+1}) / 2} is the probability of the
#'   \eqn{i}th interval.
#'
#'   **Variance**: Computed exactly as \eqn{E(X^2) - E(X)^2}, where
#'
#'   \deqn{
#'     E(X^2) = \sum_{i=1}^{n-1} \left[ x_i^2 m_i + \frac{x_i w_i^2 (f_i + 2 f_{i+1})}{3} + \frac{w_i^3 (f_i + 3 f_{i+1})}{12} \right]
#'   }{
#'     E(X^2) = sum(x_i^2 m_i + x_i w_i^2 (f_i + 2 f_{i+1})/3 + w_i^3 (f_i + 3 f_{i+1})/12)
#'   }
#'
#' @seealso [dist_quantile()], [stats::density()]
#'
#' @examples
#' # A distribution given by its density over a grid
#' at <- seq(-4, 4, by = 0.01)
#' dist <- dist_density(list(at), list(dnorm(at)))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' density(dist, 0)
#' cdf(dist, 1.96)
#' quantile(dist, 0.975)
#'
#' # The density needn't be standardised, it is scaled to integrate to one
#' dist_density(list(c(0, 1)), list(c(2, 2)))
#'
#' # A kernel density estimate
#' kd <- density(rnorm(100))
#' dist_density(list(kd$x), list(kd$y))
#'
#' @export
dist_density <- function(x, density){
  x <- as_list_of(x, .ptype = double())
  density <- as_list_of(density, .ptype = double())
  par <- vec_recycle_common(x = x, density = density)
  tab <- Map(validate_tabulated_density, par$x, par$density)
  new_dist(
    x = as_list_of(lapply(tab, `[[`, "x"), .ptype = double()),
    density = as_list_of(lapply(tab, `[[`, "density"), .ptype = double()),
    class = "dist_density"
  )
}

# Check a tabulated density, sorting it by `x` and standardising it to
# integrate to one. The density is linearly interpolated between the given
# values, so the trapezoidal rule gives its mass exactly.
validate_tabulated_density <- function(x, density) {
  if(length(x) != length(density)) {
    abort("The `x` and `density` values must be the same length.")
  }
  if(length(x) < 2L) {
    abort("At least two values are needed to define a density.")
  }
  if(anyNA(x) || anyNA(density)) {
    abort("The `x` and `density` values must not be missing.")
  }
  if(any(density < 0)) {
    abort("The `density` values must not be negative.")
  }
  if(is.unsorted(x)) {
    i <- order(x)
    x <- x[i]
    density <- density[i]
  }
  if(anyDuplicated(x)) {
    abort("The `x` values must not contain duplicates.")
  }

  n <- length(x)
  total <- sum(diff(x) * (density[-1L] + density[-n]) / 2)
  if(total <= 0) {
    abort("The `density` values must not all be zero.")
  }
  if(abs(total - 1) > 0.01) {
    warn(sprintf(
      "The density integrates to %s, standardising it to integrate to 1.",
      format(total, digits = 3)
    ))
  }

  list(x = x, density = density / total)
}

# Interval widths, probabilities, and the cumulative distribution at the values
# of a tabulated density. The density is linearly interpolated between the
# values, so each interval's probability is exactly the area of a trapezoid.
density_intervals <- function(x) {
  at <- x[["x"]]
  f <- x[["density"]]
  n <- length(at)
  w <- diff(at)
  prob <- w * (f[-1L] + f[-n]) / 2
  list(x = at, f = f, w = w, slope = diff(f) / w, cdf = c(0, cumsum(prob)))
}

#' @export
format.dist_density <- function(x, ...){
  sprintf(
    "density[%s]",
    length(x[["x"]])
  )
}

#' @export
density.dist_density <- function(x, at, ...){
  out <- stats::approx(x[["x"]], x[["density"]], xout = at)$y
  # The density is zero beyond the tabulated values
  out[is.na(out) & !is.na(at)] <- 0
  out
}

#' @export
cdf.dist_density <- function(x, q, ...){
  d <- density_intervals(x)
  n <- length(d$x)

  out <- rep(NA_real_, length(q))
  ok <- !is.na(q)
  q <- q[ok]

  i <- findInterval(q, d$x)
  z <- numeric(length(q))
  z[i >= n] <- 1

  # Integrate the linear density from the start of the interval containing `q`
  inner <- i > 0L & i < n
  if(any(inner)) {
    j <- i[inner]
    h <- q[inner] - d$x[j]
    z[inner] <- d$cdf[j] + d$f[j] * h + d$slope[j] * h^2 / 2
  }

  out[ok] <- pmin(pmax(z, 0), 1)
  out
}

#' @export
quantile.dist_density <- function(x, p, ...){
  d <- density_intervals(x)
  n <- length(d$x)

  out <- rep(NA_real_, length(p))
  ok <- !is.na(p)
  p <- p[ok]

  # Probabilities outside [0,1] have no quantile
  z <- rep(NaN, length(p))
  valid <- p >= 0 & p <= 1
  p <- p[valid]

  # Interval containing each probability. Intervals of zero probability are
  # skipped, as findInterval() matches the last of any tied values.
  j <- pmin(pmax(findInterval(p, d$cdf), 1L), n - 1L)
  target <- p - d$cdf[j]

  # Solve target = f[j]*h + slope[j]*h^2/2 for h, in the form which is stable
  # as the slope approaches zero (where the solution is target/f[j]).
  denom <- d$f[j] + sqrt(pmax(d$f[j]^2 + 2 * d$slope[j] * target, 0))
  h <- ifelse(denom > 0, 2 * target / denom, 0)

  z[valid] <- pmin(d$x[j] + h, d$x[n])
  out[ok] <- z
  out
}

#' @export
generate.dist_density <- function(x, times, ...){
  quantile(x, stats::runif(times), ...)
}

#' @export
mean.dist_density <- function(x, ...){
  d <- density_intervals(x)
  n <- length(d$x)
  f1 <- d$f[-n]
  f2 <- d$f[-1L]
  sum(d$x[-n] * diff(d$cdf) + d$w^2 * (f1 / 6 + f2 / 3))
}

#' @export
covariance.dist_density <- function(x, ...){
  d <- density_intervals(x)
  n <- length(d$x)
  x1 <- d$x[-n]
  f1 <- d$f[-n]
  f2 <- d$f[-1L]
  ex2 <- sum(
    x1^2 * diff(d$cdf) +
      x1 * d$w^2 * (f1 + 2 * f2) / 3 +
      d$w^3 * (f1 + 3 * f2) / 12
  )
  ex2 - mean(x)^2
}

#' @export
support.dist_density <- function(x, ...) {
  f <- x[["density"]]
  new_support_region(
    list(double()),
    list(range(x[["x"]])),
    list(!near(f[c(1L, length(f))], 0))
  )
}
