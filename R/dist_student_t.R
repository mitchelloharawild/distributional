#' The (non-central) location-scale Student t Distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The Student's T distribution is closely related to the [Normal()]
#' distribution, but has heavier tails. As \eqn{\nu} increases to \eqn{\infty},
#' the Student's T converges to a Normal. The T distribution appears
#' repeatedly throughout classic frequentist hypothesis testing when
#' comparing group means.
#'
#' @inheritParams stats::dt
#' @param mu The location parameter of the distribution.
#'   If `ncp == 0` (or `NULL`), this is the median.
#' @param sigma The scale parameter of the distribution.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_student_t")`
#'
#'   In the following, let \eqn{X} be a location-scale Student's T random variable with
#'   `df` = \eqn{\nu}, `mu` = \eqn{\mu}, `sigma` = \eqn{\sigma}, and
#'   `ncp` = \eqn{\delta} (non-centrality parameter).
#'
#'   If \eqn{Z} follows a standard Student's T distribution (with `df` = \eqn{\nu}
#'   and `ncp` = \eqn{\delta}), then \eqn{X = \mu + \sigma Z}.
#'
#'
#'   **Support**: \eqn{R}, the set of all real numbers
#'
#'   **Mean**:
#'
#'   For the central distribution (`ncp` = 0 or `NULL`):
#'
#'   \deqn{
#'     E(X) = \mu
#'   }{
#'     E(X) = \mu
#'   }
#'
#'   for \eqn{\nu > 1}, and undefined otherwise.
#'
#'   For the non-central distribution (`ncp` \eqn{\neq} 0):
#'
#'   \deqn{
#'     E(X) = \mu + \delta \sqrt{\frac{\nu}{2}} \frac{\Gamma((\nu-1)/2)}{\Gamma(\nu/2)} \sigma
#'   }{
#'     E(X) = \mu + \delta \sqrt(\nu / 2) \Gamma((\nu-1)/2) / \Gamma(\nu/2) \sigma
#'   }
#'
#'   for \eqn{\nu > 1}, and undefined otherwise.
#'
#'   **Variance**:
#'
#'   For the central distribution (`ncp` = 0 or `NULL`):
#'
#'   \deqn{
#'     \mathrm{Var}(X) = \frac{\nu}{\nu - 2} \sigma^2
#'   }{
#'     Var(X) = \nu / (\nu - 2) \sigma^2
#'   }
#'
#'   for \eqn{\nu > 2}. Undefined if \eqn{\nu \le 1}, infinite when \eqn{1 < \nu \le 2}.
#'
#'   For the non-central distribution (`ncp` \eqn{\neq} 0):
#'
#'   \deqn{
#'     \mathrm{Var}(X) = \left[\frac{\nu(1+\delta^2)}{\nu-2} - \left(\delta \sqrt{\frac{\nu}{2}} \frac{\Gamma((\nu-1)/2)}{\Gamma(\nu/2)}\right)^2\right] \sigma^2
#'   }{
#'     Var(X) = [(\nu(1+\delta^2))/(\nu-2) - (\delta \sqrt(\nu / 2) \Gamma((\nu-1)/2) / \Gamma(\nu/2))^2] \sigma^2
#'   }
#'
#'   for \eqn{\nu > 2}. Undefined if \eqn{\nu \le 1}, infinite when \eqn{1 < \nu \le 2}.
#'
#'   **Probability density function (p.d.f)**:
#'
#'   For the central distribution (`ncp` = 0 or `NULL`), the standard
#'   t distribution with `df` = \eqn{\nu} has density:
#'
#'   \deqn{
#'     f_Z(z) = \frac{\Gamma((\nu + 1)/2)}{\sqrt{\pi \nu} \Gamma(\nu/2)} \left(1 + \frac{z^2}{\nu} \right)^{- (\nu + 1)/2}
#'   }{
#'     f_Z(z) = \Gamma((\nu + 1) / 2) / (\sqrt(\pi \nu) \Gamma(\nu / 2)) (1 + z^2 / \nu)^(- (\nu + 1) / 2)
#'   }
#'
#'   The location-scale version with `mu` = \eqn{\mu} and `sigma` = \eqn{\sigma}
#'   has density:
#'
#'   \deqn{
#'     f(x) = \frac{1}{\sigma} f_Z\left(\frac{x - \mu}{\sigma}\right)
#'   }{
#'     f(x) = (1/\sigma) f_Z((x - \mu) / \sigma)
#'   }
#'
#'   For the non-central distribution (`ncp` \eqn{\neq} 0), the density is
#'   computed numerically via [stats::dt()].
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   For the central distribution (`ncp` = 0 or `NULL`), the cumulative
#'   distribution function is computed numerically via [stats::pt()], which
#'   uses the relationship to the incomplete beta function:
#'
#'   \deqn{
#'     F_\nu(t) = \frac{1}{2} I_x\left(\frac{\nu}{2}, \frac{1}{2}\right)
#'   }{
#'     F_\nu(t) = (1/2) I_x(\nu/2, 1/2)
#'   }
#'
#'   for \eqn{t \le 0}, where \eqn{x = \nu/(\nu + t^2)} and \eqn{I_x(a,b)} is
#'   the incomplete beta function ([stats::pbeta()]). For \eqn{t \ge 0}:
#'
#'   \deqn{
#'     F_\nu(t) = 1 - \frac{1}{2} I_x\left(\frac{\nu}{2}, \frac{1}{2}\right)
#'   }{
#'     F_\nu(t) = 1 - (1/2) I_x(\nu/2, 1/2)
#'   }
#'
#'   The location-scale version is: \eqn{F(x) = F_\nu((x - \mu)/\sigma)}.
#'
#'   For the non-central distribution (`ncp` \eqn{\neq} 0), the cumulative
#'   distribution function is computed numerically via [stats::pt()].
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   Does not exist in closed form. Moments are computed using the formulas
#'   for mean and variance above where available.
#'
#' @seealso [stats::TDist]
#'
#' @examples
#' dist <- dist_student_t(df = c(1,2,5), mu = c(0,1,2), sigma = c(1,2,3))
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
#' @name dist_student_t
#' @export
dist_student_t <- function(df, mu = 0, sigma = 1, ncp = NULL){
  df <- vec_cast(df, numeric())
  if(any(df <= 0)){
    abort("The degrees of freedom parameter of a Student t distribution must be strictly positive.")
  }
  mu <- vec_cast(mu, double())
  sigma <- vec_cast(sigma, double())
  if(any(sigma[!is.na(sigma)] <= 0)){
    abort("The scale (sigma) parameter of a Student t distribution must be strictly positive.")
  }
  # `NA` means the non-centrality parameter is absent (a central t)
  ncp <- if(is.null(ncp)) NA_real_ else vec_cast(ncp, double())
  new_dist(df = df, mu = mu, sigma = sigma, ncp = ncp, class = "dist_student_t")
}

# Apply a stats t function (dt, pt, qt) to paired arguments, using the
# non-central form only for the elements with a non-centrality parameter
student_t_stat <- function(f, q, df, ncp, ...){
  n <- vctrs::vec_size(df)
  q <- vctrs::vec_recycle(q, n)
  has_ncp <- !is.na(ncp)
  out <- numeric(n)
  if(any(!has_ncp)) out[!has_ncp] <- f(q[!has_ncp], df[!has_ncp], ...)
  if(any(has_ncp)) out[has_ncp] <- f(q[has_ncp], df[has_ncp], ncp[has_ncp], ...)
  out
}

#' @export
format.dist_student_t <- function(x, digits = 2, ...){
  ncp <- x[["ncp"]]
  sprintf(
    "t(%s, %s, %s%s)",
    format(x[["df"]], digits = digits, ...),
    format(x[["mu"]], digits = digits, ...),
    format(x[["sigma"]], digits = digits, ...),
    ifelse(is.na(ncp), "", paste(",", format(ncp, digits = digits, trim = TRUE, ...)))
  )
}

#' @export
density.dist_student_t <- function(x, at, ...){
  sigma <- x[["sigma"]]
  at <- vctrs::vec_recycle(at, vctrs::vec_size(x))

  student_t_stat(stats::dt, (at - x[["mu"]])/sigma, x[["df"]], x[["ncp"]]) / sigma
}

#' @export
log_density.dist_student_t <- function(x, at, ...){
  sigma <- x[["sigma"]]
  at <- vctrs::vec_recycle(at, vctrs::vec_size(x))

  student_t_stat(stats::dt, (at - x[["mu"]])/sigma, x[["df"]], x[["ncp"]], log = TRUE) - log(sigma)
}


#' @export
quantile.dist_student_t <- function(x, p, ..., tail = "lower"){
  p <- vctrs::vec_recycle(p, vctrs::vec_size(x))
  student_t_stat(stats::qt, p, x[["df"]], x[["ncp"]], lower.tail = tail == "lower") * x[["sigma"]] + x[["mu"]]
}

#' @export
cdf.dist_student_t <- function(x, q, ..., tail = "lower"){
  q <- vctrs::vec_recycle(q, vctrs::vec_size(x))
  student_t_stat(stats::pt, (q - x[["mu"]])/x[["sigma"]], x[["df"]], x[["ncp"]], lower.tail = tail == "lower")
}

#' @export
generate.dist_student_t <- function(x, times, ...){
  df <- rep(x[["df"]], times)
  ncp <- rep(x[["ncp"]], times)
  has_ncp <- !is.na(ncp)
  z <- numeric(length(df))
  if(any(!has_ncp)) z[!has_ncp] <- stats::rt(sum(!has_ncp), df[!has_ncp])
  if(any(has_ncp)){
    # stats::rt() only accepts a single ncp, so draw the non-central t directly
    m <- sum(has_ncp)
    z[has_ncp] <- stats::rnorm(m, ncp[has_ncp]) / sqrt(stats::rchisq(m, df[has_ncp]) / df[has_ncp])
  }
  vctrs::vec_chop(z * rep(x[["sigma"]], times) + rep(x[["mu"]], times), sizes = times)
}

#' @export
point_mass.dist_student_t <- point_mass_continuous

#' @export
support.dist_student_t <- function(x, ...) {
  n <- vctrs::vec_size(x)
  new_support_region(
    rep_len(list(numeric()), n),
    rep_len(list(c(-Inf, Inf)), n),
    rep_len(list(c(FALSE, FALSE)), n)
  )
}

# E(T) / (sigma * ncp) for the non-central standard t, with df > 1 assumed
student_t_ncp_mean <- function(df){
  sqrt(df/2) * (gamma((df-1)/2)/gamma(df/2))
}

#' @export
mean.dist_student_t <- function(x, ...){
  df <- x[["df"]]
  ncp <- x[["ncp"]]
  ok <- df > 1
  # Evaluate the formula only where it is defined (df > 1)
  df_ok <- ifelse(ok, df, 2)
  shift <- ifelse(is.na(ncp), 0, ncp) * student_t_ncp_mean(df_ok) * x[["sigma"]]
  ifelse(ok, x[["mu"]] + shift, NA_real_)
}

#' @export
covariance.dist_student_t <- function(x, ...){
  df <- x[["df"]]
  ncp <- ifelse(is.na(x[["ncp"]]), 0, x[["ncp"]])
  # Evaluate the formula only where it is defined (df > 2)
  df_ok <- ifelse(df > 2, df, 3)
  out <- ((df_ok*(1+ncp^2))/(df_ok-2) - (ncp * student_t_ncp_mean(df_ok))^2) * x[["sigma"]]^2
  ifelse(df <= 1, NA_real_, ifelse(df <= 2, Inf, out))
}

#' @export
has_symmetry.dist_student_t <- function(x, ...) TRUE
