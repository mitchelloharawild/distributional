#' The Studentized Range distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Tukey's studentized range distribution, used for Tukey's
#' honestly significant differences test in ANOVA.
#'
#' @inheritParams stats::qtukey
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_studentized_range")`
#'
#'   In the following, let \eqn{Q} be a Studentized Range random variable with
#'   parameters `nmeans` = \eqn{k} (number of groups), `df` = \eqn{\nu} (degrees
#'   of freedom), and `nranges` = \eqn{n} (number of ranges).
#'
#'   **Support**: \eqn{R^+}, the set of positive real numbers.
#'
#'   **Mean**: Approximated numerically.
#'
#'   **Variance**: Approximated numerically.
#'
#'   **Probability density function (p.d.f)**: The density does not have a
#'   closed-form expression and is computed numerically.
#'
#'   **Cumulative distribution function (c.d.f)**: The c.d.f does not have a
#'   simple closed-form expression. For \eqn{n = 1} (single range), it involves
#'   integration over the joint distribution of the sample range and an
#'   independent chi-square variable. The general form is computed numerically
#'   using algorithms described in the references for [stats::ptukey()].
#'
#'   **Moment generating function (m.g.f)**: Does not exist in closed form.
#'
#' @seealso [stats::Tukey]
#'
#' @examples
#' dist <- dist_studentized_range(nmeans = c(6, 2), df = c(5, 4), nranges = c(1, 1))
#'
#' dist
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @name dist_studentized_range
#' @export
dist_studentized_range <- function(nmeans, df, nranges){
  nmeans <- vec_cast(nmeans, double())
  df <- vec_cast(df, double())
  new_dist(nm = nmeans, df = df, nr = nranges, class = "dist_studentized_range")
}

#' @export
format.dist_studentized_range <- function(x, digits = 2, ...){
  sprintf(
    "StudentizedRange(%s, %s, %s)",
    format(x[["nm"]], digits = digits, ...),
    format(x[["df"]], digits = digits, ...),
    format(x[["nr"]], digits = digits, ...)
  )
}

#' @export
quantile.dist_studentized_range <- function(x, p, ...){
  stats::qtukey(p, x[["nm"]], x[["df"]], x[["nr"]])
}

#' @export
cdf.dist_studentized_range <- function(x, q, ...){
  stats::ptukey(q, x[["nm"]], x[["df"]], x[["nr"]])
}
