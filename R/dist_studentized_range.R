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
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   **Support**: \eqn{R^+}, the set of positive real numbers.
#'
#'   Other properties of Tukey's Studentized Range Distribution
#'   are omitted, largely because the distribution is not fun
#'   to work with.
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
