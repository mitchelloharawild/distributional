#' Comparison and logical operators for distributions
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' `==` and `!=` compare the structure of distributions: two distributions are
#' equal when they are from the same family and have identical parameters.
#'
#' @details
#' **Equality.** `x == y` is `TRUE` when `x` and `y` are the same family with
#' the same parameters, and gives a logical vector. A number on either side is
#' treated as a [dist_degenerate()] point mass, so `dist_degenerate(1) == 1` is
#' `TRUE` but `dist_normal(1) == 1` is `FALSE`. Comparisons with a missing
#' distribution are `NA`. `==` does not give the probability that two random
#' variables are equal. For discrete distributions, the probability
#' \eqn{P(X = k)} is `density(x, k)`.
#'
#' The operands are paired by position, and an operand of size 1 is recycled.
#'
#' @param e1,e2 Distributions, numbers or logicals.
#'
#' @return `==` and `!=` give a logical vector.
#'
#' @examples
#' # Equality compares the family and parameters
#' dist_normal(1:3) == dist_normal(1)
#' dist_degenerate(1) == 1
#' dist_normal(1) == 1
#'
#' # The probability of a point is density()
#' density(dist_poisson(3), 2)
#'
#' @aliases distribution-comparison
#' @export
Ops.distribution <- function(e1, e2) {
  switch(
    .Generic,
    `==` = dist_equal(e1, e2),
    `!=` = !dist_equal(e1, e2),
    NextMethod()
  )
}

# Structural equality of the distributions in `e1` and `e2`, paired by
# position: the same family (class and record attributes) and equal parameters.
# Numbers are cast to `dist_degenerate()`. Missing distributions give `NA`.
dist_equal <- function(e1, e2) {
  cast <- function(e, arg) {
    if (is_distribution(e)) e else vctrs::vec_cast(e, new_dist(), x_arg = arg, call = NULL)
  }
  args <- vctrs::vec_recycle_common(e1 = cast(e1, "e1"), e2 = cast(e2, "e2"), .call = NULL)
  out <- vecvec::vecvec_mapply(args, function(a, b) {
    n <- vctrs::vec_size(a)
    if (inherits(a, "dist_na") || inherits(b, "dist_na")) return(rep_len(NA, n))
    if (!same_record_type(a, b)) return(rep_len(FALSE, n))
    record_equal(a, b)
  })
  if (!length(out@x)) return(rep_len(NA, length(args[[1L]])))
  vecvec::unvecvec(out)
}

# Compare two records of the same family field by field. Fields holding
# distributions (as nested families do) are compared with `dist_equal()`, since
# `identical()` doesn't compare the storage of `distribution` vectors. Other
# list fields (such as the closures of `dist_transformed()`) are compared
# element by element, ignoring the environments of closures, which are
# different for every call to an operator even when the functions are equal.
record_equal <- function(a, b) {
  out <- rep_len(TRUE, vctrs::vec_size(a))
  for (f in vctrs::fields(a)) {
    fa <- vctrs::field(a, f)
    fb <- vctrs::field(b, f)
    eq <- if (is_distribution(fa)) {
      dist_equal(fa, fb)
    } else if (vctrs::vec_is_list(fa)) {
      as.logical(unlist(.mapply(object_equal, list(fa, fb), NULL)))
    } else {
      vctrs::vec_equal(fa, fb)
    }
    out <- out & eq
  }
  out
}

object_equal <- function(u, v) {
  if (is_distribution(u) || is_distribution(v)) {
    return(is_distribution(u) && is_distribution(v) && length(u) == length(v) && all(dist_equal(u, v)))
  }
  identical(u, v, ignore.environment = TRUE)
}

# Whether two family records hold the same family: the same class, field
# names and other attributes (such as the variate names).
same_record_type <- function(a, b) {
  attrs <- function(x) {
    a <- attributes(x)
    a[sort(setdiff(names(a), "names"))]
  }
  identical(attrs(a), attrs(b)) && identical(vctrs::fields(a), vctrs::fields(b))
}
