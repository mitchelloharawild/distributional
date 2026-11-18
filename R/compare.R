#' Comparison and logical operators for distributions
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The ordering operators `<`, `<=`, `>` and `>=` give the probability that the
#' comparison is true, as a [dist_bernoulli()] distribution. `==` and `!=`
#' compare the structure of distributions: two distributions are equal when
#' they are from the same family and have identical parameters. The logical
#' operators `&`, `|` and `!` combine Bernoulli distributions (such as the
#' results of comparisons) as independent events.
#'
#' @details
#' **Ordering.** Comparing a distribution with a number gives a Bernoulli
#' distribution whose probability is that of the comparison being true, so
#' `dist_normal() > 1.96` is `Bernoulli(0.025)` and
#' `mean(dist_normal() > 1.96)` is the probability. The number can be on either
#' side. The probabilities are computed with [cdf()], using the upper tail for
#' `>` and `>=` so that small upper tail probabilities keep their precision.
#' The difference between `<` and `<=` (and between `>` and `>=`) is the
#' probability mass at the number, which is 0 for continuous distributions and
#' `density(x, q)` for discrete distributions. The point mass of mixed
#' distributions with a continuous part (such as [dist_inflated()] of a
#' continuous distribution) isn't included, so comparisons exactly at it are
#' approximate.
#'
#' Comparing two distributions, `x > y` is the probability \eqn{P(X - Y > 0)},
#' computed from `x - y`. This assumes that `x` and `y` are independent (as
#' arithmetic between distributions does), and uses a closed form where one is
#' known (such as for two normal distributions) or [dist_convolved()]
#' otherwise.
#'
#' **Logical operators.** `!x` is `Bernoulli(1 - p)`, `x & y` is
#' `Bernoulli(px * py)` and `x | y` is `Bernoulli(px + py - px * py)`. These
#' assume that the events are independent, **even when they are comparisons
#' of the same distribution**. For example, `(x > -1) & (x < 1)` multiplies
#' \eqn{P(X > -1)} and \eqn{P(X < 1)}, which is not \eqn{P(-1 < X < 1)}. Use
#' `cdf(x, 1) - cdf(x, -1)` for the probability of an interval. A logical
#' operand is treated as a certain event: `TRUE` has probability 1, `FALSE`
#' probability 0, and `NA` gives a missing result. Distributions other than
#' Bernoulli (or [dist_degenerate()] at 0 or 1) are an error.
#'
#' **Equality.** `x == y` is `TRUE` when `x` and `y` are the same family with
#' the same parameters, and gives a logical vector. A number on either side is
#' treated as a [dist_degenerate()] point mass, so `dist_degenerate(1) == 1` is
#' `TRUE` but `dist_normal(1) == 1` is `FALSE`. Comparisons with a missing
#' distribution are `NA`. `==` does not give the probability that two random
#' variables are equal. For discrete distributions, the probability
#' \eqn{P(X = k)} is `density(x, k)`.
#'
#' The operands are paired by position, and an operand of size 1 is recycled.
#' Missing distributions or numbers give missing results.
#'
#' @param e1,e2 Distributions, numbers or logicals.
#'
#' @return `<`, `<=`, `>`, `>=`, `&`, `|` and `!` give a vector of Bernoulli
#' distributions.
#' `==` and `!=` give a logical vector.
#'
#' @seealso [cdf()], [density()]
#'
#' @examples
#' # The probability of exceeding a threshold
#' x <- dist_normal()
#' x > 1.96
#' mean(x > 1.96)
#'
#' # Vectors of thresholds or distributions are paired by position
#' dist_poisson(1:3) >= 2
#' 0 < dist_normal(-1:1)
#'
#' # Comparing two independent distributions
#' dist_normal(1) > dist_normal(0)
#'
#' # Logical operators treat events as independent
#' !(x > 1.96)
#' (x > -1) & (x < 1)
#' # which is not the probability of the interval, as the events are dependent
#' cdf(x, 1) - cdf(x, -1)
#' (x > 1.96) | (dist_normal(5) < 3)
#' (x > 0) & TRUE
#'
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
  if (missing(e2)) {
    if (.Generic == "!") return(new_bernoulli(1 - bernoulli_prob(e1, "!")))
    return(NextMethod())
  }
  switch(
    .Generic,
    `==` = dist_equal(e1, e2),
    `!=` = !dist_equal(e1, e2),
    `<` = ,
    `<=` = ,
    `>` = ,
    `>=` = dist_order(e1, e2, .Generic),
    `&` = ,
    `|` = dist_logic(e1, e2, .Generic),
    NextMethod()
  )
}

# `&` and `|` of two independent events, given as Bernoulli distributions or
# logicals, paired by position
dist_logic <- function(e1, e2, op) {
  p <- vctrs::vec_recycle_common(
    e1 = bernoulli_prob(e1, op), e2 = bernoulli_prob(e2, op), .call = NULL
  )
  new_bernoulli(switch(
    op,
    `&` = p$e1 * p$e2,
    `|` = p$e1 + p$e2 - p$e1 * p$e2
  ))
}

# The probability of each event in `x`, an operand of a logical operator `op`:
# a Bernoulli distribution, a degenerate distribution at 0 or 1, or a logical
# (or number) that is 0 or 1. Missing values give `NA`.
bernoulli_prob <- function(x, op) {
  if (!is_distribution(x)) {
    if (!is.logical(x) && !is.numeric(x)) {
      abort(sprintf("Can't use `%s` with <%s>.", op, class(x)[1L]), call = NULL)
    }
    p <- as.double(unname(x))
    if (any(!is.na(p) & p != 0 & p != 1)) {
      abort(sprintf("Can't use `%s` with numbers other than 0 or 1.", op), call = NULL)
    }
    return(p)
  }
  fam <- family(x)
  p <- rep_len(NA_real_, length(x))
  ok <- fam %in% c("bernoulli", "degenerate")
  if (any(ok)) p[ok] <- mean(x[ok])
  # Missing distributions are stored without a family or as `dist_na`
  miss <- is.na(fam) | fam == "na"
  bad <- !miss & !(fam == "bernoulli" | (fam == "degenerate" & p %in% c(0, 1)))
  if (any(bad)) {
    abort(c(
      sprintf("Can't use `%s` with distributions other than Bernoulli.", op),
      x = sprintf("Element %i is %s.", which(bad)[1L], format(x[which(bad)[1L]])),
      i = "Compare distributions with a number first, such as `x > 0`."
    ), call = NULL)
  }
  p
}

# The probability that the ordering comparison `op` between `e1` and `e2` is
# true, as a Bernoulli distribution. One operand may be a number (either
# side), paired with the distributions by position. Two distributions are
# compared through their difference, assuming they are independent.
dist_order <- function(e1, e2, op) {
  if (is_distribution(e1) && is_distribution(e2)) {
    return(dist_order(e1 - e2, 0, op))
  }
  # Write `q op x` as `x op' q`
  if (!is_distribution(e1)) {
    q <- e1
    x <- e2
    op <- switch(op, `<` = ">", `<=` = ">=", `>` = "<", `>=` = "<=")
  } else {
    x <- e1
    q <- e2
  }
  if (!is.numeric(q) && !is.logical(q)) {
    abort(sprintf(
      "Can't compare a distribution with <%s> using `%s`.",
      class(q)[1L], op
    ), call = NULL)
  }
  if (n_variates(x, "q") > 1L) {
    abort(sprintf("Can't compare multivariate distributions using `%s`.", op), call = NULL)
  }
  q <- vctrs::vec_cast(unname(q), double())
  args <- vctrs::vec_recycle_common(x = x, q = q, .call = NULL)
  x <- args$x
  q <- args$q

  # The upper tail is computed directly to keep its precision
  tail <- if (op %in% c(">", ">=")) "upper" else "lower"
  p <- dist_paired(cdf, x, q = q, tail = tail, .ptype = double())
  if (op == "<") p <- p - point_mass(x, q)
  if (op == ">=") p <- p + point_mass(x, q)
  new_bernoulli(p)
}

# The probability mass P(X = at) of each distribution in `x` at `at` (paired
# by position). It is 0 for continuous distributions, and `density()` for
# distributions with integer (or logical) support and for point masses.
# Sample distributions give the proportion of draws equal to `at`, and
# transformed distributions the mass of the base distribution at the inverse
# of `at` (the `density()` of a transformed discrete distribution includes a
# Jacobian, so it isn't the probability mass).
#
# Mixed distributions are judged by their support alone. A continuous
# distribution with a point mass (such as `dist_inflated()` of a continuous
# distribution) gives 0, so comparisons at the point mass are approximate.
point_mass <- function(x, at) {
  dist_paired(function(x, at) {
    if (inherits(x, "dist_degenerate")) return(density(x, at))
    if (inherits(x, "dist_sample")) {
      return(as.double(unlist(.mapply(function(s, a) mean(s == a, na.rm = TRUE), list(x[["x"]], at), NULL))))
    }
    if (inherits(x, "dist_transformed")) {
      n <- vctrs::vec_size(x)
      return(transformed_by_group(x, function(dist, tf, inv, idx) {
        at <- transformed_arg(at, idx, n)
        # A transformation without an inverse has an unknown mass
        base_at <- tryCatch(suppressWarnings(inv(at)), error = function(e) rep_len(NA_real_, length(at)))
        point_mass(dist, base_at)
      }))
    }
    out <- numeric(vctrs::vec_size(x))
    discrete <- vapply(
      vctrs::field(support(x), "x"),
      function(proto) is.integer(proto) || is.logical(proto),
      logical(1L)
    )
    # The mass is only at integers (avoiding warnings from `density()`)
    discrete <- discrete & is.finite(at) & at == trunc(at)
    if (any(discrete)) out[discrete] <- density(x[discrete], at[discrete])
    out
  }, x, at = at, .ptype = double())
}

# Bernoulli distributions with probabilities `p`, which are missing where `p`
# is missing. Rounding error (such as from subtracting a point mass) is kept
# within [0, 1].
new_bernoulli <- function(p) {
  p <- pmin(pmax(as.double(p), 0), 1)
  miss <- is.na(p)
  out <- dist_bernoulli(replace(p, miss, 0))
  if (any(miss)) out[miss] <- NA
  out
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
