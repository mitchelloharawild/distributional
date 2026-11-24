#' Apply a statistic/probability-function generic across a distribution vector
#'
#' @description
#' `.f` is called once per storage slot (or group of slots) by
#' [vecvec::vecvec_mapply()], so every family's methods must be vectorised:
#' `.f(x, ...)` receives the `n` distributions of one family and returns a
#' result of size `n`. See `_dev/dist-port.md` for the full contract.
#'
#' `apply_dist()` passes every argument in `...` unchanged to every call.
#'
#' `.ptype` is the type of the result for each distribution, which is used when
#' there are no distributions to compute (`x` is empty or all missing), so that
#' the result still has the type the generic usually returns.
#'
#' `broadcast_dist()` combines its first argument in `...` (such as `at`, `q`,
#' `p` or `size`, normalised by `broadcast_arg()`) with the distributions using
#' the rules of `vignette("broadcasting")`. Other arguments are passed
#' unchanged.
#'
#' * Scalars are broadcast: the size 1 argument is passed to every call.
#' * Vectors are mapped: the distributions are repeated once per point and the
#'   points once per distribution, so every point is paired with every
#'   distribution, and the results are split into one element per
#'   distribution.
#' * Lists are recycled: each element is paired with the distributions by
#'   position, giving one data frame column.
#'
#' In every case the family methods only receive the argument paired with the
#' distributions by position: size `n` or 1 (for multivariate distributions, a
#' matrix with one row per distribution).
#'
#' @noRd
apply_dist <- function(x, .f, ..., .ptype){
  set_variate_names(apply_slots(x, .f, list2(...), .ptype = .ptype), variates(x))
}

#' @rdname apply_dist
#' @noRd
broadcast_dist <- function(x, .f, ..., .ptype){
  args <- list2(...)
  arg <- args[[1L]]
  dn <- variates(x)

  # Lists are recycled
  if (is_list_of(arg)) {
    validate_recycling(x, arg)
    out <- lapply(arg, function(a) {
      args[[1L]] <- a
      set_variate_names(apply_slots(x, .f, args, 1L, .ptype), dn)
    })
    return(new_data_frame(out, n = length(x)))
  }

  # Vectors are mapped
  if (vec_size(arg) > 1L) {
    n <- length(x)
    if (n == 0L) return(list())
    m <- vec_size(arg)
    if (length(unique(slot_dims(x))) > 1L) {
      # Results of different dimension are listed, one element per distribution
      out <- lapply(seq_len(n), function(i) {
        apply_slots(x[rep(i, m)], .f, args, 1L, .ptype)
      })
      return(set_variate_names(out, dn))
    }
    args[[1L]] <- vec_rep(arg, n)
    out <- apply_slots(x[rep(seq_len(n), each = m)], .f, args, 1L, .ptype)
    return(set_variate_names(vec_chop(out, sizes = rep(m, n)), dn))
  }

  # Scalars are broadcast
  apply_dist(x, .f, !!!args, .ptype = .ptype)
}

# Evaluate `.f` for every distribution in `x`, once per storage slot. With
# `pos`, `args[[pos]]` is paired with the distributions by position (and
# recycled if it has size 1). Other arguments are passed unchanged. When there
# is nothing to compute (`x` is empty or all missing), the result is
# `vec_init(.ptype, length(x))`.
apply_slots <- function(x, .f, args, pos = NULL, .ptype = NULL) {
  # Fast path: a vector stored in a single slot (the common case) has nothing
  # to group or align, so slice the slot and call `.f` on it directly. This
  # avoids building and unpacking an intermediate vecvec, which dominates the
  # cost of calls on short vectors.
  if (length(x@x) == 1L &&
      (is.null(pos) || vec_size(args[[pos]]) %in% c(1L, vec_size(x)))) {
    idx <- as.vector(vecvec:::vv_index(x))
    if (!anyNA(idx)) {
      slot <- x@x[[1L]]
      if (!identical(idx, seq_len(vec_size(slot)))) slot <- vec_slice(slot, idx)
      return(inject(.f(slot, !!!args)))
    }
  }
  .l <- if (is.null(pos)) list(x) else list(x, args[[pos]])
  out <- vecvec::vecvec_mapply(.l, function(vals, a) {
    if (!missing(a)) args[[pos]] <- a
    inject(.f(vals, !!!args))
  })
  if (!length(out@x) && !is.null(.ptype)) return(vec_init(.ptype, length(x)))
  # Results with different numbers of columns can't share one matrix (the
  # narrower ones would be padded), so each distribution is evaluated
  # separately and the results are listed.
  widths <- vapply(out@x, function(s) {
    if (is.matrix(s) || is.data.frame(s)) ncol(s) else 1L
  }, integer(1L))
  if (length(unique(widths)) > 1L) {
    n <- vec_size(x)
    return(lapply(seq_len(n), function(i) {
      a <- args
      if (!is.null(pos) && vec_size(a[[pos]]) == n) a[[pos]] <- vec_slice(a[[pos]], i)
      apply_slots(x[i], .f, a, pos, .ptype)
    }))
  }
  vecvec::unvecvec(out)
}

# Call the generic `.f` on the `distribution` vector `x` with its first
# argument in `...` (for example `q = q`) paired with the distributions by
# position, as the family methods receive their own broadcast argument. Other
# arguments in `...` are passed unchanged. The argument has size `vec_size(x)`
# or 1. Unlike `cdf(x, q)`, which would map a size-`n` `q` over all `n`
# distributions, this gives one result for each distribution. `.ptype` is the
# result type used when there are no distributions to compute (see
# `apply_slots()`).
#
# Nested families (truncated, inflated, ...) store their inner distributions as
# a `distribution` vector field, so `dist_paired(cdf, x[["dist"]], q = q)` calls
# the inner family methods once per family with all of their distributions.
dist_paired <- function(.f, x, ..., .ptype = NULL) {
  args <- list2(...)
  apply_slots(x, .f, args, 1L, .ptype)
}

# Name the columns of results with one column per variate (a matrix of points
# or moments, or a data frame of intervals) after the variates of `x`. A list of
# results (one per distribution) is named element by element. Results whose
# columns are not the variates are left as they are.
set_variate_names <- function(x, dn) {
  if (is.null(dn)) return(x)
  if (is.matrix(x) || is.data.frame(x)) {
    if (ncol(x) == length(dn)) colnames(x) <- dn
    return(x)
  }
  if (vec_is_list(x)) x[] <- lapply(x, set_variate_names, dn = dn)
  x
}

# Normalise an argument for `broadcast_dist()`: lists become named `list_of`s
# (recycled over the distributions), and vectors are cast to `.ptype`.
# Matrices (the points of multivariate distributions) are left as they are.
broadcast_arg <- function(x, .ptype = NULL, arg = "x") {
  if(is.list(x)) {
    x <- as_list_of(as.list(x), .ptype = .ptype)
    if(is.null(names(x))) {
      names(x) <- vec_as_names(character(vec_size(x)), repair = "unique", quiet = TRUE)
    }
    return(x)
  }
  if(is.null(.ptype) || is.matrix(x)) return(x)
  vec_cast(x, .ptype, x_arg = arg)
}

# Values of a multivariate distribution's variates (`at`, `q`) must be given
# as a matrix with one column for each variate, or a list of such matrices
# (recycled by broadcast_dist()). A bare vector would be ambiguous about
# whether it describes one point or several.
validate_arg_dim <- function(x, value, arg) {
  d <- n_variates(x, arg)
  # Vectors are unambiguous for univariate distributions (they are many points)
  if(d == 1L) return(value)

  pts <- if(is.list(value)) value else list(value)
  is_points <- vapply(pts, function(p) {
    is_empty(p) || (is.matrix(p) && ncol(p) == d)
  }, logical(1L))
  if(!all(is_points)) {
    abort(sprintf(
      "`%s` must be a matrix with one column for each of the %i variates (such as `%s = cbind(...)`).",
      arg, d, arg
    ))
  }
  value
}

# The number of variates of the distributions in `x` (1 if there are none).
# Points can only describe distributions with the same number of variates, so
# a vector mixing numbers of variates is an error. Missing distributions have
# no variates, so they are ignored.
# The number of variates of each storage slot (missing distributions ignored).
# Parametric families that are always univariate. Their dimension is known
# without drawing a random value (the default `dim()` method generates one).
univariate_dist_classes <- c(
  "dist_bernoulli", "dist_beta", "dist_binomial", "dist_burr", "dist_cauchy",
  "dist_chisq", "dist_degenerate", "dist_exponential", "dist_f", "dist_gamma",
  "dist_geometric", "dist_gev", "dist_gh", "dist_gk", "dist_gpd",
  "dist_gumbel", "dist_horseshoe", "dist_hypergeometric",
  "dist_inverse_exponential", "dist_inverse_gamma", "dist_inverse_gaussian",
  "dist_laplace", "dist_logarithmic", "dist_logistic", "dist_lognormal",
  "dist_negative_binomial", "dist_normal", "dist_pareto", "dist_poisson",
  "dist_poisson_inverse_gaussian", "dist_student_t", "dist_studentized_range",
  "dist_tweedie", "dist_uniform", "dist_weibull"
)

slot_dims <- function(x) {
  slots <- Filter(function(s) !inherits(s, "dist_na"), x@x)
  vapply(slots, function(s) {
    if (inherits(s, univariate_dist_classes)) return(1L)
    # only the first element is needed (and slicing a length 1 slot is wasteful)
    if (vec_size(s) > 1L) s <- vec_slice(s, 1L)
    as.integer(dim(s) %||% 1L)
  }, integer(1L))
}

n_variates <- function(x, arg) {
  # dim() methods for distributions return a single number
  d <- unique(slot_dims(x))
  if(length(d) > 1L) {
    abort(sprintf(
      "Can't use `%s` with distributions that have different numbers of variates (%s).",
      arg, paste(sort(d), collapse = ", ")
    ))
  }
  if(length(d)) d else 1L
}

validate_recycling <- function(x, arg) {
  sizes <- if (vec_is_list(arg)) list_sizes(arg) else vec_size(arg)
  bad <- !sizes %in% c(1L, vec_size(x))
  if(any(bad)) {
    abort(
      sprintf("Cannot recycle input of size %i to match the distributions (size %i).",
              sizes[bad][1L], vec_size(x)
      )
    )
  }
}
