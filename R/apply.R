#' Apply a statistic/probability-function generic across a distribution vector
#'
#' @description
#' Implements the broadcasting rules of `vignette("broadcasting")` for the
#' argument marked with `arg_listable()` (at most one); all other arguments are
#' passed unchanged to every call.
#'
#' * Scalars are broadcast: the size 1 argument is passed to every call.
#' * Vectors are mapped: the distributions are repeated once per point and the
#'   points once per distribution, so every point is paired with every
#'   distribution, and the results are split into one element per
#'   distribution.
#' * Lists are recycled: each element is paired with the distributions by
#'   position, giving one data frame column.
#'
#' `.f` is called once per storage slot (or group of slots) by
#' [vecvec::vecvec_mapply()], so every family's methods must be vectorised:
#' `.f(x, arg)` receives the `n` distributions of one family and an `arg` of
#' size `n` or 1 (for multivariate distributions, a matrix with one row per
#' distribution), paired by position, and returns a result of size `n`. See
#' `_dev/dist-port.md` for the families still to be ported to this contract.
#'
#' @noRd
apply_dist <- function(x, .f, ...){
  dn <- variates(x)
  args <- dots_list(...)
  is_arg_listable <- vapply(args, inherits, FUN.VALUE = logical(1L), "arg_listable")
  if (sum(is_arg_listable) > 1) abort("Only one distribution argument can be unpacked at a time.\nThis shouldn't happen, please report a bug at https://github.com/mitchelloharawild/distributional/issues/")

  if (!any(is_arg_listable)) {
    out <- apply_slots(x, .f, args)
    return(if (vec_is_list(out)) lapply(out, set_matrix_dimnames, dn = dn) else set_matrix_dimnames(out, dn))
  }

  arg_pos <- which(is_arg_listable)
  arg <- args[[arg_pos]]
  oldClass(arg) <- setdiff(oldClass(arg), "arg_listable")

  # Lists are recycled
  if (is_list_of(arg)) {
    validate_recycling(x, arg)
    out <- lapply(arg, function(a) set_matrix_dimnames(apply_slots(x, .f, args, arg_pos, a), dn))
    return(new_data_frame(out, n = length(x)))
  }

  # Vectors are mapped
  if (vec_size(arg) > 1L) {
    n <- length(x)
    if (n == 0L) return(list())
    m <- vec_size(arg)
    out <- apply_slots(x[rep(seq_len(n), each = m)], .f, args, arg_pos, vec_rep(arg, n))
    return(lapply(vec_chop(out, sizes = rep(m, n)), set_matrix_dimnames, dn = dn))
  }

  # Scalars are broadcast
  args[[arg_pos]] <- arg
  out <- apply_slots(x, .f, args)
  if (vec_is_list(out)) lapply(out, set_matrix_dimnames, dn = dn) else set_matrix_dimnames(out, dn)
}

# Evaluate `.f` for every distribution in `x`, once per storage slot. With
# `pos`, `args[[pos]]` is taken from `a`, which is paired with the
# distributions by position (and recycled if it has size 1).
apply_slots <- function(x, .f, args, pos = NULL, a = NULL) {
  if (is.null(pos)) {
    return(vecvec::vecvec_mapply(
      list(x), function(vals) inject(.f(vals, !!!args)), SIMPLIFY = TRUE
    ))
  }
  vecvec::vecvec_mapply(
    list(x, a),
    function(vals, a) {
      args[[pos]] <- a
      inject(.f(vals, !!!args))
    },
    SIMPLIFY = TRUE
  )
}

set_matrix_dimnames <- function(x, dn) {
  if((is.matrix(x) || is.data.frame(x)) && !is.null(dn)){
    # Set dimension names
    colnames(x) <- dn
  }
  x
}

# Declare a function's argument as allowing list inputs for mapping values
arg_listable <- function(x, .ptype) {
  if(is.list(x)) {
    x <- as_list_of(as.list(x), .ptype)
    if(is.null(names(x))) {
      names(x) <- vec_as_names(character(vec_size(x)), repair = "unique")
    }
  } else if(!is.matrix(x)) {
    vec_assert(x, .ptype)
  }
  # Declares list arguments to be unpacked for apply_dist()
  class(x) <- c("arg_listable", oldClass(x))
  x
}

# Values of a multivariate distribution's variates (`at`, `q`) should be given
# as a matrix with one column for each variate. A bare vector is ambiguous about
# whether it describes one point or several, so it is deprecated and for now
# interpreted as a matrix with one column for each variate.
validate_arg_dim <- function(x, value, arg) {
  # Matrices already describe the points, lists are unpacked by apply_dist()
  if(is.matrix(value) || is.list(value) || is_empty(value)) return(value)

  # Number of variates (dim() methods for distributions return a single number)
  d <- if (length(x@x)) (dim(x@x[[1L]][1]) %||% 1L) else 1L
  # Vectors are unambiguous for univariate distributions (they are many points)
  if(d == 1L) return(value)

  lifecycle::deprecate_soft(
    when = "0.9.0",
    what = I(sprintf(
      "Using a vector for the `%s` argument of a multivariate distribution", arg
    )),
    with = I(sprintf(
      "a matrix with one column for each of the %i variates (such as `%s = cbind(...)`)",
      d, arg
    )),
    # The user's frame is 2 calls up: <generic>() then <generic>.distribution()
    user_env = caller_env(2)
  )

  if(length(value) != 1L && length(value) %% d != 0L) {
    abort(sprintf(
      "Cannot use an input of length %i as points of a distribution with %i variates.",
      length(value), d
    ))
  }
  matrix(value, ncol = d)
}

validate_recycling <- function(x, arg) {
  if(is_list_of(arg)) return(lapply(arg, validate_recycling, x = x))
  if(!any(vec_size(arg) == c(1, vec_size(x)))) {
    abort(
      sprintf("Cannot recycle input of size %i to match the distributions (size %i).",
              vec_size(arg), vec_size(x)
      )
    )
  }
}
