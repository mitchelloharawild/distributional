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
apply_dist <- function(x, .f, ...){
  out <- apply_slots(x, .f, list2(...))
  dn <- variates(x)
  if (vec_is_list(out)) lapply(out, set_matrix_dimnames, dn = dn) else set_matrix_dimnames(out, dn)
}

#' @rdname apply_dist
#' @noRd
broadcast_dist <- function(x, .f, ...){
  args <- list2(...)
  arg <- args[[1L]]
  dn <- variates(x)

  # Lists are recycled
  if (is_list_of(arg)) {
    validate_recycling(x, arg)
    out <- lapply(arg, function(a) {
      args[[1L]] <- a
      set_matrix_dimnames(apply_slots(x, .f, args, 1L), dn)
    })
    return(new_data_frame(out, n = length(x)))
  }

  # Vectors are mapped
  if (vec_size(arg) > 1L) {
    n <- length(x)
    if (n == 0L) return(list())
    m <- vec_size(arg)
    args[[1L]] <- vec_rep(arg, n)
    out <- apply_slots(x[rep(seq_len(n), each = m)], .f, args, 1L)
    return(lapply(vec_chop(out, sizes = rep(m, n)), set_matrix_dimnames, dn = dn))
  }

  # Scalars are broadcast
  apply_dist(x, .f, !!!args)
}

# Evaluate `.f` for every distribution in `x`, once per storage slot. With
# `pos`, `args[[pos]]` is paired with the distributions by position (and
# recycled if it has size 1). Other arguments are passed unchanged.
apply_slots <- function(x, .f, args, pos = NULL) {
  if (is.null(pos)) {
    return(vecvec::vecvec_mapply(
      list(x), function(vals) inject(.f(vals, !!!args)), SIMPLIFY = TRUE
    ))
  }
  vecvec::vecvec_mapply(
    list(x, args[[pos]]),
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

# Values of a multivariate distribution's variates (`at`, `q`) should be given
# as a matrix with one column for each variate. A bare vector is ambiguous about
# whether it describes one point or several, so it is deprecated and for now
# interpreted as a matrix with one column for each variate.
validate_arg_dim <- function(x, value, arg) {
  # Matrices already describe the points, lists are recycled by broadcast_dist()
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
