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
  .l <- if (is.null(pos)) list(x) else list(x, args[[pos]])
  out <- vecvec::vecvec_mapply(.l, function(vals, a) {
    if (!missing(a)) args[[pos]] <- a
    inject(.f(vals, !!!args))
  })
  if (!length(out@x) && !is.null(.ptype)) return(vec_init(.ptype, length(x)))
  vecvec::unvecvec(out)
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

# Values of a multivariate distribution's variates (`at`, `q`) should be given
# as a matrix with one column for each variate. A bare vector is ambiguous about
# whether it describes one point or several, so it is deprecated and for now
# interpreted as a matrix with one column for each variate. The elements of a
# list (recycled by broadcast_dist()) are interpreted in the same way.
validate_arg_dim <- function(x, value, arg) {
  d <- n_variates(x, arg)
  # Vectors are unambiguous for univariate distributions (they are many points)
  if(d == 1L) return(value)

  pts <- if(is.list(value)) value else list(value)
  is_vec <- vapply(pts, function(p) !is.matrix(p) && !is_empty(p), logical(1L))
  if(!any(is_vec)) return(value)

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

  pts[is_vec] <- lapply(pts[is_vec], function(p) {
    if(length(p) != 1L && length(p) %% d != 0L) {
      abort(sprintf(
        "Cannot use an input of length %i as points of a distribution with %i variates.",
        length(p), d
      ))
    }
    matrix(p, ncol = d)
  })
  if(is.list(value)) {
    value[] <- pts
    value
  } else {
    pts[[1L]]
  }
}

# The number of variates of the distributions in `x` (1 if there are none).
# Points can only describe distributions with the same number of variates, so
# a vector mixing numbers of variates is an error. Missing distributions have
# no variates, so they are ignored.
n_variates <- function(x, arg) {
  slots <- Filter(function(s) !inherits(s, "dist_na"), x@x)
  # dim() methods for distributions return a single number
  d <- unique(vapply(slots, function(s) as.integer(dim(s[1L]) %||% 1L), integer(1L)))
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
