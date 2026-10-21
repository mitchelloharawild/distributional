#' Apply a statistic/probability-function generic across a distribution vector
#'
#' @description
#' For each storage slot (one per distribution family present in `x`),
#' `apply_dist()` either makes a single vectorised call to `.f` (when the
#' family is in the "ported" registry, see `R/00_classes.R`) or falls back to
#' looping element-by-element and combining the results (matching the
#' pre-migration behaviour, which every family's methods still support). This
#' lets families be vectorised one at a time - see `_dev/vecvec.md` phase 1.
#'
#' @noRd
apply_dist <- function(x, .f, ...){
  dn <- variates(x)
  loc <- locate_slots(x)
  n <- loc$n

  args <- dots_list(...)
  is_arg_listable <- vapply(args, inherits, FUN.VALUE = logical(1L), "arg_listable")
  if (sum(is_arg_listable) > 1) abort("Only one distribution argument can be unpacked at a time.\nThis shouldn't happen, please report a bug at https://github.com/mitchelloharawild/distributional/issues/")
  arg_pos <- if (any(is_arg_listable)) which(is_arg_listable) else NA_integer_

  unpack_listable <- multi_arg <- FALSE
  .unpack_names <- NULL
  if (!is.na(arg_pos)) {
    arg <- args[[arg_pos]]
    class(arg) <- setdiff(class(arg), "arg_listable")
    if (unpack_listable <- is_list_of(arg)) {
      validate_recycling(x, arg)
      .unpack_names <- names(arg)
      # `arg` is a named list of *argument* vectors (e.g. `list(a = <vec>, b =
      # <vec>)`), not one entry per distribution; `transpose_c()` recycles
      # those against each other to a common length and regroups into one
      # small named-value vector per distribution position. That common
      # length is 1 (every distribution gets the same values) or `n`
      # (validated above); recycle up to `n` so per-position indexing below
      # always has one entry per distribution.
      arg <- vec_recycle(transpose_c(arg), size = n)
    } else {
      multi_arg <- length(arg) > 1
    }
    args[[arg_pos]] <- arg
  }

  out <- vector("list", n)
  for (k in seq_along(loc$slots)) {
    pos <- loc$pos[[k]]
    if (!length(pos)) next
    vals <- loc$slots[[k]][loc$local[pos]]

    if (is_ported_dist(vals) && !unpack_listable && !multi_arg) {
      # One vectorised call across the whole slot.
      res <- rlang::inject(.f(vals, !!!args))
      out[pos] <- vctrs::vec_chop(res)
    } else {
      # Not (yet) ported, or a broadcasting form that still needs a loop:
      # fall back to the pre-migration per-element behaviour.
      for (j in seq_along(pos)) {
        p <- pos[j]
        elem_args <- args
        if (!is.na(arg_pos)) {
          elem_args[[arg_pos]] <- if (unpack_listable) {
            # True per-distribution pairing: already regrouped by position.
            args[[arg_pos]][[p]]
          } else {
            # Constant, broadcast to every element - matching mapply()'s
            # recycling in the pre-migration implementation: a length-1 list
            # (e.g. a single-row matrix split by `split_matrix_rows()`)
            # unwraps to its one element, everything else (an atomic vector,
            # or a longer list meant to be seen whole by every call, e.g. a
            # multi-row matrix) passes through unchanged.
            a <- args[[arg_pos]]
            if (is.list(a) && length(a) == 1L) a[[1L]] else a
          }
        }
        out[[p]] <- rlang::inject(.f(vals[j], !!!elem_args))
      }
    }
  }

  # NA-index positions never reach a family method above; fill them in with
  # an NA-flavoured placeholder of the same ptype as a computed sibling, so
  # the reshape below doesn't silently shrink the output length.
  is_null <- vapply(out, is.null, logical(1L))
  if (any(is_null) && !all(is_null)) {
    proto <- out[[which(!is_null)[1L]]]
    out[is_null] <- list(vctrs::vec_init(proto, 1L))
  }

  if(unpack_listable) {
    # TODO - update and repair multivariate distribution i/o with unpacking
    out <- as_list_of(out)
    if (rbind_mat <- is.matrix(attr(out, "ptype"))) {
      out <- as_list_of(lapply(out, split_matrix_rows))
    }
    out <- transpose_c(out)
    if(rbind_mat) {
      out <- lapply(out, function(z) `colnames<-`(do.call(rbind, z), dn))
    }
    names(out) <- .unpack_names
    out <- new_data_frame(out, n = n)
  } else if(multi_arg) {
    if(length(dn) > 1) out <- lapply(out, function(z) set_matrix_dimnames(z, dn))
  } else {
    out <- vec_c(!!!out)
    out <- if(vec_is_list(out))
      lapply(out, set_matrix_dimnames, dn = dn)
    else
      set_matrix_dimnames(out, dn)
  }
  out
}

set_matrix_dimnames <- function(x, dn) {
  if((is.matrix(x) || is.data.frame(x)) && !is.null(dn)){
    # Set dimension names
    colnames(x) <- dn
  }
  x
}

transpose_c <- function(.l) {
  stopifnot(is_list_of(.l))
  .ptype <- vec_init(attr(.l, "ptype"), 1L)
  if(is_empty(.l)) return(.l)
  inner_names <- names(.l[[1L]])
  .l <- vec_recycle_common(!!!.l)
  result <- lapply(seq_along(.l[[1L]]), function(i) {
    unname(vec_c(!!!lapply(.l, vec_slice, i), .ptype = .ptype))
  })
  set_names(result, inner_names)
}

split_matrix_rows <- function(x) {
  lapply(seq_len(nrow(x)), function(i) x[i,,drop=FALSE])
}

# Declare a function's argument as allowing list inputs for mapping values
arg_listable <- function(x, .ptype) {
  if(is.list(x)) {
    x <- as_list_of(as.list(x), .ptype)
    if(is.matrix(attr(x, "ptype"))) {
      x <- lapply(x, split_matrix_rows)
      x <- as_list_of(x, .ptype)
    }
    if(is.null(names(x))) {
      names(x) <- vec_as_names(character(vec_size(x)), repair = "unique")
    }
  } else if(is.matrix(x)) {
    x <- split_matrix_rows(x)
  } else {
    vec_assert(x, .ptype)
  }
  # Declares list arguments to be unpacked for apply_dist()
  class(x) <- c("arg_listable", class(x))
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
