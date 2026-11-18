# These defaults are inherently per-distribution numerical methods. When they
# are used by a vectorised (ported) family with more than one element sharing
# the call, they loop over `vec_slice(x, i)`, pairing `vec_slice()` of the
# broadcast argument (size `n` or 1) with each element. See `_dev/dist-port.md`.
default_arg_slice <- function(arg, i) {
  if (vctrs::vec_size(arg) == 1L) arg else vctrs::vec_slice(arg, i)
}

# Combine per-element results into a vector, or a matrix with one row each.
default_bind <- function(res) {
  if (any(vapply(res, is.matrix, logical(1L)))) {
    do.call(rbind, res)
  } else {
    unlist(res)
  }
}

# Call `.f(x_i, arg_i, ...)` for each distribution `x_i` of `x` (as a size 1
# `distribution` vector slice), where `arg_i` is the argument paired with it by
# position (`arg` is omitted if `.f` doesn't take one), and combine the results
# with `.combine()`, which takes the list of results.
per_dist <- function(x, .f, arg, ..., .combine = default_bind) {
  has_arg <- !missing(arg)
  res <- lapply(vctrs::vec_seq_along(x), function(i) {
    if (!has_arg) {
      .f(vctrs::vec_slice(x, i), ...)
    } else {
      .f(vctrs::vec_slice(x, i), default_arg_slice(arg, i), ...)
    }
  })
  .combine(res)
}
bind_c <- function(res) vctrs::vec_c(!!!res)
# Univariate results are a vector; multivariate ones a list of matrices
bind_covariance <- function(res) {
  if (any(vapply(res, is.matrix, logical(1L)))) res else unlist(res)
}

#' @export
density.dist_default <- function(x, ...){
  abort(
    sprintf("The distribution class `%s` does not support `density()`",
            class(x)[1])
  )
}

#' @export
log_density.dist_default <- function(x, ...){
  log(density(x, ...))
}

#' @export
quantile.dist_default <- function(x, p, ..., tail = "lower"){
  if(tail == "upper") return(quantile(x, 1 - p, ...))
  if (vctrs::vec_size(x) != 1L) return(per_dist(x, quantile, p, ...))
  # abort(
  #   sprintf("The distribution class `%s` does not support `quantile()`",
  #           class(x)[1])
  # )
  vapply(p, function(p) {
    stats::optim(0, function(pos){
      (p - cdf(x, pos, ...))^2
    })$par
  }, numeric(1L))
}
#' @export
log_quantile.dist_default <- function(x, p, ...){
  quantile(x, exp(p), ...)
}
#' @export
cdf.dist_default <- function(x, q, times = 1e5, ..., tail = "lower"){
  if(tail == "upper") return(1 - cdf(x, q, times = times, ...))
  if (vctrs::vec_size(x) != 1L) return(per_dist(x, cdf, q, times = times, ...))
  # Use Monte Carlo integration
  r <- generate(x, times = times)[[1L]]
  if(is.list(q)) {
    # Turn into matrix
    q <- do.call(rbind, q)
  }
  if(!is.matrix(r)) {
    return(vapply(q, function(q) mean(r <= q), numeric(1L)))
  }
  out <- numeric(NROW(q))
  for(i in seq_along(out)) {
    out[i] <- mean(apply(sweep(r, 2, q[i,]) < 0, 1, all))
  }
  return(out)
}
#' @export
log_cdf.dist_default <- function(x, q, ...){
  log(cdf(x, q, ...))
}

#' @export
generate.dist_default <- function(x, times, ...){
  times <- vctrs::vec_recycle(times, vctrs::vec_size(x))
  lapply(vctrs::vec_seq_along(x), function(i) {
    vapply(stats::runif(times[[i]], 0, 1), quantile, numeric(1L), x = vctrs::vec_slice(x, i), ...)
  })
}

# `sample` is a list of samples paired with the distributions by position
#' @export
likelihood.dist_default <- function(x, sample, ...){
  vapply(vctrs::vec_seq_along(x), function(i) {
    prod(vapply(sample[[i]], density, numeric(1L), x = vctrs::vec_slice(x, i)))
  }, numeric(1L))
}

#' @export
log_likelihood.dist_default <- function(x, sample, ...){
  vapply(vctrs::vec_seq_along(x), function(i) {
    sum(vapply(sample[[i]], log_density, numeric(1L), x = vctrs::vec_slice(x, i)))
  }, numeric(1L))
}

#' @export
parameters.dist_default <- function(x, ...) {
  flds <- vctrs::fields(x)
  cols <- lapply(flds, function(f) vctrs::field(x, f))
  names(cols) <- flds
  vctrs::new_data_frame(cols, n = vctrs::vec_size(x))
}

#' @export
family.dist_default <- function(object, recursive = FALSE, ...) {
  fam <- substring(class(object)[1], first = 6)
  if (!recursive) return(rep_len(fam, vctrs::vec_size(object)))

  # The recursive form describes one distribution at a time; loop when this
  # default is used by a vectorised (ported) family with more than one
  # element sharing the call. See `_dev/vecvec.md` section 3.4.
  if (vctrs::vec_size(object) != 1L) {
    return(per_dist(object, family, recursive = TRUE, .combine = identity))
  }

  # Identify fields which hold embedded distribution(s), and recurse into
  # them: either a single distribution (e.g. dist_inflated()'s `dist`, stored
  # as a raw record or a `distribution` vector), or a list of distributions
  # (e.g. dist_mixture()'s `dist`).
  flds <- vctrs::fields(object)
  nested <- lapply(flds, function(f) {
    z <- dist_field_get(object, f)
    if (is_distribution(z)) {
      family(z, recursive = TRUE)[[1L]]
    } else if (inherits(z, "dist_default")) {
      family(z, recursive = TRUE)
    } else if (is.list(z) && length(z) && all(vapply(z, inherits, logical(1L), "dist_default"))) {
      unname(lapply(z, family, recursive = TRUE))
    } else {
      NULL
    }
  })
  names(nested) <- flds
  nested <- nested[!vapply(nested, is.null, logical(1L))]

  if (!length(nested)) fam else c(list(family = fam), nested)
}

#' @export
support.dist_default <- function(x, ...) {
  # `quantile()`/`density()` below assume a single distribution (`c(0, 1)` is
  # a *constant* pair of probabilities here, not one-per-element); loop when
  # this default is used by a vectorised (ported) family with more than one
  # element sharing the call. See `_dev/vecvec.md` section 3.4.
  if (vctrs::vec_size(x) != 1L) {
    return(per_dist(x, support, ..., .combine = bind_c))
  }
  # `x` is a single raw distribution here, so its methods can't be mapped over
  # several points. Go through `distribution` which maps (one list element).
  dx <- wrap_dist(list(x))
  lims <- quantile(dx, c(0, 1))[[1L]]
  closed <- if(any(is.na(lims))) {
    c(FALSE, FALSE)
  } else {
    # Default to open limits on error
    lim_dens <- tryCatch(
      suppressWarnings(density(dx, lims)[[1L]]),
      error = function(e) c(0,0)
    )
    !near(lim_dens, 0)
  }
  new_support_region(
    list(vctrs::vec_init(generate(dx, 1)[[1L]], n = 0L)),
    list(lims),
    list(closed)
  )
}

#' @export
has_symmetry.dist_default <- function(x, ...){
  # Crude test that works in *most* cases.
  # Should be replaced with a more robust check using moments.
  tryCatch(
    unname(skewness(x) == 0),
    error = function(e) rep_len(FALSE, vctrs::vec_size(x))
  )
}

#' @export
mean.dist_default <- function(x, ...){
  if (vctrs::vec_size(x) != 1L) {
    return(per_dist(x, mean, ...))
  }
  dx <- wrap_dist(list(x))
  x_sup <- support(x)
  dist_type <- field(x_sup, "x")[[1]]
  if (!is.numeric(dist_type)) return(NA_real_)
  if (is.double(dist_type)) {
    limits <- field(x_sup, "lim")[[1]]
    tryCatch(
      stats::integrate(function(at) density(dx, at)[[1L]] * at, limits[1], limits[2])$value,
      error = function(e) NA_real_
    )
  } else {
    mean(quantile(dx, stats::ppoints(1000))[[1L]], na.rm = TRUE)
  }
}
#' @export
variance.dist_default <- function(x, ...){
  if (vctrs::vec_size(x) != 1L) {
    return(per_dist(x, variance, ...))
  }
  x <- covariance(x, ...)
  if(is.matrix(x[[1]]) && ncol(x[[1]]) > 1){
    matrix(diag(x[[1]]), nrow = 1)
  } else x
}
#' @export
covariance.dist_default <- function(x, ...){
  if (vctrs::vec_size(x) != 1L) {
    return(per_dist(x, covariance, ..., .combine = bind_covariance))
  }
  dx <- wrap_dist(list(x))
  x_sup <- support(x)
  dist_type <- field(x_sup, "x")[[1]]
  if (!is.numeric(dist_type)) return(NA_real_)
  else if (is.matrix(dist_type)) stats::cov(generate(x, times = 1000)[[1L]])
  else if (is.double(dist_type)) {
    limits <- field(x_sup, "lim")[[1]]
    tryCatch(
      stats::integrate(function(at) density(dx, at)[[1L]] * at^2, limits[1], limits[2])$value,
      error = function(e) NA_real_
    ) - mean(x)^2
  } else {
    stats::var(quantile(dx, stats::ppoints(1000))[[1L]], na.rm = TRUE)
  }
}

#' @export
median.dist_default <- function(x, na.rm = FALSE, ...){
  quantile(x, p = 0.5, ...)
}

#' @export
hilo.dist_default <- function(x, size = 95, ...){
  lower <- quantile(x, (100-size)/200, ...)
  upper <- quantile(x, (100+size)/200, ...)
  if(is.matrix(lower) && is.matrix(upper)) {
    # One row of `hilo` columns per distribution
    return(vctrs::vec_rbind(!!!lapply(seq_len(nrow(lower)), function(i) {
      vctrs::new_data_frame(split(
        new_hilo(lower[i, ], upper[i, ], size = rep_len(size, ncol(lower))),
        seq_len(ncol(lower))))
    })))
  }
  new_hilo(lower, upper, size)
}

#' @export
hdr.dist_default <- function(x, size = 95, n = 4096, ...){
  # This implementation is inherently scalar (it works on one distribution's
  # density grid at a time); loop when this default is used by a vectorised
  # (ported) family with more than one element sharing the call. See
  # `_dev/vecvec.md` section 3.4.
  if (vctrs::vec_size(x) != 1L) {
    return(per_dist(x, hdr, size = size, n = n, ..., .combine = bind_c))
  }
  # Use exact quantiles for symmetric distributions
  if (has_symmetry(x)) {
    return(
      new_hdr(
        lower = list(quantile(x, (100 - size)/200)),
        upper = list(quantile(x, (100 + size)/200)),
        size = size
      )
    )
  }

  # `x` is a single raw distribution, whose methods only take arguments paired
  # with it by position (size 1). Go through `distribution`, which maps the
  # vectors of probabilities and points over it (one list element).
  dx <- wrap_dist(list(x))
  dist_x <- quantile(dx, seq(0.5/n, 1 - 0.5/n, length.out = n))[[1L]]
  # Remove duplicate values of dist_x from less continuous distributions
  dist_x <- unique(dist_x)
  dist_y <- density(dx, dist_x)[[1L]]
  alpha <- quantile(dist_y, probs = 1-size/100)

  crossing_alpha <- function(alpha, x, y){
    it <- seq_len(length(y) - 1)
    dd <- y - alpha
    dd <- dd[it + 1] * dd[it]
    index <- it[dd <= 0]
    # unique() removes possible duplicates if sequential dd has same value.
    # More robust approach is required.
    out <- unique(
      vapply(
        index,
        function(.x) stats::approx(y[.x + c(0,1)], x[.x + c(0,1)], xout = alpha)$y,
        numeric(1L)
      )
    )
    # Add boundary values which may exceed the crossing point.
    c(x[1][y[1]>alpha], out, x[length(x)][y[length(y)]>alpha])
  }

  # purrr::map(alpha, crossing_alpha, dist_x, dist_y)
  hdr <- crossing_alpha(alpha, dist_x, dist_y)
  lower_hdr <- seq_along(hdr)%%2==1
  new_hdr(lower = list(hdr[lower_hdr]), upper = list(hdr[!lower_hdr]), size = size)
}

#' @export
format.dist_default <- function(x, ...){
  "?"
}

#' @export
print.dist_default <- function(x, ...){
  cat(format(x, ...))
}

#' @export
dim.dist_default <- function(x){
  # Dimensions of random draws is the cheapest approach for the default dim().
  # The RNG state is restored so it does not affect the global random state.
  suppressWarnings(restore_rng(NCOL(generate(x, times = 1)[[1L]])))
}

invert_fail <- function(...) stop("Inverting transformations for distributions is not yet supported.")

#' Attempt to get the inverse of f(x) by name. Returns invert_fail
#' (a function that raises an error if called) if there is no known inverse.
#' @param f string. Name of a function.
#' @noRd
get_unary_inverse <- function(f) {
  switch(f,
    sqrt = function(x) x^2,
    exp = log,
    log = function(x, base = exp(1)) base ^ x,
    log2 = function(x) 2^x,
    log10 = function(x) 10^x,
    expm1 = log1p,
    log1p = expm1,
    cos = acos,
    sin = asin,
    tan = atan,
    acos = cos,
    asin = sin,
    atan = tan,
    cosh = acosh,
    sinh = asinh,
    tanh = atanh,
    acosh = cosh,
    asinh = sinh,
    atanh = tanh,

    invert_fail
  )
}

#' Attempt to get the inverse of f(x, constant) by name. Returns invert_fail
#' (a function that raises an error if called) if there is no known inverse.
#' @param f string. Name of a function.
#' @param constant a constant value
#' @noRd
get_binary_inverse_1 <- function(f, constant) {
  force(constant)

  switch(f,
    `+` = function(x) x - constant,
    `-` = function(x) x + constant,
    `*` = function(x) x / constant,
    `/` = function(x) x * constant,
    `^` = function(x) {
      # Odd integer powers are monotonic over the reals, so their real root
      # must be defined for negative x (where `x^(1/k)` is NaN in R).
      if (length(constant) == 1L && is.finite(constant) && constant %% 2 == 1) {
        sign(x) * abs(x) ^ (1/constant)
      } else {
        x ^ (1/constant)
      }
    },

    invert_fail
  )
}

#' Attempt to get the inverse of f(constant, x) by name. Returns invert_fail
#' (a function that raises an error if called) if there is no known inverse.
#' @param f string. Name of a function.
#' @param constant a constant value
#' @noRd
get_binary_inverse_2 <- function(f, constant) {
  force(constant)

  switch(f,
    `+` = function(x) x - constant,
    `-` = function(x) constant - x,
    `*` = function(x) x / constant,
    `/` = function(x) constant / x,
    `^` = function(x) log(x, base = constant),

    invert_fail
  )
}

# Whether a record of distributions is multivariate (judged from its first
# distribution, as `dim()` of a family describes the whole family)
dist_is_multivariate <- function(x) {
  vctrs::vec_size(x) > 0L && dim(vctrs::vec_slice(x, 1L)) > 1
}

# Group the positions of a list of functions `fns` by `identical()` functions,
# returning a group id for each position (in order of first appearance).
fn_group_id <- function(fns) {
  id <- integer(length(fns))
  reps <- list()
  for (i in seq_along(fns)) {
    k <- 0L
    for (j in seq_along(reps)) {
      if (identical(fns[[i]], fns[[reps[[j]]]])) {
        k <- j
        break
      }
    }
    if (k == 0L) {
      reps[[length(reps) + 1L]] <- i
      k <- length(reps)
    }
    id[i] <- k
  }
  id
}

# Build one function with `.f(i)` for each group of positions sharing the same
# `...` keys (vectors or group ids of equal size), and return them as a list
# with one (shared) function object per position. Sharing the function objects
# lets `transformed_by_group()` handle the positions of a group together.
fns_by_group <- function(.f, ...) {
  keys <- list(...)
  names(keys) <- paste0("k", seq_along(keys))
  group <- vctrs::vec_group_id(vctrs::new_data_frame(keys))
  first <- match(seq_len(attr(group, "n")), group)
  lapply(first, .f)[group]
}

#' @method Math dist_default
#' @export
Math.dist_default <- function(x, ...) {
  if(dist_is_multivariate(x)) stop("Transformations of multivariate distributions are not yet supported.")

  trans <- new_function(exprs(x = ), body = expr((!!sym(.Generic))(x, !!!dots_list(...))))

  inverse_fun <- get_unary_inverse(.Generic)
  inverse <- new_function(exprs(x = ), body = expr((!!inverse_fun)(x, !!!dots_list(...))))

  dist_record(dist_transformed(wrap_dist(list(x)), trans, inverse))
}

#' @method Ops dist_default
#' @export
Ops.dist_default <- function(e1, e2) {
  if(.Generic %in% c("-", "+") && missing(e2)){
    e2 <- e1
    e1 <- if(.Generic == "+") 1 else -1
    .Generic <- "*"
  }
  is_dist <- c(inherits(e1, "dist_default"), inherits(e2, "dist_default"))
  if(any(vapply(list(e1, e2)[is_dist], dist_is_multivariate, logical(1L)))){
    stop("Transformations of multivariate distributions are not yet supported.")
  }

  # The operands are paired by position (a size 1 operand is recycled)
  n <- max(vctrs::vec_size(e1), vctrs::vec_size(e2))
  e1 <- vctrs::vec_recycle(e1, n)
  e2 <- vctrs::vec_recycle(e2, n)

  # For + and - between two independent distributions use numerical convolution
  if (all(is_dist) && .Generic %in% c("+", "-")) {
    d1 <- wrap_dist(list(e1))
    d2 <- wrap_dist(list(e2))
    if (.Generic == "-") {
      d2 <- -d2
    }
    return(dist_record(dist_convolved(d1, d2)))
  }

  if(all(is_dist)) {
    # Transformations of the same distributions can be combined. The
    # distributions are compared one at a time as their raw records, since
    # `identical()` doesn't compare the storage of `distribution` vectors.
    same <- inherits(e1, "dist_transformed") && inherits(e2, "dist_transformed") &&
      all(vapply(seq_len(n), function(i) {
        identical(dist_elem(e1[["dist"]], i), dist_elem(e2[["dist"]], i))
      }, logical(1L)))
    if (!same) {
      stop(sprintf("The %s operation is not supported for <%s> and <%s>", .Generic, class(e1)[1], class(e2)[1]))
    }
    tf1 <- e1[["transform"]]
    tf2 <- e2[["transform"]]
    trans <- fns_by_group(function(i) {
      new_function(exprs(x = ), expr((!!sym(.Generic))((!!tf1[[i]])(x), (!!tf2[[i]])(x))))
    }, fn_group_id(tf1), fn_group_id(tf2))
    return(dist_record(dist_transformed(e1[["dist"]], trans, list(invert_fail))))
  }

  # Ops between distributions and constants, with one transformation (and
  # inverse) for each unique constant
  generic <- .Generic
  if (is_dist[1]) {
    dist <- e1
    constant <- e2
    trans_i <- function(i) {
      new_function(exprs(x = ), body = expr((!!sym(generic))(x, !!constant[[i]])))
    }
    inverse_i <- function(i) get_binary_inverse_1(generic, constant[[i]])
  } else {
    dist <- e2
    constant <- e1
    trans_i <- function(i) {
      new_function(exprs(x = ), body = expr((!!sym(generic))(!!constant[[i]], x)))
    }
    inverse_i <- function(i) get_binary_inverse_2(generic, constant[[i]])
  }
  constant <- unname(constant)
  dist_record(dist_transformed(
    wrap_dist(list(dist)), fns_by_group(trans_i, constant), fns_by_group(inverse_i, constant)
  ))
}

# If there are mix of Ops with default, then use the default method.
# `chooseOpsMethod()` only exists in R >= 4.3.0, so this method is registered
# conditionally in `.onLoad()` (R/zzz.R) rather than in the NAMESPACE.
# `@exportS3Method NULL` tells roxygen that it isn't exported on purpose.
#' @exportS3Method NULL
#' @noRd
chooseOpsMethod.dist_default <- function(x, y, mx, my, cl, reverse) {
  # A missing operand gives a missing result, whichever side it is on
  inherits(x, "dist_na") || !inherits(y, "dist_na")
}