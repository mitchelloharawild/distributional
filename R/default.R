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
  if (vctrs::vec_size(x) != 1L) {
    return(default_bind(lapply(vctrs::vec_seq_along(x), function(i) {
      quantile(vctrs::vec_slice(x, i), default_arg_slice(p, i), ...)
    })))
  }
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
  if (vctrs::vec_size(x) != 1L) {
    return(default_bind(lapply(vctrs::vec_seq_along(x), function(i) {
      cdf(vctrs::vec_slice(x, i), default_arg_slice(q, i), times = times, ...)
    })))
  }
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
    return(lapply(vctrs::vec_seq_along(object), function(i) {
      family(vctrs::vec_slice(object, i), recursive = TRUE)
    }))
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
    return(vctrs::vec_c(!!!lapply(vctrs::vec_seq_along(x), function(i) support(vctrs::vec_slice(x, i), ...))))
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
    return(default_bind(lapply(vctrs::vec_seq_along(x), function(i) {
      mean(vctrs::vec_slice(x, i), ...)
    })))
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
    return(default_bind(lapply(vctrs::vec_seq_along(x), function(i) {
      variance(vctrs::vec_slice(x, i), ...)
    })))
  }
  x <- covariance(x, ...)
  if(is.matrix(x[[1]]) && ncol(x[[1]]) > 1){
    matrix(diag(x[[1]]), nrow = 1)
  } else x
}
#' @export
covariance.dist_default <- function(x, ...){
  if (vctrs::vec_size(x) != 1L) {
    res <- lapply(vctrs::vec_seq_along(x), function(i) {
      covariance(vctrs::vec_slice(x, i), ...)
    })
    # Univariate results are a vector; multivariate ones a list of matrices
    return(if (any(vapply(res, is.matrix, logical(1L)))) res else unlist(res))
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
    return(vctrs::vec_c(!!!lapply(vctrs::vec_seq_along(x), function(i) {
      hdr(vctrs::vec_slice(x, i), size = size, n = n, ...)
    })))
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

  dist_x <- quantile(x, seq(0.5/n, 1 - 0.5/n, length.out = n))
  # Remove duplicate values of dist_x from less continuous distributions
  dist_x <- unique(dist_x)
  dist_y <- density(x, dist_x)
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
    `^` = function(x) x ^ (1/constant),

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

#' @method Math dist_default
#' @export
Math.dist_default <- function(x, ...) {
  if(dim(x) > 1) stop("Transformations of multivariate distributions are not yet supported.")

  trans <- new_function(exprs(x = ), body = expr((!!sym(.Generic))(x, !!!dots_list(...))))

  inverse_fun <- get_unary_inverse(.Generic)
  inverse <- new_function(exprs(x = ), body = expr((!!inverse_fun)(x, !!!dots_list(...))))

  dist_first_record(dist_transformed(wrap_dist(list(x)), trans, inverse))
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
  if(any(vapply(list(e1, e2)[is_dist], dim, numeric(1L)) > 1)){
    stop("Transformations of multivariate distributions are not yet supported.")
  }

  # For + and - between two independent distributions use numerical convolution
  if (all(is_dist) && .Generic %in% c("+", "-")) {
    d1 <- wrap_dist(list(e1))
    d2 <- wrap_dist(list(e2))
    if (.Generic == "-") {
      d2 <- -d2
    }
    return(dist_first_record(dist_convolved(d1, d2)))
  }

  trans <- if(all(is_dist)) {
    if(inherits(e1, "dist_transformed") && inherits(e2, "dist_transformed") &&
       identical(e1$dist, e2$dist)){
      new_function(exprs(x = ), expr((!!sym(.Generic))((!!e1$transform[[1L]])(x), (!!e2$transform[[1L]])(x))))
    } else {
      stop(sprintf("The %s operation is not supported for <%s> and <%s>", .Generic, class(e1)[1], class(e2)[1]))
    }
  } else if(is_dist[1]){
    new_function(exprs(x = ), body = expr((!!sym(.Generic))(x, !!e2)))
  } else {
    new_function(exprs(x = ), body = expr((!!sym(.Generic))(!!e1, x)))
  }

  inverse <- if(all(is_dist)) {
    invert_fail
  } else if(is_dist[1]){
    get_binary_inverse_1(.Generic, e2)
  } else {
    get_binary_inverse_2(.Generic, e1)
  }

  dist_first_record(dist_transformed(wrap_dist(list(e1,e2)[which(is_dist)]), trans, inverse))
}

# If there are mix of Ops with default, then use the default method
chooseOpsMethod.dist_default <- function(x, y, mx, my, cl, reverse) TRUE