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
quantile.dist_default <- function(x, p, ...){
  # abort(
  #   sprintf("The distribution class `%s` does not support `quantile()`",
  #           class(x)[1])
  # )
  stats::optim(0, function(pos){
    (p - cdf(x, pos, ...))^2
  })$par
}
#' @export
log_quantile.dist_default <- function(x, p, ...){
  quantile(x, exp(p), ...)
}
#' @export
cdf.dist_default <- function(x, q, times = 1e5,...){
  # Use Monte Carlo integration
  r <- generate(x, times = times)
  if(is.list(q)) {
    # Turn into matrix
    q <- do.call(rbind, q)
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
  vapply(stats::runif(times,0,1), quantile, numeric(1L), x = x, ...)
}

#' @export
likelihood.dist_default <- function(x, sample, ...){
  prod(vapply(sample, density, numeric(1L), x = x))
}

#' @export
log_likelihood.dist_default <- function(x, sample, ...){
  sum(vapply(sample, log_density, numeric(1L), x = x))
}

#' @export
parameters.dist_default <- function(x, ...) {
  # Reduce parameter structures to length 1 list if needed.
  lapply(unclass(x), function(z) {
    if(inherits(z, "dist_default")) wrap_dist(list(z))
    else if (tryCatch(vec_size(z), error = function(e) Inf) > 1) list(z)
    else z
  })
}

#' @export
family.dist_default <- function(object, ...) {
  substring(class(object)[1], first = 6)
}

#' @export
support.dist_default <- function(x, ...) {
  lims <- quantile(x, c(0, 1))
  closed <- if(any(is.na(lims))) {
    c(FALSE, FALSE)
  } else {
    # Default to open limits on error
    lim_dens <- tryCatch(
      suppressWarnings(density(x, lims)),
      error = function(e) c(0,0)
    )
    !near(lim_dens, 0)
  }
  new_support_region(
    list(vctrs::vec_init(generate(x, 1), n = 0L)),
    list(lims),
    list(closed)
  )
}

#' @export
mean.dist_default <- function(x, ...){
  x_sup <- support(x)
  dist_type <- field(x_sup, "x")[[1]]
  if (!is.numeric(dist_type)) return(NA_real_)
  if (is.double(dist_type)) {
    limits <- field(x_sup, "lim")[[1]]
    tryCatch(
      stats::integrate(function(at) density(x, at) * at, limits[1], limits[2])$value,
      error = function(e) NA_real_
    )
  } else {
    mean(quantile(x, stats::ppoints(1000)), na.rm = TRUE)
  }
}
#' @export
variance.dist_default <- function(x, ...){
  x <- covariance(x, ...)
  if(is.matrix(x[[1]]) && ncol(x[[1]]) > 1){
    matrix(diag(x[[1]]), nrow = 1)
  } else x
}
#' @export
covariance.dist_default <- function(x, ...){
  x_sup <- support(x)
  dist_type <- field(x_sup, "x")[[1]]
  if (!is.numeric(dist_type)) return(NA_real_)
  else if (is.matrix(dist_type)) stats::cov(generate(x, times = 1000))
  else if (is.double(dist_type)) {
    limits <- field(x_sup, "lim")[[1]]
    tryCatch(
      stats::integrate(function(at) density(x, at) * at^2, limits[1], limits[2])$value,
      error = function(e) NA_real_
    ) - mean(x)^2
  } else {
    stats::var(quantile(x, stats::ppoints(1000)), na.rm = TRUE)
  }
}

#' @export
median.dist_default <- function(x, na.rm = FALSE, ...){
  quantile(x, p = 0.5, ...)
}

#' @export
hilo.dist_default <- function(x, size = 95, ...){
  lower <- quantile(x, 0.5-size/200, ...)
  upper <- quantile(x, 0.5+size/200, ...)
  if(is.matrix(lower) && is.matrix(upper)) {
    return(
      vctrs::new_data_frame(split(
        new_hilo(drop(lower), drop(upper), size = rep_len(size, length(lower))),
      seq_along(lower)))
    )
  }
  new_hilo(lower, upper, size)
}

#' @export
hdr.dist_default <- function(x, size = 95, n = 512, ...){
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
  # Quick and dirty dimension calculation
  NCOL(generate(x, times = 1))
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

  vec_data(dist_transformed(wrap_dist(list(x)), trans, inverse))[[1]]
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

  trans <- if(all(is_dist)) {
    if(identical(e1$dist, e2$dist)){
      new_function(exprs(x = ), expr((!!sym(.Generic))((!!e1$transform)(x), (!!e2$transform)(x))))
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

  vec_data(dist_transformed(wrap_dist(list(e1,e2)[which(is_dist)]), trans, inverse))[[1]]
}
