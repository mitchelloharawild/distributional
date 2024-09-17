#' Modify a distribution with a transformation
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' The [`density()`], [`mean()`], and [`variance()`] methods are approximate as
#' they are based on numerical derivatives.
#'
#' @param dist A univariate distribution vector.
#' @param transform A function used to transform the distribution. This
#' transformation should be monotonic over appropriate domain.
#' @param inverse The inverse of the `transform` function.
#'
#' @examples
#' # Create a log normal distribution
#' dist <- dist_transformed(dist_normal(0, 0.5), exp, log)
#' density(dist, 1) # dlnorm(1, 0, 0.5)
#' cdf(dist, 4) # plnorm(4, 0, 0.5)
#' quantile(dist, 0.1) # qlnorm(0.1, 0, 0.5)
#' generate(dist, 10) # rlnorm(10, 0, 0.5)
#'
#' @export
dist_transformed <- function(dist, transform, inverse){
  vec_is(dist, new_dist())
  if(is.function(transform)) transform <- list(transform)
  if(is.function(inverse)) inverse <- list(inverse)
  new_dist(dist = vec_data(dist),
           transform = transform, inverse = inverse,
           dimnames = dimnames(dist), class = "dist_transformed")
}

#' @export
format.dist_transformed <- function(x, ...){
  sprintf(
    "t(%s)",
    format(x[["dist"]])
  )
}

#' @export
support.dist_transformed <- function(x, ...) {
  support <- support(x[["dist"]])
  lim <- field(support, "lim")[[1]]
  lim <- suppressWarnings(x[['transform']](lim))
  if (all(!is.na(lim))) {
    lim <- sort(lim)
  }
  field(support, "lim")[[1]] <- lim
  support
}

#' @export
density.dist_transformed <- function(x, at, ...){
  inv <- function(v) suppressWarnings(x[["inverse"]](v))
  jacobian <- vapply(at, numDeriv::jacobian, numeric(1L), func = inv)
  d <- density(x[["dist"]], inv(at)) * abs(jacobian)
  limits <- field(support(x), "lim")[[1]]
  closed <- field(support(x), "closed")[[1]]
  if (!any(is.na(limits))) {
    `%less_than%` <- if (closed[1]) `<` else `<=`
    `%greater_than%` <- if (closed[2]) `>` else `>=`
    d[which(at %less_than% limits[1] | at %greater_than% limits[2])] <- 0
  }
  d
}

#' @export
cdf.dist_transformed <- function(x, q, ...){
  inv <- function(v) suppressWarnings(x[["inverse"]](v))
  p <- cdf(x[["dist"]], inv(q), ...)
  # TODO - remove null dist check when dist_na is structured correctly (revdep temp fix)
  if(!is.null(x[["dist"]]) && !monotonic_increasing(x[["transform"]], support(x[["dist"]]))) p <- 1 - p

  # TODO: Rework for support of closed limits and prevent computation
  x_sup <- support(x)
  x_lim <- field(x_sup, "lim")[[1]]
  x_cls <- field(x_sup, "closed")[[1]]
  if (!any(is.na(x_lim))) {
    p[q <= x_lim[1] & !x_cls[1]] <- 0
    p[q >= x_lim[2] & !x_cls[2]] <- 1
  }
  p
}

#' @export
quantile.dist_transformed <- function(x, p, ...){
  # TODO - remove null dist check when dist_na is structured correctly (revdep temp fix)
  if(!is.null(x[["dist"]]) && !monotonic_increasing(x[["transform"]], support(x[["dist"]]))) p <- 1 - p
  x[["transform"]](quantile(x[["dist"]], p, ...))
}

#' @export
generate.dist_transformed <- function(x, ...){
  x[["transform"]](generate(x[["dist"]], ...))
}

#' @export
mean.dist_transformed <- function(x, ...){
  mu <- mean(x[["dist"]])
  sigma2 <- variance(x[["dist"]])
  if(is.na(sigma2)){
    # warning("Could not compute the transformed distribution's mean as the base distribution's variance is unknown. The transformed distribution's median has been returned instead.")
    return(x[["transform"]](mu))
  }
  drop(
    x[["transform"]](mu) + numDeriv::hessian(x[["transform"]], mu, method.args=list(d = 0.01))/2*sigma2
  )
}

#' @export
covariance.dist_transformed <- function(x, ...){
  mu <- mean(x[["dist"]])
  sigma2 <- variance(x[["dist"]])
  if(is.na(sigma2)) return(NA_real_)
  drop(
    numDeriv::jacobian(x[["transform"]], mu)^2*sigma2 + (numDeriv::hessian(x[["transform"]], mu, method.args=list(d = 0.01))*sigma2)^2/2
  )
}

#' @method Math dist_transformed
#' @export
Math.dist_transformed <- function(x, ...) {
  trans <- new_function(exprs(x = ), body = expr((!!sym(.Generic))((!!x$transform)(x), !!!dots_list(...))))

  inverse_fun <- get_unary_inverse(.Generic)
  inverse <- new_function(exprs(x = ), body = expr((!!x$inverse)((!!inverse_fun)(x, !!!dots_list(...)))))

  vec_data(dist_transformed(wrap_dist(list(x[["dist"]])), trans, inverse))[[1]]
}

#' @method Ops dist_transformed
#' @export
Ops.dist_transformed <- function(e1, e2) {
  if(.Generic %in% c("-", "+") && missing(e2)){
    e2 <- e1
    e1 <- if(.Generic == "+") 1 else -1
    .Generic <- "*"
  }
  is_dist <- c(inherits(e1, "dist_default"), inherits(e2, "dist_default"))
  trans <- if(all(is_dist)) {
    if(identical(e1$dist, e2$dist)){
      new_function(exprs(x = ), expr((!!sym(.Generic))((!!e1$transform)(x), (!!e2$transform)(x))))
    } else {
      stop(sprintf("The %s operation is not supported for <%s> and <%s>", .Generic, class(e1)[1], class(e2)[1]))
    }
  } else if(is_dist[1]){
    new_function(exprs(x = ), body = expr((!!sym(.Generic))((!!e1$transform)(x), !!e2)))
  } else {
    new_function(exprs(x = ), body = expr((!!sym(.Generic))(!!e1, (!!e2$transform)(x))))
  }

  inverse <- if(all(is_dist)) {
    invert_fail
  } else if(is_dist[1]){
    inverse_fun <- get_binary_inverse_1(.Generic, e2)
    new_function(exprs(x = ), body = expr((!!e1$inverse)((!!inverse_fun)(x))))
  } else {
    inverse_fun <- get_binary_inverse_2(.Generic, e1)
    new_function(exprs(x = ), body = expr((!!e2$inverse)((!!inverse_fun)(x))))
  }

  vec_data(dist_transformed(wrap_dist(list(list(e1,e2)[[which(is_dist)[1]]][["dist"]])), trans, inverse))[[1]]
}

monotonic_increasing <- function(f, support) {
  # Shortcut for identity function (used widely in ggdist)
  if(!is.primitive(f) && identical(body(f), as.name(names(formals(f))))) {
    return(TRUE)
  }

  # Currently assumes (without checking, #9) monotonicity of f over the domain
  x <- f(field(support, "lim")[[1]])
  isTRUE(x[[2L]] > x[[1L]])
}
