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
#' @param deriv The derivative of the `inverse` function. If not provided, the
#' derivative will be computed numerically.
#'
#' @description
#' `dist_transformed()` requires you to explicitly provide the inverse and the
#' derivative of the inverse functions. To attempt automatic symbolic computation
#' of these functions, you can create a transformed distribution by appplying the
#' transformation to an existing distribution object, e.g. `exp(dist_normal(0, 0.5))`.
#'
#'
#' @examples
#' # Create a log normal distribution
#' dist <- dist_transformed(dist_normal(0, 0.5), exp, log)
#' density(dist, 1) # dlnorm(1, 0, 0.5)
#' cdf(dist, 4) # plnorm(4, 0, 0.5)
#' quantile(dist, 0.1) # qlnorm(0.1, 0, 0.5)
#' generate(dist, 10) # rlnorm(10, 0, 0.5)
#'
#' # provide a derivative of the inverse function to avoid numerical differentiation
#' dist <- dist_transformed(dist_normal(0, 0.5), exp, log, function(x) 1/x)
#'
#' @export
dist_transformed <- function(dist, transform, inverse, deriv = NULL){
  # TODO: if deriv is not provided, we can still try to compute it symbolically
  vec_is(dist, new_dist())
  if (is.function(transform)) transform <- list(transform)
  if (is.function(inverse)) inverse <- list(inverse)
  if (is.function(deriv)) deriv <- list(deriv)
  new_dist(dist = vec_data(dist),
           transform = transform, inverse = inverse, deriv = deriv,
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
  # temporary fix for 1/dist_wrap('norm')
  if (identical(lim, c(0,0))) lim <- c(-Inf, Inf)
  field(support, "lim")[[1]] <- lim
  support
}

#' @export
density.dist_transformed <- function(x, at, deriv_method = "sym",
                                     verbose = getOption('dist.verbose', FALSE), ...) {
  inv <- x[["inverse"]]
  if (is.null(x[['deriv']]) || deriv_method == "numeric") {
    if (verbose) message('Using numerical differentiation.')
    jacobian <- suppressWarnings(
      vapply(at, numDeriv::jacobian, numeric(1L), func = inv)
    )
  } else {
    if (verbose) message('Using symbolic differentiation')
    jacobian <- suppressWarnings(x[['deriv']](at))
  }

  d <- suppressWarnings(density(x[["dist"]], inv(at)) * abs(jacobian))

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
  limits <- field(support(x), "lim")[[1]]
  if (!any(is.na(limits))) {
    p[q <= limits[1]] <- 0
    p[q >= limits[2]] <- 1
  }
  p
}

#' @export
quantile.dist_transformed <- function(x, p, ...){
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
  dots <- dots_list(...)
  trans <- new_function(exprs(x = ), body = expr((!!sym(.Generic))((!!x$transform)(x), !!!dots)))

  # replace x in the previous inverse function body with the current inverse function body
  prev_inverse <- x$inverse
  current_inverse <- get_unary_inverse(.Generic, ...)
  body(prev_inverse) <- substituteDirect(body(prev_inverse), list(x = body(current_inverse)))
  inverse <- Deriv::Simplify(prev_inverse)

  deriv <- suppressWarnings(try(Deriv::Deriv(inverse, x = 'x'), silent = TRUE))
  if (inherits(deriv, "try-error")) {
    message('Cannot compute the derivative of the inverse function symbolicly.')
  }

  vec_data(dist_transformed(wrap_dist(list(x[["dist"]])), trans, inverse, deriv))[[1]]
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

  if (all(is_dist)) {
    inverse <- invert_fail
  } else if (is_dist[1]) {
    current_inverse <- get_binary_inverse_1(.Generic, e2)
    prev_inverse <- e1$inverse
  } else {
    current_inverse <- get_binary_inverse_2(.Generic, e1)
    prev_inverse <- e2$inverse
  }

  if (!all(is_dist)) {
    body <- substituteDirect(body(prev_inverse), list(x = body(current_inverse)))
    inverse <- new_function(exprs(x = ), body = body)
  }


  deriv <- suppressWarnings(try(Deriv::Deriv(inverse, x = 'x'), silent = TRUE))
  if(inherits(deriv, "try-error")) {
    message('Cannot compute the derivative of the inverse function symbolicly.')
  }

  vec_data(dist_transformed(wrap_dist(list(list(e1,e2)[[which(is_dist)[1]]][["dist"]])), trans, inverse, deriv))[[1]]
}
