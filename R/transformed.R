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
#' @param d_inverse The derivative of the `inverse` function. If not provided, the
#' we will attempt to compute it symbolically, or use numerical differentiation.
#'
#' @description
#' `dist_transformed()` requires you to explicitly provide the inverse and the
#' derivative of the inverse functions. To attempt automatic symbolic computation
#' of these functions, you can create a transformed distribution by appplying the
#' transformation to an existing distribution object, e.g. `exp(dist_normal(0, 0.5))`.
#'
#'
#' @examples
#' # Create a log normal distribution - the derivative is found symbolically if possible
#' dist <- dist_transformed(dist_normal(0, 0.5), exp, log)
#' density(dist, 1) # dlnorm(1, 0, 0.5)
#' cdf(dist, 4) # plnorm(4, 0, 0.5)
#' quantile(dist, 0.1) # qlnorm(0.1, 0, 0.5)
#' generate(dist, 10) # rlnorm(10, 0, 0.5)
#'
#' # provide a derivative of the inverse function to avoid numerical differentiation
#' box_cox_transform <- function(x, lambda = 3) {
#'   if (lambda == 0) return(log(x))
#'   (x^lambda - 1) / lambda
#' }
#' box_cox_inv <- function(x, lambda = 3) {
#'  if (lambda == 0) return(exp(x))
#'  (lambda * x + 1)^(1/lambda)
#' }
#' box_cox_deriv <- function(x, lambda = 3) {
#'  if (lambda == 0) return(exp(x))
#'  (lambda * x + 1)^(1/lambda - 1)
#' }
#' dist <- dist_transformed(dist_normal(0, 0.5), box_cox_transform, box_cox_inv, box_cox_deriv)
#'
#' @export
dist_transformed <- function(dist, transform, inverse, d_inverse = NULL){
  vec_is(dist, new_dist())
  if (is.function(transform)) transform <- list(transform)
  if (is.function(inverse)) inverse <- list(inverse)
  if (is.function(d_inverse)) d_inverse <- list(d_inverse)
  if (is.null(d_inverse)) {
    d_inverse <- lapply(inverse, symbolic_derivative)
  }

  args <- vctrs::vec_recycle_common(dist = dist, transform = transform, inverse = inverse, d_inverse = d_inverse)
  args <- transpose(args)
  funs <- transpose(lapply(args, function(x) add_transform(x$dist, x)))
  dist <- lapply(args, function(x) if (inherits(x$dist, "dist_transformed")) x$dist$dist else x$dist)

  new_dist(dist = dist,
           transform = funs$transform, inverse = funs$inverse, d_inverse = funs$d_inverse,
           dimnames = dimnames(args$dist), class = "dist_transformed")
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
  lim <- suppressWarnings(eval_transform(x, lim))
  if (all(!is.na(lim))) {
    lim <- sort(lim)
  }
  # temporary fix for 1/dist_wrap('norm')
  if (identical(lim, c(0,0))) lim <- c(-Inf, Inf)
  field(support, "lim")[[1]] <- lim
  support
}

#' @export
density.dist_transformed <- function(x, at, verbose = getOption('dist.verbose', FALSE), ...) {
  on.exit(options(dist.verbose = verbose), add = T)
  inv <- eval_inverse(x, at)
  jacobian <- eval_deriv(x, at)

  d <- density(x[["dist"]], inv) * abs(jacobian)

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
  p <- cdf(x[["dist"]], eval_inverse(x, q), ...)
  limits <- field(support(x), "lim")[[1]]
  if(!monotonic_increasing(x[["transform"]], support(x[["dist"]]))) p <- 1 - p
  if (!any(is.na(limits))) {
    p[q <= limits[1]] <- 0
    p[q >= limits[2]] <- 1
  }
  p
}

#' @export
quantile.dist_transformed <- function(x, p, ...){
  if(!monotonic_increasing(x[["transform"]], support(x[["dist"]]))) p <- 1 - p
  q <- quantile(x[["dist"]], p, ...)
  eval_transform(x, q)
}

#' @export
generate.dist_transformed <- function(x, ...){
  y <- generate(x[["dist"]], ...)
  eval_transform(x, y)
}

#' @export
mean.dist_transformed <- function(x, ...){
  mu <- mean(x[["dist"]])
  sigma2 <- variance(x[["dist"]])
  trans <- function(value) eval_transform(x, value)
  if(is.na(sigma2)){
    # warning("Could not compute the transformed distribution's mean as the base distribution's variance is unknown. The transformed distribution's median has been returned instead.")
    return(trans(mu))
  }
  drop(
    trans(mu) + numDeriv::hessian(trans, mu, method.args = list(d = 0.01))/2*sigma2
  )
}

#' @export
covariance.dist_transformed <- function(x, ...){
  trans <- function(value) eval_transform(x, value)
  mu <- mean(x[["dist"]])
  sigma2 <- variance(x[["dist"]])
  if(is.na(sigma2)) return(NA_real_)
  drop(
    numDeriv::jacobian(trans, mu)^2*sigma2 + (numDeriv::hessian(trans, mu, method.args=list(d = 0.01))*sigma2)^2/2
  )
}

monotonic_increasing <- function(f, support) {
  # Currently assumes (without checking, #9) monotonicity of f over the domain
  x <- f(field(support, "lim")[[1]])
  x[[2L]] > x[[1L]]
}

# Add transform (to be incorporated in Math.dist_transformed and Ops.dist_transformed)
#' @param .x a list of functions `transform`, `inverse`, `d_inverse`
#' @param .y a list of functions `transform`, `inverse`, `d_inverse` to add to `.x`
#' @noRd
add_transform <- function(.x, .y) {
  force(.x)
  force(.y)
  if (is.null(.x$transform)) {
    list(
      transform = .y$transform,
      inverse = .y$inverse,
      d_inverse = .y$d_inverse
    )
  } else {
    list(
      transform = function(x) .y$transform(.x$transform(x)),
      inverse = function(x) .x$inverse(.y$inverse(x)),
      d_inverse = chain_rule(.x$inverse, .y$inverse, .x$d_inverse, .y$d_inverse)
    )
  }
}
