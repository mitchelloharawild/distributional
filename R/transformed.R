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
  vec_is(dist, new_dist())
  if (is.function(transform)) transform <- list(wrap_primitive(transform))
  if (is.function(inverse)) inverse <- list(wrap_primitive(inverse))
  if (is.null(deriv)) {
    deriv <- lapply(inverse, function(inv){
      suppressWarnings(try(Deriv::Deriv(inv, x = 'x'), silent = TRUE))
    })
  } else if (is.function(deriv)) {
    deriv <- list(wrap_primitive(deriv))
  } else if (is.list(deriv)) {
    deriv <- lapply(deriv, wrap_primitive)
  }

  deriv <- lapply(deriv, function(d) if(is.function(d)) d else NULL)

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
density.dist_transformed <- function(x, at, deriv_method = "symbolic",
                                     verbose = getOption('dist.verbose', FALSE), ...) {
  deriv_method <- match.arg(deriv_method, c("symbolic", "numeric"))
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

