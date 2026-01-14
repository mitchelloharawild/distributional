#' Create a distribution from p/d/q/r style functions
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' If a distribution is not yet supported, you can vectorise p/d/q/r functions
#' using this function. `dist_wrap()` stores the distributions parameters, and
#' provides wrappers which call the appropriate p/d/q/r functions.
#'
#' Using this function to wrap a distribution should only be done if the
#' distribution is not yet available in this package. If you need a distribution
#' which isn't in the package yet, consider making a request at
#' https://github.com/mitchelloharawild/distributional/issues.
#'
#' @param dist The name of the distribution used in the functions (name that is
#' prefixed by p/d/q/r)
#' @param ... Named arguments used to parameterise the distribution.
#' @param package The package from which the distribution is provided. If NULL,
#' the calling environment's search path is used to find the distribution
#' functions. Alternatively, an arbitrary environment can also be provided here.
# #' @param p,d,q,r The functions used to compute the p/d/q/r
# #' (pdf/cdf/quantile/generate)
#'
#' @details
#'
#' The `dist_wrap()` function provides a generic interface to create distribution
#' objects from any set of p/d/q/r style functions. The statistical properties
#' depend on the specific distribution being wrapped.
#' 
#' @examples
#' dist <- dist_wrap("norm", mean = 1:3, sd = c(3, 9, 2))
#'
#' density(dist, 1) # dnorm()
#' cdf(dist, 4) # pnorm()
#' quantile(dist, 0.975) # qnorm()
#' generate(dist, 10) # rnorm()
#'
#' library(actuar)
#' dist <- dist_wrap("invparalogis", package = "actuar", shape = 2, rate = 2)
#' density(dist, 1) # actuar::dinvparalogis()
#' cdf(dist, 4) # actuar::pinvparalogis()
#' quantile(dist, 0.975) # actuar::qinvparalogis()
#' generate(dist, 10) # actuar::rinvparalogis()
#'
#' @export
dist_wrap <- function(dist, ..., package = NULL){
  vec_assert(dist, character(), 1L)
  if(is.null(package)) {
    env <- rlang::caller_env()
  } else if (is.character(package)) {
    env <- rlang::pkg_env(package)
  } else {
    env <- as.environment(package)
  }
  par <- vec_recycle_common(dist = dist, env = list(env), ...)
  new_dist(!!!par, class = "dist_wrap")
}

#' @export
format.dist_wrap <- function(x, ...){
  sprintf(
    "%s(%s)",
    x[["dist"]],
    paste0(x[-(1:2)], collapse = ", ")
  )
}

#' @export
density.dist_wrap <- function(x, at, ...){
  fn <- get(paste0("d", x[["dist"]][[1]]), envir = x$env, mode = "function")

  # Remove distribution name and environment from parameters
  par <- x[-(1:2)]

  do.call(fn, c(list(at), par))
}

#' @export
log_density.dist_wrap <- function(x, at, ...){
  fn <- get(paste0("d", x[["dist"]][[1]]), envir = x$env, mode = "function")

  # Remove distribution name and environment from parameters
  par <- x[-(1:2)]

  # Use density(log = TRUE) if supported
  if(is.null(formals(fn)$log)){
    log(do.call(fn, c(list(at), par)))
  } else {
    do.call(fn, c(list(at), par, log = TRUE))
  }
}

#' @export
cdf.dist_wrap <- function(x, q, ...){
  fn <- get(paste0("p", x[["dist"]][[1]]), envir = x$env, mode = "function")

  # Remove distribution name and environment from parameters
  par <- x[-(1:2)]

  do.call(fn, c(list(q), par))
}

#' @export
quantile.dist_wrap <- function(x, p, ...){
  fn <- get(paste0("q", x[["dist"]][[1]]), envir = x$env, mode = "function")

  # Remove distribution name and environment from parameters
  par <- x[-(1:2)]

  do.call(fn, c(list(p), par))
}

#' @export
generate.dist_wrap <- function(x, times, ...){
  fn <- get(paste0("r", x[["dist"]][[1]]), envir = x$env, mode = "function")

  # Remove distribution name and environment from parameters
  par <- x[-(1:2)]

  do.call(fn, c(list(times), par))
}

#' @export
parameters.dist_wrap <- function(x, ...) {
  # All parameters except distribution environment
  x[-2L]
}
