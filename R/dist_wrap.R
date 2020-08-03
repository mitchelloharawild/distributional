#' Create a distribution from p/d/q/r style functions
#'
#' \lifecycle{experimental}
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
#' @param package The package from which the distribution is provided.
# #' @param p,d,q,r The functions used to compute the p/d/q/r
# #' (pdf/cdf/quantile/generate)
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
dist_wrap <- function(dist, ..., package = "stats"){
                      # p = NULL, d = NULL, q = NULL, r = NULL){
  vec_assert(dist, character(), 1L)
  vec_assert(package, character(), 1L)
  par <- vec_recycle_common(dist = dist, package = package, ...)
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
  fn <- paste0("d", x[["dist"]][[1]])
  env <- rlang::pkg_env(x[["package"]][[1]])
  # fn <- get(fn, env = env, mode = "function")
  par <- x[-(1:2)]
  eval_tidy(call2(fn, at, !!!par), env = env)
}

#' @export
log_density.dist_wrap <- function(x, at, ...){
  fn <- paste0("d", x[["dist"]][[1]])
  env <- rlang::pkg_env(x[["package"]][[1]])
  # fn <- get(fn, env = env, mode = "function")
  par <- x[-(1:2)]
  eval_tidy(call2(fn, at, !!!par, log = TRUE), env = env)
}

#' @export
cdf.dist_wrap <- function(x, q, ...){
  fn <- paste0("p", x[["dist"]][[1]])
  env <- rlang::pkg_env(x[["package"]][[1]])
  # fn <- get(fn, env = env, mode = "function")
  par <- x[-(1:2)]
  eval_tidy(call2(fn, q, !!!par), env = env)
}

#' @export
quantile.dist_wrap <- function(x, p, ...){
  fn <- paste0("q", x[["dist"]][[1]])
  env <- rlang::pkg_env(x[["package"]][[1]])
  # fn <- get(fn, env = env, mode = "function")
  par <- x[-(1:2)]
  eval_tidy(call2(fn, p, !!!par), env = env)
}

#' @export
generate.dist_wrap <- function(x, times, ...){
  fn <- paste0("r", x[["dist"]][[1]])
  env <- rlang::pkg_env(x[["package"]][[1]])
  # fn <- get(fn, env = env, mode = "function")
  par <- x[-(1:2)]
  eval_tidy(call2(fn, times, !!!par), env = env)
}
