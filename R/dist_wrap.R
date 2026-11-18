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

# Elements wrapping the same functions (same distribution name and environment)
# share one call, so `get()` runs once per group. Returns the positions of each
# group, and the function (prefixed by `prefix`) to call on them.
dist_wrap_groups <- function(x, prefix){
  nm <- x[["dist"]]
  env <- x[["env"]]
  key <- paste(nm, vapply(env, rlang::obj_address, character(1L)))
  lapply(split(seq_along(key), factor(key, unique(key))), function(idx){
    list(
      idx = idx,
      fn = get(paste0(prefix, nm[[idx[[1L]]]]), envir = env[[idx[[1L]]]], mode = "function")
    )
  })
}

# Call the wrapped function `prefix` + dist once per group, with the remaining
# fields (and `arg`, when it has one value per distribution) subset to the group.
# `call` is given the function, the group's first argument and its parameters.
dist_wrap_apply <- function(x, prefix, arg, call){
  n <- vec_size(x)
  par <- dist_fields_except(x, c("dist", "env"))
  per_dist <- vec_size(arg) != 1L
  out <- vector("list", n)
  for (g in dist_wrap_groups(x, prefix)) {
    idx <- g$idx
    res <- call(
      g$fn,
      if (per_dist) vec_slice(arg, idx) else arg,
      lapply(par, vec_slice, i = idx)
    )
    out[idx] <- vec_chop(vec_recycle(res, length(idx)))
  }
  vec_c(!!!out)
}

#' @export
format.dist_wrap <- function(x, ...){
  par <- dist_fields_except(x, c("dist", "env"))
  args <- if (length(par)) do.call(paste, c(unname(par), sep = ", ")) else character(vec_size(x))
  sprintf("%s(%s)", x[["dist"]], args)
}

# The data type of a wrapped distribution depends on the function it wraps.
#' @export
point_mass.dist_wrap <- point_mass_elementwise

#' @export
density.dist_wrap <- function(x, at, ...){
  dist_wrap_apply(x, "d", at, function(fn, at, par) do.call(fn, c(list(at), par)))
}

#' @export
log_density.dist_wrap <- function(x, at, ...){
  dist_wrap_apply(x, "d", at, function(fn, at, par) {
    # Use density(log = TRUE) if supported
    if(is.null(formals(fn)$log)){
      log(do.call(fn, c(list(at), par)))
    } else {
      do.call(fn, c(list(at), par, log = TRUE))
    }
  })
}

#' @export
cdf.dist_wrap <- function(x, q, ..., tail = "lower"){
  dist_wrap_apply(x, "p", q, function(fn, q, par) {
    if(tail == "lower") return(do.call(fn, c(list(q), par)))
    if("lower.tail" %in% names(formals(fn))) {
      do.call(fn, c(list(q), par, lower.tail = FALSE))
    } else {
      1 - do.call(fn, c(list(q), par))
    }
  })
}

#' @export
quantile.dist_wrap <- function(x, p, ..., tail = "lower"){
  dist_wrap_apply(x, "q", p, function(fn, p, par) {
    if(tail == "lower") return(do.call(fn, c(list(p), par)))
    if("lower.tail" %in% names(formals(fn))) {
      do.call(fn, c(list(p), par, lower.tail = FALSE))
    } else {
      do.call(fn, c(list(1 - p), par))
    }
  })
}

#' @export
generate.dist_wrap <- function(x, times, ...){
  par <- dist_fields_except(x, c("dist", "env"))
  out <- vector("list", vec_size(x))
  for (g in dist_wrap_groups(x, "r")) {
    idx <- g$idx
    t <- times[idx]
    gpar <- lapply(par, function(p) vec_rep_each(vec_slice(p, idx), t))
    out[idx] <- vec_chop(do.call(g$fn, c(list(sum(t)), gpar)), sizes = t)
  }
  out
}

#' @export
parameters.dist_wrap <- function(x, ...) {
  # All parameters except distribution environment
  dist_fields_except(x, "env")
}
