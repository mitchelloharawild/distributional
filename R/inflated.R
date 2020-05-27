#' Inflate a value of a probability distribution
#'
#' \lifecycle{maturing}
#'
#' @param dist The distribution(s) to inflate.
#' @param prob The added probability of observing `x`.
#' @param x The value to inflate. The default of `x = 0` is for zero-inflation.
#'
#' @name dist_inflated
#' @export
dist_inflated <- function(dist, prob, x = 0){
  vec_is(dist, new_dist())
  vec_is(x, numeric())
  vec_is(x, numeric())
  if(prob < 0 || prob > 1){
    abort("The inflation probability must be between 0 and 1.")
  }
  new_dist(dist = dist, x = x, p = prob,
           dimnames = dimnames(dist), class = "dist_inflated")
}

#' @export
format.dist_inflated <- function(x, ...){
  sprintf(
    "%i+%s",
    x[["x"]],
    format(x[["dist"]])
  )
}

#' @export
density.dist_inflated <- function(x, at, ...){
  x[["p"]]*(at==x[["x"]]) + (1-x[["p"]])*density(x[["dist"]], at, ...)
}

#' @export
quantile.dist_inflated <- function(x, p, ...){
  qt <- quantile(x[["dist"]], pmax(0, (p - x[["p"]]) / (1-x[["p"]])), ...)
  if(qt >= x[["x"]]) return(qt)
  qt <- quantile(x[["dist"]], p, ...)
  if(qt < x[["x"]]) qt else x[["x"]]
}

#' @export
cdf.dist_inflated <- function(x, q, ...){
  x[["p"]]*(q>=x[["x"]]) + (1-x[["p"]])*cdf(x[["dist"]], q, ...)
}

#' @export
generate.dist_inflated <- function(x, times, ...){
  p <- x[["p"]]
  inf <- stats::runif(times) < p
  r <- numeric(times)
  r[inf] <- x[["x"]]
  r[!inf] <- generate(x[["dist"]], sum(!inf))
  r
}

#' @export
mean.dist_inflated <- function(x, ...){
  p <- x[["p"]]
  p*x[["x"]] + (1-p)*mean(x[["dist"]])
}

#' @export
variance.dist_inflated <- function(x, ...){
  if(x[["x"]] != 0) return(NextMethod())
  m1 <- mean(x[["dist"]])
  v <- variance(x[["dist"]])
  m2 <- v + m1^2
  p <- x[["p"]]
  (1-p)*v + p*(1-p)*m1^2
}
