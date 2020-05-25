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
  infl <- dist_scaled(dist_degenerate(x), prob)
  dist <- dist_scaled(dist, 1-prob)
  new_dist(dist, infl,
           dimnames = dimnames(dist), class = c("dist_inflated", "dist_mixture"))
}

#' @export
format.dist_inflated <- function(x, ...){
  sprintf(
    "%i+%s",
    x[[2]][["dist"]][["x"]],
    format(x[[1]][["dist"]])
  )
}
