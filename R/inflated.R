dist_inflated <- function(dist, p, x = 0){
  vec_is(dist, new_dist())
  vec_is(x, numeric())
  vec_is(x, numeric())
  if(p < 0 || p > 1){
    abort("The inflation probability must be between 0 and 1.")
  }
  infl <- dist_scaled(dist_degenerate(x), p)
  dist <- dist_scaled(dist, 1-p)
  new_dist(dist, infl, class = c("dist_inflated", "dist_mixture"))
}

#' @export
format.dist_inflated <- function(x, ...){
  sprintf(
    "%i+%s",
    format(x[["dist"]]),
    x[["x"]]
  )
}
