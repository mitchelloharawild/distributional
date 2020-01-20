dist_mixture <- function(..., weights = numeric()){
  dist <- dots_list(...)
  vec_is(weights, numeric(), length(dist))
  if(sum(weights) != 1){
    abort("Weights of a mixture model must sum to 1.")
  }
  if(any(weights < 0)){
    abort("All weights in a mixtue model must be non-negative.")
  }
  dist <- mapply(dist_scaled, dist = dist, scale = weights, SIMPLIFY = FALSE)
  new_dist(!!!dist, class = "dist_mixture")
}

#' @export
format.dist_mixture <- function(x, ...){
  sprintf(
    "mixture(n=%i)",
    length(x)
  )
}

#' @export
density.dist_mixture <- function(x, ...){
  sum(vapply(x, density, numeric(1L), ...))
}

#' @export
cdf.dist_mixture <- function(x, ...){
  sum(vapply(x, cdf, numeric(1L), ...))
}
