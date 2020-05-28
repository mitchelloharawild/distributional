#' Create a mixture of distributions
#'
#' \lifecycle{experimental}
#'
#' @param ... Distributions to be used in the mixture.
#' @param weights The weight of each distribution passed to `...`.
#'
#' @examples
#' dist_mixture(dist_normal(0, 1), dist_normal(5, 2), weights = c(0.3, 0.7))
#'
#' @export
dist_mixture <- function(..., weights = numeric()){
  dist <- dots_list(...)
  vec_is(weights, numeric(), length(dist))
  if(sum(weights) != 1){
    abort("Weights of a mixture model must sum to 1.")
  }
  if(any(weights < 0)){
    abort("All weights in a mixtue model must be non-negative.")
  }
  new_dist(dist = transpose(dist), w = list(weights), class = "dist_mixture")
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
  sum(x[["w"]]*vapply(x[["dist"]], density, numeric(1L), ...))
}

#' @export
quantile.dist_mixture <- function(x, p, ...){
  # Find bounds for optimisation based on range of each quantile
  dist_q <- vapply(x[["dist"]], quantile, numeric(1L), p, ..., USE.NAMES = FALSE)
  if(vctrs::vec_unique_count(dist_q) == 1) return(dist_q[1])

  # Search the cdf() for appropriate quantile
  stats::optimise(
    function(pos) (p - cdf(x, pos, ...))^2,
    interval = c(min(dist_q), max(dist_q))
  )$minimum
}

#' @export
cdf.dist_mixture <- function(x, ...){
  sum(x[["w"]]*vapply(x[["dist"]], cdf, numeric(1L), ...))
}

#' @export
generate.dist_mixture <- function(x, times, ...){
  dist_idx <- .bincode(stats::runif(times), breaks = c(0, cumsum(x[["w"]])))
  r <- numeric(times)
  for(i in seq_along(x[["dist"]])){
    r_pos <- dist_idx == i
    r[r_pos] <- generate(x[["dist"]][[i]], sum(r_pos), ...)
  }
  r
}

#' @export
mean.dist_mixture <- function(x, ...){
  sum(x[["w"]]*vapply(x[["dist"]], mean, numeric(1L), ...))
}

#' @export
variance.dist_mixture <- function(x, ...){
  m <- vapply(x[["dist"]], mean, numeric(1L), ...)
  v <- vapply(x[["dist"]], variance, numeric(1L), ...)
  m1 <- sum(x[["w"]]*m)
  m2 <- sum(x[["w"]]*(m^2 + v))
  m2 - m1^2
}
