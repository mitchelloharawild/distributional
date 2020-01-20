dist_scaled <- function(dist, scale){
  vec_is(dist, new_dist())
  new_dist(dist = dist, scale = scale, class = "dist_scaled")
}

#' @export
format.dist_scaled <- function(x, ...){
  format(x[["dist"]], ...)
}

#' @export
density.dist_scaled <- function(x, ...){
  x[["scale"]]*density(x[["dist"]], ...)
}

# #' @export
# quantile.dist_scaled <- function(x, ...){
#   x[["scale"]]*quantile(x[["dist"]], ...)
# }

#' @export
cdf.dist_scaled <- function(x, ...){
  x[["scale"]]*cdf(x[["dist"]], ...)
}

#' @export
generate.dist_scaled <- function(x, ...){
  x[["scale"]]*generate(x[["dist"]], ...)
}
