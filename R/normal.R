#' @export
dist_normal <- function(mu = 0, sigma = 1){
  vec_cast(mu, double())
  vec_cast(sigma, double())
  if(any(sigma <= 0)){
    abort("Standard deviation of a normal distribution must be greater than 0")
  }
  normal <- new_rcrd(
    list(mu = mu, sigma = sigma),
    class = "dist_normal"
  )
  new_dist(normal)
}

#' @export
format.dist_normal <- function(x, digits = 2, ...){
  x <- vec_data(x)
  sprintf(
    "N(%s, %s)",
    format(x[["mu"]], digits = digits, ...),
    format(x[["sigma"]], digits = digits, ...)
  )
}
