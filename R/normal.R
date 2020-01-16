#' @export
dist_normal <- function(mu = 0, sigma = 1){
  vec_cast(mu, double())
  vec_cast(sigma, double())
  if(any(sigma < 0)){
    abort("Standard deviation of a normal distribution must be non-negative")
  }
  # normal <- new_rcrd(
  #   list(mu = mu, sigma = sigma),
  #   class = "dist_normal"
  # )
  new_dist(mu = mu, sigma = sigma, class = "dist_normal")
}

#' @export
print.dist_normal <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_normal <- function(x, digits = 2, ...){
  sprintf(
    "N(%s, %s)",
    format(x[["mu"]], digits = digits, ...),
    format(x[["sigma"]], digits = digits, ...)
  )
}

#' @export
quantile.dist_normal <- function(x, p){
  qnorm(p, x[["mu"]], x[["sigma"]])
}
