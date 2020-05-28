#' The Normal distribution
#'
#' \lifecycle{stable}
#'
#' @param mu The mean (location parameter) of the distribution.
#' @param sigma The standard deviation (scale parameter) of the distribution.
#'
#' @seealso [stats::Normal]
#'
#' @examples
#' dist_normal(mu = 1:5, sigma = 3)
#'
#' @export
dist_normal <- function(mu = 0, sigma = 1){
  mu <- vec_cast(mu, double())
  sigma <- vec_cast(sigma, double())
  if(any(sigma[!is.na(sigma)] < 0)){
    abort("Standard deviation of a normal distribution must be non-negative")
  }
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
    format(x[["sigma"]]^2, digits = digits, ...)
  )
}

#' @export
density.dist_normal <- function(x, at, ...){
  stats::dnorm(at, x[["mu"]], x[["sigma"]])
}

#' @export
quantile.dist_normal <- function(x, p, ...){
  stats::qnorm(p, x[["mu"]], x[["sigma"]])
}

#' @export
cdf.dist_normal <- function(x, q, ...){
  stats::pnorm(q, x[["mu"]], x[["sigma"]])
}

#' @export
generate.dist_normal <- function(x, times, ...){
  stats::rnorm(times, x[["mu"]], x[["sigma"]])
}

#' @export
mean.dist_normal <- function(x, ...){
  x[["mu"]]
}

#' @export
variance.dist_normal <- function(x, ...){
  x[["sigma"]]^2
}

#' @export
Ops.dist_normal <- function(e1, e2){
  ok <- switch(.Generic, `+` = , `-` = , `*` = , `/` = TRUE, FALSE)
  if (!ok) {
    return(NextMethod())
  }
  if(.Generic == "/" && inherits(e2, "dist_normal")){
    abort(sprintf("Cannot divide by a normal distribution"))
  }
  if(.Generic %in% c("-", "+") && missing(e2)){
    e2 <- e1
    e1 <- if(.Generic == "+") 1 else -1
    .Generic <- "*"
  }
  if(.Generic == "-"){
    .Generic <- "+"
    e2 <- -e2
  }
  else if(.Generic == "/"){
    .Generic <- "*"
    e2 <- 1/e2
  }

  # Ops between two normals
  if(inherits(e1, "dist_normal") && inherits(e2, "dist_normal")){
    if(.Generic == "*"){
      abort(sprintf("Multiplying two normal distributions is not supported."))
    }

    e1$mu <- e1$mu + e2$mu
    e1$sigma <- sqrt(e1$sigma^2 + e2$sigma^2)
    return(e1)
  }

  # Ops between a normal and scalar
  if(inherits(e1, "dist_normal")){
    dist <- e1
    scalar <- e2
  } else {
    dist <- e2
    scalar <- e1
  }
  if(!is.numeric(scalar)){
    abort(sprintf("Cannot %s a `%s` with a normal distribution",
                  switch(.Generic, `+` = "add", `-` = "subtract", `*` = "multiply", `/` = "divide"),
                  class(scalar)))
  }

  if(.Generic == "+"){
    dist$mu <- dist$mu + scalar
  }
  else if(.Generic == "*"){
    dist$mu <- dist$mu * scalar
    dist$sigma <- dist$sigma * abs(scalar)
  }
  dist
}
