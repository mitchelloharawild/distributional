#' The Binomial distribution
#'
#' \lifecycle{stable}
#'
#' @param size The number of trials.
#' @param prob The probability of success on each trial.
#'
#' @examples
#' dist_binomial(size = 1:5, prob = c(0.05, 0.5, 0.3, 0.9, 0.1))
#'
#' @name dist_binomial
#' @export
dist_binomial <- function(size, prob){
  size <- vec_cast(size, integer())
  prob <- vec_cast(prob, double())
  if(any(size < 0)){
    abort("The number of observations cannot be negative.")
  }
  if(any((prob < 0) | (prob > 1))){
    abort("The probability of success must be between 0 and 1.")
  }
  new_dist(n = size, p = prob, class = "dist_binomial")
}

#' @export
print.dist_binomial <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_binomial <- function(x, digits = 2, ...){
  sprintf(
    "B(%s, %s)",
    format(x[["n"]], digits = digits, ...),
    format(x[["p"]], digits = digits, ...)
  )
}

#' @export
density.dist_binomial <- function(x, at, ...){
  stats::dbinom(at, x[["n"]], x[["p"]])
}

#' @export
quantile.dist_binomial <- function(x, p, ...){
  stats::qbinom(p, x[["n"]], x[["p"]])
}

#' @export
cdf.dist_binomial <- function(x, q, ...){
  stats::pbinom(q, x[["n"]], x[["p"]])
}

#' @export
generate.dist_binomial <- function(x, times, ...){
  stats::rbinom(times, x[["n"]], x[["p"]])
}

#' @export
mean.dist_binomial <- function(x, ...){
  x[["n"]]*x[["p"]]
}

#' @export
variance.dist_binomial <- function(x, ...){
  x[["n"]]*x[["p"]]*(1-x[["p"]])
}
