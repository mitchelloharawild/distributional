#' The Negative Binomial distribution
#'
#' \lifecycle{stable}
#'
#' @inheritParams stats::NegBinomial
#'
#' @seealso [stats::NegBinomial]
#'
#' @examples
#' dist_negative_binomial(size = 10, prob = 0.5)
#'
#' @export
dist_negative_binomial <- function(size, prob){
  size <- vec_cast(size, double())
  prob <- vec_cast(prob, double())
  if(any(prob < 0 | prob > 1)){
    abort("Probability of success must be between 0 and 1.")
  }
  new_dist(size = size, prob = prob, class = "dist_negbin")
}

#' @export
print.dist_negbin <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_negbin <- function(x, digits = 2, ...){
  sprintf(
    "NB(%s, %s)",
    format(x[["size"]], digits = digits, ...),
    format(x[["prob"]], digits = digits, ...)
  )
}

#' @export
density.dist_negbin <- function(x, at, ...){
  stats::dnbinom(at, x[["size"]], x[["prob"]])
}

#' @export
quantile.dist_negbin <- function(x, p, ...){
  stats::qnbinom(p, x[["size"]], x[["prob"]])
}

#' @export
cdf.dist_negbin <- function(x, q, ...){
  stats::pnbinom(q, x[["size"]], x[["prob"]])
}

#' @export
generate.dist_negbin <- function(x, times, ...){
  stats::rnbinom(times, x[["size"]], x[["prob"]])
}

#' @export
mean.dist_negbin <- function(x, ...){
  x[["size"]] * (1 - x[["prob"]]) / x[["prob"]]
}

#' @export
variance.dist_negbin <- function(x, ...){
  x[["size"]] * (1 - x[["prob"]]) / x[["prob"]]^2
}
