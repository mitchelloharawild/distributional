#' The multivariate normal distribution
#'
#' @inheritParams stats::NegBinomial
#'
#' @examples
#' dist_multivariate_normal(mu = list(c(1,2)), sigma = list(matrix(c(4,2,2,3), ncol=2)))
#'
#' @export
dist_multivariate_normal <- function(mu = 0, sigma = diag(1)){
  new_dist(mu = mu, sigma = sigma,
           dimnames = colnames(sigma[[1]]), class = "dist_mvnorm")
}

#' @export
print.dist_mvnorm <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_mvnorm <- function(x, digits = 2, ...){
  sprintf(
    "MVN[%i]",
    length(x[["mu"]])
  )
}

#' @export
density.dist_mvnorm <- function(x, at, ...){
  mvtnorm::dmvnorm(at, x[["mu"]], x[["sigma"]])
}

#' @export
quantile.dist_mvnorm <- function(x, p, type = c("univariate", "equicoordinate"), ...){
  type <- match.arg(type)
  if (type == "univariate") {
    qnorm(p, x[["mu"]], sd = diag(sqrt(x[["sigma"]])))
  } else {
    mvtnorm::qmvnorm(p, mean = x[["mu"]], sigma = x[["sigma"]])$quantile
  }
}

#' @export
cdf.dist_mvnorm <- function(x, q, ...){
  mvtnorm::pmvnorm(q, mean = x[["mu"]], sigma = x[["sigma"]])[1]
}

#' @export
generate.dist_mvnorm <- function(x, times, ...){
  mvtnorm::rmvnorm(times, x[["mu"]], x[["sigma"]])
}

#' @export
mean.dist_mvnorm <- function(x, ...){
  x[["mu"]]
}

#' @export
variance.dist_mvnorm <- function(x, ...){
  # Wrap in list to preserve matrix structure
  list(x[["sigma"]])
}

#' @export
dim.dist_mvnorm <- function(x){
  length(x[["mu"]])
}
