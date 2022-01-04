#' The multivariate normal distribution
#'
#' \lifecycle{maturing}
#'
#' @param mu A list of numeric vectors for the distribution's mean.
#' @param sigma A list of matrices for the distribution's variance-covariance matrix.
#'
#' @seealso [mvtnorm::dmvnorm], [mvtnorm::qmvnorm]
#'
#' @examples
#' dist <- dist_multivariate_normal(mu = list(c(1,2)), sigma = list(matrix(c(4,2,2,3), ncol=2)))
#' dist
#'
#' @examplesIf requireNamespace("mvtnorm", quietly = TRUE)
#' mean(dist)
#' variance(dist)
#' support(dist)
#' generate(dist, 10)
#'
#' density(dist, c(2, 1))
#' density(dist, c(2, 1), log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @export
dist_multivariate_normal <- function(mu = 0, sigma = diag(1)){
  new_dist(mu = mu, sigma = sigma,
           dimnames = colnames(sigma[[1]]), class = "dist_mvnorm")
}

#' @export
format.dist_mvnorm <- function(x, digits = 2, ...){
  sprintf(
    "MVN[%i]",
    length(x[["mu"]])
  )
}

#' @export
density.dist_mvnorm <- function(x, at, ..., na.rm = FALSE){
  require_package("mvtnorm")
  if(is.list(at)) return(vapply(at, density, numeric(1L), x = x, ...))
  mvtnorm::dmvnorm(at, x[["mu"]], x[["sigma"]])
}

#' @export
log_density.dist_mvnorm <- function(x, at, ..., na.rm = FALSE){
  require_package("mvtnorm")
  if(is.list(at)) return(vapply(at, log_density, numeric(1L), x = x, ...))
  mvtnorm::dmvnorm(at, x[["mu"]], x[["sigma"]], log = TRUE)
}

#' @export
quantile.dist_mvnorm <- function(x, p, type = c("univariate", "equicoordinate"),
                                 ..., na.rm = FALSE){
  type <- match.arg(type)
  if (type == "univariate") {
    matrix(
      stats::qnorm(p, mean = rep(x[["mu"]], each = length(p)),
                   sd = rep(diag(sqrt(x[["sigma"]])), each = length(p)), ...),
      nrow = length(p)
    )
  } else {
    require_package("mvtnorm")
    mvtnorm::qmvnorm(p, mean = x[["mu"]], sigma = x[["sigma"]], ...)$quantile
  }
}

#' @export
cdf.dist_mvnorm <- function(x, q, ..., na.rm = FALSE){
  if(is.list(q)) return(vapply(q, cdf, numeric(1L), x = x, ...))
  require_package("mvtnorm")
  mvtnorm::pmvnorm(as.numeric(q), mean = x[["mu"]], sigma = x[["sigma"]], ...)[1]
}

#' @export
generate.dist_mvnorm <- function(x, times, ..., na.rm = FALSE){
  require_package("mvtnorm")
  mvtnorm::rmvnorm(times, x[["mu"]], x[["sigma"]], ...)
}

#' @export
mean.dist_mvnorm <- function(x, ...){
  matrix(x[["mu"]], nrow = 1)
}

#' @export
covariance.dist_mvnorm <- function(x, ...){
  # Wrap in list to preserve matrix structure
  list(x[["sigma"]])
}

#' @export
dim.dist_mvnorm <- function(x){
  length(x[["mu"]])
}
