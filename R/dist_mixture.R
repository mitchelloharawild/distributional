#' Create a mixture of distributions
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' A mixture distribution combines multiple component distributions with 
#' specified weights. The resulting distribution can model complex, 
#' multimodal data by representing it as a weighted sum of simpler 
#' distributions.
#'
#' @param ... Distributions to be used in the mixture. Can be any 
#'   distributional objects.
#' @param weights A numeric vector of non-negative weights that sum to 1. 
#'   The length must match the number of distributions passed to `...`. 
#'   Each weight \eqn{w_i} represents the probability that a random draw 
#'   comes from the \eqn{i}-th component distribution.
#'
#' @details
#'
#'   In the following, let \eqn{X} be a mixture random variable composed 
#'   of \eqn{K} component distributions \eqn{F_1, F_2, \ldots, F_K} with 
#'   corresponding weights \eqn{w_1, w_2, \ldots, w_K} where 
#'   \eqn{\sum_{i=1}^K w_i = 1} and \eqn{w_i \geq 0} for all \eqn{i}.
#'
#'   **Support**: The union of the supports of all component distributions
#'
#'   **Mean**: 
#'   
#'   For univariate mixtures:
#'   \deqn{
#'     E(X) = \sum_{i=1}^K w_i \mu_i
#'   }{
#'     E(X) = sum_{i=1}^K w_i * mu_i
#'   }
#'   
#'   where \eqn{\mu_i} is the mean of the \eqn{i}-th component distribution.
#'   
#'   For multivariate mixtures:
#'   \deqn{
#'     E(\mathbf{X}) = \sum_{i=1}^K w_i \boldsymbol{\mu}_i
#'   }{
#'     E(X) = sum_{i=1}^K w_i * mu_i
#'   }
#'   
#'   where \eqn{\boldsymbol{\mu}_i} is the mean vector of the \eqn{i}-th 
#'   component distribution.
#'
#'   **Variance**: 
#'   
#'   For univariate mixtures:
#'   \deqn{
#'     \text{Var}(X) = \sum_{i=1}^K w_i (\mu_i^2 + \sigma_i^2) - \left(\sum_{i=1}^K w_i \mu_i\right)^2
#'   }{
#'     Var(X) = sum_{i=1}^K w_i * (mu_i^2 + sigma_i^2) - (sum_{i=1}^K w_i * mu_i)^2
#'   }
#'   
#'   where \eqn{\sigma_i^2} is the variance of the \eqn{i}-th component 
#'   distribution.
#'
#'   **Covariance**: 
#'   
#'   For multivariate mixtures:
#'   \deqn{
#'     \text{Cov}(\mathbf{X}) = \sum_{i=1}^K w_i \left[ (\boldsymbol{\mu}_i - \bar{\boldsymbol{\mu}})(\boldsymbol{\mu}_i - \bar{\boldsymbol{\mu}})^T + \boldsymbol{\Sigma}_i \right]
#'   }{
#'     Cov(X) = sum_{i=1}^K w_i * [ (mu_i - mu_bar)(mu_i - mu_bar)^T + Sigma_i ]
#'   }
#'   
#'   where \eqn{\bar{\boldsymbol{\mu}} = \sum_{i=1}^K w_i \boldsymbol{\mu}_i} 
#'   is the overall mean vector and \eqn{\boldsymbol{\Sigma}_i} is the 
#'   covariance matrix of the \eqn{i}-th component distribution.
#'
#'   **Probability density/mass function (p.d.f/p.m.f)**:
#'
#'   \deqn{
#'     f(x) = \sum_{i=1}^K w_i f_i(x)
#'   }{
#'     f(x) = sum_{i=1}^K w_i * f_i(x)
#'   }
#'   
#'   where \eqn{f_i(x)} is the density or mass function of the \eqn{i}-th 
#'   component distribution.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   For univariate mixtures:
#'   \deqn{
#'     F(x) = \sum_{i=1}^K w_i F_i(x)
#'   }{
#'     F(x) = sum_{i=1}^K w_i * F_i(x)
#'   }
#'   
#'   where \eqn{F_i(x)} is the c.d.f. of the \eqn{i}-th component 
#'   distribution.
#'   
#'   For multivariate mixtures, the c.d.f. is approximated numerically.
#'
#'   **Quantile function**:
#'   
#'   For univariate mixtures, the quantile function has no closed form 
#'   and is computed numerically by inverting the c.d.f. using root-finding 
#'   ([stats::uniroot()]).
#'   
#'   For multivariate mixtures, quantiles are not yet implemented.
#'
#' @seealso [stats::uniroot()], [vctrs::vec_unique_count()]
#'
#' @examples
#' # Univariate mixture of two normal distributions
#' dist <- dist_mixture(dist_normal(0, 1), dist_normal(5, 2), weights = c(0.3, 0.7))
#' dist
#' 
#' mean(dist)
#' variance(dist)
#' 
#' density(dist, 2)
#' cdf(dist, 2)
#' quantile(dist, 0.5)
#' 
#' generate(dist, 10)
#'
#' @name dist_mixture
#' @export
dist_mixture <- function(..., weights = numeric()){
  dist <- dots_list(...)
  dn <- unique(lapply(dist, dimnames))
  dn <- if(length(dn) == 1) dn[[1]] else NULL

  vec_is(weights, numeric(), length(dist))

  if(!abs(sum(weights)-1) < sqrt(.Machine$double.eps)){
    abort("Weights of a mixture model must sum to 1.")
  }
  if(any(weights < 0)){
    abort("All weights in a mixtue model must be non-negative.")
  }
  new_dist(dist = transpose(dist), w = list(weights),
           class = "dist_mixture", dimnames = dn)
}

#' @export
format.dist_mixture <- function(x, width = getOption("width"), ...){
  dists <- unlist(lapply(x[["dist"]], format))

  dist_info <- paste0(x[["w"]], "*", dists, collapse = ", ")

  long_dist <- paste0("mixture(", dist_info, ")")
  short_dist <- paste0("mixture(n=", length(dists), ")")
  ifelse(nchar(long_dist) <= width, long_dist, short_dist)
}

#' @export
density.dist_mixture <- function(x, at, ...){
  if(NROW(at) > 1) return(vapply(at, density, numeric(1L), x = x, ...))
  sum(x[["w"]]*vapply(x[["dist"]], density, numeric(1L), at = at, ...))
}

#' @export
quantile.dist_mixture <- function(x, p, ...){
  d <- dim(x)
  if(d > 1)
    stop("quantile is not implemented for multivariate mixtures.")
  if(length(p) > 1) return(vapply(p, quantile, numeric(1L), x = x, ...))

  # Find bounds for optimisation based on range of each quantile
  dist_q <- vapply(x[["dist"]], quantile, numeric(1L), p, ..., USE.NAMES = FALSE)
  if(vctrs::vec_unique_count(dist_q) == 1) return(dist_q[1])
  if(p == 0) return(min(dist_q))
  if(p == 1) return(max(dist_q))

  # Search the cdf() for appropriate quantile
  stats::uniroot(
    function(pos) p - cdf(x, pos, ...),
    interval = c(min(dist_q), max(dist_q)),
    extendInt = "yes"
  )$root
}

#' @export
cdf.dist_mixture <- function(x, q, times = 1e5, ...){
  d <- dim(x)
  if(d == 1L) {
    if(length(q) > 1) return(vapply(q, cdf, numeric(1L), x = x, ...))
    sum(x[["w"]]*vapply(x[["dist"]], cdf, numeric(1L), q = q, ...))
  } else {
    NextMethod()
  }
}

#' @export
generate.dist_mixture <- function(x, times, ...){
  dist_idx <- .bincode(stats::runif(times), breaks = c(0, cumsum(x[["w"]])))
  r <- matrix(nrow = times, ncol = dim(x))
  for(i in seq_along(x[["dist"]])){
    r_pos <- dist_idx == i
    if(any(r_pos)) {
      r[r_pos,] <- generate(x[["dist"]][[i]], sum(r_pos), ...)
    }
  }
  r[,seq(NCOL(r)), drop = TRUE]
}

#' @export
mean.dist_mixture <- function(x, ...){
  d <- dim(x)
  m <- vapply(x[["dist"]], mean, numeric(d), ...)
  if(d == 1L) {
    sum(x[["w"]] * m)
  } else {
    matrix(x[["w"]], ncol = d, nrow = 1) %*% t(m)
  }
}

#' @export
covariance.dist_mixture <- function(x, ...){
  d <- dim(x)
  if(d == 1L) {
    m <- vapply(x[["dist"]], mean, numeric(1L), ...)
    v <- vapply(x[["dist"]], variance, numeric(1L), ...)
    m1 <- sum(x[["w"]]*m)
    m2 <- sum(x[["w"]]*(m^2 + v))
    m2 - m1^2
  } else {
    m <- lapply(x[["dist"]], mean)
    w <- as.list(x[["w"]])
    mbar <- mapply("*", m, w, SIMPLIFY = FALSE)
    mbar <- do.call("+", mbar)
    m <- lapply(m, function(u){u - mbar})
    v <- lapply(x[["dist"]], function(u){covariance(u)[[1]]})
    cov <- mapply(function(m,v,w) {w * ( t(m) %*% m + v ) }, m, v, w, SIMPLIFY = FALSE)
    list(do.call("+", cov))
  }
}

#' @export
dim.dist_mixture <- function(x){
  dim(x[["dist"]][[1]])
}
