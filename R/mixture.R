#' Create a mixture of distributions
#'
#' @description
#' `r lifecycle::badge('maturing')`
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
  dn <- unique(lapply(dist, dimnames))
  dn <- if(length(dn) == 1) dn[[1]] else NULL

  vec_is(weights, numeric(), length(dist))
  if(sum(weights) != 1){
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
  dists <- lapply(x[["dist"]], format) |>
    unlist()

  dist_info <- paste0(x[["w"]], "*", dists) |>
    paste0(collapse = ", ")

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
