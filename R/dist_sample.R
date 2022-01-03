#' Sampling distribution
#'
#' \lifecycle{stable}
#'
#' @param x A list of sampled values.
#'
#' @examples
#' # Univariate numeric samples
#' dist <- dist_sample(x = list(rnorm(100), rnorm(100, 10)))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' generate(dist, 10)
#'
#' density(dist, 1)
#'
#' # Multivariate numeric samples
#' dist <- dist_sample(x = list(cbind(rnorm(100), rnorm(100, 10))))
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' generate(dist, 10)
#'
#' density(dist, 1)
#'
#' @export
dist_sample <- function(x){
  vec_assert(x, list())
  x <- as_list_of(x, .ptype = vec_ptype(x[[1]]))
  new_dist(x = x, class = "dist_sample")
}

#' @export
format.dist_sample <- function(x, ...){
  sprintf(
    "sample[%s]",
    vapply(x, vec_size, integer(1L))
  )
}

#' @export
density.dist_sample <- function(x, at, ..., na.rm = TRUE){
  # Apply independently over sample variates
  if(is.matrix(x$x)) {
    return(
      apply(x$x, 2,
        function(x, ...) density.dist_sample(list(x=x), ...),
        at = at, ..., na.rm = TRUE
      )
    )
  }
  # Shortcut if only one point in density is needed
  if(vec_size(at) == 1){
    return(density(x[["x"]], from = at, to = at, n = 1)$y)
  }
  d <- density(x[["x"]], from = min(at), to = max(at), ..., na.rm=na.rm)
  stats::approx(d$x, d$y, xout = at)$y
}


#' @export
quantile.dist_sample <- function(x, p, ..., na.rm = TRUE){
  # Apply independently over sample variates
  if(is.matrix(x$x)) {
    return(
      apply(x$x, 2,
            function(x, ...) quantile.dist_sample(list(x=x), ...),
            p = p, ..., na.rm = TRUE
      )
    )
  }
  quantile(x$x, probs = p, ..., na.rm = na.rm, names = FALSE)
}

#' @export
cdf.dist_sample <- function(x, q, ..., na.rm = TRUE){
  # Apply independently over sample variates
  if(is.matrix(x$x)) {
    return(
      apply(x$x, 2,
            function(x, ...) cdf.dist_sample(list(x=x), ...),
            q = q, ..., na.rm = TRUE
      )
    )
  }
  if(length(q) > 1) return(vapply(q, cdf, numeric(1L), x = x, ...))
  vapply(x, function(x, q) mean(x < q, ..., na.rm = na.rm), numeric(1L), q = q)
}

#' @export
generate.dist_sample <- function(x, times, ...){
  i <- sample.int(vec_size(x[["x"]]), size = times, replace = TRUE)
  if(is.matrix(x$x)) x$x[i,,drop = FALSE] else x$x[i]
}

#' @export
mean.dist_sample <- function(x, ...){
  if(is.matrix(x$x)) apply(x$x, 2, mean, ...) else mean(x$x, ...)
}

#' @export
median.dist_sample <- function(x, na.rm = FALSE, ...){
  if(is.matrix(x$x))
    apply(x$x, 2, median, na.rm = na.rm, ...)
  else
    median(x$x, na.rm = na.rm, ...)
}

#' @export
covariance.dist_sample <- function(x, ...){
  if(is.matrix(x$x)) stats::cov(x$x, ...) else stats::var(x$x, ...)
}

#' @export
skewness.dist_sample <- function(x, ..., na.rm = FALSE) {
  if(is.matrix(x)) {abort("Multivariate sample skewness is not yet implemented.")}
  n <- lengths(x, use.names = FALSE)
  x <- lapply(x, function(.) . - mean(., na.rm = na.rm))
  sum_x2 <- vapply(x, function(.) sum(.^2, na.rm = na.rm), numeric(1L), USE.NAMES = FALSE)
  sum_x3 <- vapply(x, function(.) sum(.^3, na.rm = na.rm), numeric(1L), USE.NAMES = FALSE)
  y <- sqrt(n) * sum_x3/(sum_x2^(3/2))
  y * ((1 - 1/n))^(3/2)
}
