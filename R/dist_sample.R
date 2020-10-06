#' Sampling distribution
#'
#' \lifecycle{stable}
#'
#' @param x A list of sampled values.
#'
#' @examples
#' dist_sample(x = list(rnorm(100), rnorm(100, 10)))
#'
#' @export
dist_sample <- function(x){
  has_NA <- FALSE
  x <- lapply(x, function(.) if(any(na_pos <- is.na(.))){
    has_NA<<-TRUE
    .[!na_pos]
  } else .)
  if(has_NA){
    warn("Missing sampled values have been removed from the sample distribution.")
  }
  x <- as_list_of(x, .ptype = double())
  new_dist(x = x, class = "dist_sample")
}

#' @export
print.dist_sample <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_sample <- function(x, ...){
  sprintf(
    "sample[%s]",
    vapply(x, length, integer(1L))
  )
}

#' @export
density.dist_sample <- function(x, at, ...){
  d <- density(x[["x"]], from = min(at), to = max(at))
  stats::approx(d$x, d$y, xout = at)$y
}


#' @export
quantile.dist_sample <- function(x, p, ..., na.rm = TRUE){
  if(length(p) > 1) return(vapply(p, quantile, numeric(1L), x = x, ...))
  vapply(x, quantile, numeric(1L), probs = p, ..., na.rm = na.rm, USE.NAMES = FALSE)
}

#' @export
cdf.dist_sample <- function(x, q, ...){
  if(length(q) > 1) return(vapply(q, cdf, numeric(1L), x = x, ...))
  vapply(x, function(x, q){
    mean(x < q)
  }, numeric(1L), q = q)
}

#' @export
generate.dist_sample <- function(x, times, ...){
  sample(x[["x"]], size = times, replace = TRUE)
}

#' @export
mean.dist_sample <- function(x, ...){
  vapply(x, mean, numeric(1L), ..., USE.NAMES = FALSE)
}

#' @export
variance.dist_sample <- function(x, ...){
  vapply(x, stats::var, numeric(1L), ..., USE.NAMES = FALSE)
}

#' @export
skewness.dist_sample <- function(x, ...) {
  n <- lengths(x, use.names = FALSE)
  x <- lapply(x, function(.) . - mean(.))
  sum_x2 <- vapply(x, function(.) sum(.^2), numeric(1L), USE.NAMES = FALSE)
  sum_x3 <- vapply(x, function(.) sum(.^3), numeric(1L), USE.NAMES = FALSE)
  y <- sqrt(n) * sum_x3/(sum_x2^(3/2))
  y * ((1 - 1/n))^(3/2)
}
