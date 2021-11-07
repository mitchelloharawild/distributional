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
format.dist_sample <- function(x, ...){
  sprintf(
    "sample[%s]",
    vapply(x, length, integer(1L))
  )
}

#' @export
density.dist_sample <- function(x, at, ...){
  # Shortcut if only one point in density is needed
  if(vec_size(at) == 1){
    return(density(x[["x"]], from = at, to = at, n = 1)$y)
  }
  d <- density(x[["x"]], from = min(at), to = max(at))
  stats::approx(d$x, d$y, xout = at)$y
}


#' @export
quantile.dist_sample <- function(x, p, ..., na.rm = TRUE){
  quantile(x$x, probs = p, ..., na.rm = na.rm, names = FALSE)
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
median.dist_sample <- function(x, na.rm = FALSE, ...){
  vapply(x, median, numeric(1L), na.rm = na.rm, ..., USE.NAMES = FALSE)
}

#' @export
covariance.dist_sample <- function(x, ...){
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
